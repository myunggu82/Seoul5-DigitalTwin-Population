# scripts/99_run_pipeline.R
# Seoul5-ABSP: End-to-end reproducible pipeline (inputs -> process -> outputs -> validation)
# Run: Rscript scripts/99_run_pipeline.R

source(here("scripts", "00_config.R"))
source(here("R", "checks.R"))
source(here("R", "helpers.R"))
source(here("R", "io.R"))
source(here("R", "validation.R"))

# =========================================================
# 1) Read + schema check
# =========================================================
inp <- read_inputs(paths, loc_kr)
validate_input_schema(inp)

jumin       <- inp$jumin
lp          <- inp$lp
purpose     <- inp$purpose
mig         <- inp$mig
hh_type2    <- inp$hh_type2
hn_final    <- inp$hn_final
hh_gen_raw  <- inp$hh_gen_raw

# =========================================================
# 2) Dong lookup (LP-based)
# =========================================================
dong_lookup <- lp %>%
  distinct(시군구명, 읍면동명, 행정기관코드10) %>%
  transmute(
    gu        = 시군구명,
    dong      = str_squish(읍면동명),
    dong_code = pad10(행정기관코드10)
  )

# =========================================================
# 3) Core synthetic population (registered -> individuals)
# =========================================================
pop_reg <- jumin %>%
  mutate(지역 = str_squish(지역)) %>%
  left_join(dong_lookup, by = c("지역" = "dong")) %>%
  filter(
    gu %in% target_gu,
    !is.na(dong_code),
    시기 == base_month,
    성별 != "계"
  ) %>%
  mutate(n = as.integer(인구수)) %>%
  filter(n > 0)

core_pop <- pop_reg %>%
  tidyr::uncount(n, .remove = FALSE, .id = "ind_in_cell") %>%
  group_by(gu, 지역) %>%
  mutate(ind_seq = row_number()) %>%
  ungroup() %>%
  transmute(
    gu,
    dong         = str_squish(지역),
    dong_code,
    ind_id       = paste0(dong_code, "_", ind_seq),
    sex_reg      = recode(성별, "남"="남자","여"="여자", .default=성별),
    age_reg_band = 연령,
    age_lp_band  = str_replace_all(recode_age_to_lp(연령), " ", ""),
    age_mig_band = recode_age_to_mig(연령),
    month        = 시기
  )

assert_unique_key(core_pop, c("ind_id"))

# =========================================================
# 4) Time-slot distributions (Living population) -> time_slot_distributions
# =========================================================
time_slot_map <- tibble::tribble(
  ~시간대, ~time_slot,
  "심야(00-06)", "midnight_00_06",
  "아침(06-09)", "morning_06_09",
  "오전(09-12)", "midday_09_12",
  "오후(12-19)", "afternoon_12_19",
  "저녁(19-21)", "evening_19_21",
  "밤(21-24)",   "late_evening_21_24"
)

lp_time <- lp %>%
  filter(시군구명 %in% target_gu) %>%
  mutate(
    dong_code = pad10(행정기관코드10),
    성별 = recode(성별, "남"="남자","여"="여자", .default=성별),
    연령 = str_replace_all(연령, " ", "")
  ) %>%
  left_join(time_slot_map, by = "시간대") %>%
  filter(!is.na(time_slot)) %>%
  transmute(
    dong_code,
    sex_reg     = 성별,
    age_lp_band = 연령,
    day_type    = if_else(요일구분=="평일","weekday","weekend"),
    time_slot,
    lp_n        = 생활인구수
  )

time_slot_distributions <- lp_time %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type, time_slot) %>%
  summarise(lp = sum(lp_n, na.rm=TRUE), .groups="drop") %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type) %>%
  mutate(p_time = if_else(sum(lp)>0, lp/sum(lp), 0)) %>%
  ungroup() %>%
  select(dong_code, sex_reg, age_lp_band, day_type, time_slot, p_time)

qc_time <- check_sum_to_one(
  time_slot_distributions,
  c("dong_code","sex_reg","age_lp_band","day_type"),
  "p_time"
)

# =========================================================
# 5) Purpose distributions (Living population) -> purpose_distributions
# =========================================================
purpose_slot_map <- tibble::tribble(
  ~시간대, ~time_slot,
  "심야", "midnight_00_06",
  "아침", "morning_06_09",
  "오전", "midday_09_12",
  "오후", "afternoon_12_19",
  "저녁", "evening_19_21",
  "밤",   "late_evening_21_24"
)

purpose_distributions <- purpose %>%
  filter(시군구명 %in% target_gu) %>%
  mutate(
    dong_code   = pad10(행정기관코드),
    sex_reg     = recode(성별, "남"="남자","여"="여자", .default=성별),
    age_lp_band = str_replace_all(연령, " ", "")
  ) %>%
  left_join(purpose_slot_map, by="시간대") %>%
  filter(!is.na(time_slot)) %>%
  transmute(
    dong_code, sex_reg, age_lp_band, time_slot,
    purpose_cat = 목적_eng,
    lp_n = 생활인구수
  ) %>%
  group_by(dong_code, sex_reg, age_lp_band, time_slot, purpose_cat) %>%
  summarise(lp_purpose = sum(lp_n, na.rm=TRUE), .groups="drop") %>%
  group_by(dong_code, sex_reg, age_lp_band, time_slot) %>%
  mutate(p_purpose = if_else(sum(lp_purpose)>0, lp_purpose/sum(lp_purpose), 0)) %>%
  ungroup() %>%
  select(dong_code, sex_reg, age_lp_band, time_slot, purpose_cat, p_purpose)

qc_purpose <- check_sum_to_one(
  purpose_distributions,
  c("dong_code","sex_reg","age_lp_band","time_slot"),
  "p_purpose"
)

# =========================================================
# 6) Mobility summary (origin-dong based) -> mobility_summary
# =========================================================
mobility_summary <- mig %>%
  filter(출발시군구명.x %in% target_gu) %>%
  group_by(출발시군구명.x, 출발행정동명.x, 연령대) %>%
  summarise(
    total_move = sum(이동인구수, na.rm = TRUE),
    mean_dist  = weighted.mean(평균이동거리, w = 이동인구수, na.rm = TRUE),
    mean_time  = weighted.mean(평균이동시간, w = 이동인구수, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  rename(
    gu           = 출발시군구명.x,
    dong         = 출발행정동명.x,
    age_mig_band = 연령대
  ) %>%
  mutate(dong = str_squish(dong)) %>%
  left_join(
    dong_lookup %>% mutate(dong = str_squish(dong)),
    by = c("gu" = "gu", "dong" = "dong")
  ) %>%
  filter(!is.na(dong_code)) %>%
  transmute(
    dong_code,
    age_mig_band,
    total_move,
    mean_dist,
    mean_time
  ) %>%
  arrange(dong_code, age_mig_band)

# =========================================================
# 7) Household attribute distributions
#    7-1) Household-head age x generation (Excel) -> hh_head_age_dist + hh_gen_multi_dist
# =========================================================
hh_gen_clean <- hh_gen_raw %>%
  mutate(
    지역   = str_squish(`행정구역별(시군구)`),
    성별   = `가구주의 성별`,
    연령대 = `가구주의 연령별`
  ) %>%
  filter(
    !is.na(지역),
    지역 %in% target_gu,
    성별 == "계",
    연령대 != "합계"
  ) %>%
  mutate(
    n_1gen   = readr::parse_number(dplyr::na_if(`2024.1`, "X")),
    n_2gen   = readr::parse_number(dplyr::na_if(`2024.2`, "X")),
    n_3gen   = readr::parse_number(dplyr::na_if(`2024.3`, "X")),
    n_1p     = readr::parse_number(dplyr::na_if(`2024.4`, "X")),
    n_nonkin = readr::parse_number(dplyr::na_if(`2024.5`, "X"))
  )

hh_head_age_dist <- hh_gen_clean %>%
  select(지역, head_age_band = 연령대, n_1gen, n_2gen, n_3gen, n_1p, n_nonkin) %>%
  pivot_longer(cols = starts_with("n_"), names_to = "gen_cat_raw", values_to = "n_head") %>%
  mutate(
    gen_cat = case_when(
      gen_cat_raw == "n_1p"      ~ "size_1",
      gen_cat_raw == "n_nonkin"  ~ "nonkin",
      gen_cat_raw == "n_1gen"    ~ "gen1",
      gen_cat_raw == "n_2gen"    ~ "gen2",
      gen_cat_raw == "n_3gen"    ~ "gen3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gen_cat), !is.na(n_head)) %>%
  group_by(지역, gen_cat) %>%
  mutate(
    n_total    = sum(n_head, na.rm = TRUE),
    p_head_age = if_else(n_total > 0, n_head / n_total, 0)
  ) %>%
  ungroup() %>%
  transmute(
    gu = 지역,
    gen_cat,
    head_age_band,
    p_head_age
  ) %>%
  arrange(gu, gen_cat, head_age_band)

hh_gen_multi_dist <- hh_gen_clean %>%
  select(지역, n_1gen, n_2gen, n_3gen) %>%
  pivot_longer(cols = starts_with("n_"), names_to = "gen_cat_raw", values_to = "n_head") %>%
  mutate(
    gen_cat = case_when(
      gen_cat_raw == "n_1gen" ~ "gen1",
      gen_cat_raw == "n_2gen" ~ "gen2",
      gen_cat_raw == "n_3gen" ~ "gen3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(gen_cat), !is.na(n_head)) %>%
  group_by(지역, gen_cat) %>%
  summarise(n_cat = sum(n_head, na.rm = TRUE), .groups = "drop") %>%
  group_by(지역) %>%
  mutate(
    total_cat = sum(n_cat, na.rm = TRUE),
    p_gen_cat = if_else(total_cat > 0, n_cat / total_cat, 0)
  ) %>%
  ungroup() %>%
  rename(gu = 지역)

# =========================================================
# 7-2) Household size distribution (dong-level) -> hh_size_dist
# =========================================================
hh_size_value_lut <- tibble::tribble(
  ~hh_size_cat, ~hh_size_val,
  "size_1",     1L,
  "size_2",     2L,
  "size_3",     3L,
  "size_4",     4L,
  "size_5plus", 5L
)

hh_size_dist <- hn_final %>%
  filter(연도 == 2024) %>%
  mutate(지역 = str_squish(지역)) %>%
  left_join(dong_lookup %>% mutate(dong = str_squish(dong)), by = c("지역" = "dong")) %>%
  filter(
    gu %in% target_gu,
    !is.na(dong_code)
  ) %>%
  mutate(val_num = parse_number(as.character(값))) %>%
  filter(!is.na(val_num)) %>%
  mutate(
    hh_size_cat = case_when(
      str_detect(유형, "1인가구") ~ "size_1",
      str_detect(유형, "2인가구") ~ "size_2",
      str_detect(유형, "3인가구") ~ "size_3",
      str_detect(유형, "4인가구") ~ "size_4",
      str_detect(유형, "5인가구") ~ "size_5plus",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(hh_size_cat)) %>%
  group_by(dong_code, hh_size_cat) %>%
  summarise(hh_n = sum(val_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(dong_code) %>%
  mutate(
    hh_n_total = sum(hh_n, na.rm = TRUE),
    p_hh_size  = if_else(hh_n_total > 0, hh_n / hh_n_total, 0)
  ) %>%
  ungroup() %>%
  left_join(hh_size_value_lut, by="hh_size_cat") %>%
  arrange(dong_code, hh_size_cat)

qc_hh_size <- check_sum_to_one(hh_size_dist, c("dong_code"), "p_hh_size")

# =========================================================
# 7-3) Household "type/nationality/collective" distribution (gu-level) -> hh_type_dist
# =========================================================
hh_type_dist <- hh_type2 %>%
  filter(
    연도 == 2024,
    연령 == "전체",
    지역 %in% target_gu
  ) %>%
  mutate(
    nat_cat = case_when(
      유형 == "일반가구_가구원_계(명)"          ~ "general_household",
      유형 == "외국인가구_가구원_계(명)"        ~ "foreign_household",
      유형 == "집단가구_비친족원_6인이하(명)"   ~ "collective_nonkin_le6",
      유형 == "집단가구_비친족원_7인이상(명)"   ~ "collective_nonkin_ge7",
      유형 == "집단가구_시설이용(명)"          ~ "collective_institution",
      TRUE                                     ~ NA_character_
    ),
    val_num = parse_number(as.character(값))
  ) %>%
  filter(!is.na(nat_cat), !is.na(val_num)) %>%
  group_by(지역, nat_cat) %>%
  summarise(n_person = sum(val_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(지역) %>%
  mutate(
    n_total   = sum(n_person, na.rm = TRUE),
    p_hh_type = if_else(n_total > 0, n_person / n_total, 0)
  ) %>%
  ungroup() %>%
  transmute(
    gu = 지역,
    nat_cat,
    p_hh_type
  ) %>%
  arrange(gu, nat_cat)

qc_hh_type <- check_sum_to_one(hh_type_dist, c("gu"), "p_hh_type")

# =========================================================
# 8) Household file generation (hh_file)
#    - Determine number of households per dong using avg household size
#    - Sample hh size (dong-level), hh type (gu-level), generation type (gu-level), head age (gu x gen)
# =========================================================
dong_person_n <- core_pop %>%
  count(dong_code, name = "n_person")

dong_hh_param <- dong_person_n %>%
  left_join(
    hh_size_dist %>%
      group_by(dong_code) %>%
      summarise(avg_hh_size = sum(p_hh_size * hh_size_val, na.rm = TRUE), .groups = "drop"),
    by = "dong_code"
  ) %>%
  mutate(
    avg_hh_size = if_else(is.na(avg_hh_size) | avg_hh_size <= 0, 2, avg_hh_size),
    n_hh        = ceiling(n_person / avg_hh_size)
  )

dong_meta <- dong_lookup %>%
  distinct(dong_code, gu, dong)

set.seed(SEED)

hh_file <- dong_hh_param %>%
  split(.$dong_code) %>%
  imap_dfr(function(df_dong, d_code) {
    n_hh_i <- df_dong$n_hh[1]
    if (is.na(n_hh_i) || n_hh_i <= 0) return(tibble())
    
    size_tab <- hh_size_dist %>% filter(dong_code == d_code)
    if (nrow(size_tab) == 0) return(tibble())
    
    hh_sizes <- sample(
      size_tab$hh_size_cat, size = n_hh_i, replace = TRUE, prob = size_tab$p_hh_size
    )
    
    dong_info <- dong_meta %>% filter(dong_code == d_code) %>% slice(1)
    gu_i <- dong_info$gu[1]
    
    nat_tab <- hh_type_dist %>% filter(gu == gu_i)
    nat_cat_smpl <- if (nrow(nat_tab) == 0) rep(NA_character_, n_hh_i) else
      sample(nat_tab$nat_cat, size = n_hh_i, replace = TRUE, prob = nat_tab$p_hh_type)
    
    # generation category assignment (gen_cat)
    gen_cat_vec <- rep(NA_character_, n_hh_i)
    
    idx_nonkin <- nat_cat_smpl %in% c("collective_nonkin_le6","collective_nonkin_ge7","collective_institution")
    gen_cat_vec[idx_nonkin] <- "nonkin"
    
    idx_size1 <- nat_cat_smpl == "general_household" & hh_sizes == "size_1"
    gen_cat_vec[idx_size1] <- "size_1"
    
    idx_multi <- nat_cat_smpl %in% c("general_household","foreign_household") & hh_sizes != "size_1" & is.na(gen_cat_vec)
    if (any(idx_multi)) {
      w_multi <- hh_gen_multi_dist %>% filter(gu == gu_i)
      if (nrow(w_multi) == 0 || all(is.na(w_multi$p_gen_cat))) {
        gen_cat_vec[idx_multi] <- "gen2"
      } else {
        gen_cat_vec[idx_multi] <- sample(
          w_multi$gen_cat, size = sum(idx_multi), replace = TRUE, prob = w_multi$p_gen_cat
        )
      }
    }
    
    gen_cat_vec[is.na(gen_cat_vec)] <- "gen2"
    
    # head age band by (gu x gen_cat)
    hh_head_age <- map_chr(gen_cat_vec, function(gcat) {
      w <- hh_head_age_dist %>% filter(gu == gu_i, gen_cat == gcat)
      if (nrow(w) == 0 || all(is.na(w$p_head_age))) return(NA_character_)
      sample(w$head_age_band, size = 1, replace = TRUE, prob = w$p_head_age)
    })
    
    tibble(
      hh_id         = paste0(d_code, "_HH", seq_len(n_hh_i)),
      dong_code     = d_code,
      gu            = gu_i,
      dong          = dong_info$dong[1],
      hh_size_cat   = hh_sizes,
      nat_cat       = nat_cat_smpl,
      gen_cat       = gen_cat_vec,
      head_age_band = hh_head_age
    ) %>%
      left_join(hh_size_value_lut, by = "hh_size_cat")
  })

assert_unique_key(hh_file, c("hh_id"))

# =========================================================
# 9) Person file generation (person_file)
#    - Link core individuals to households within dong
#    - Slot-based age-structure matching using head_age_band and gen_cat
# =========================================================
hh_for_assign <- hh_file %>%
  mutate(head_age_mid = get_age_mid(head_age_band)) %>%
  select(hh_id, dong_code, hh_size_cat, hh_size_val, nat_cat, head_age_band, head_age_mid, gen_cat)

set.seed(SEED)

core_pop_shuffled <- core_pop %>%
  mutate(age_mid = get_age_mid(age_reg_band)) %>%
  arrange(dong_code, desc(age_mid), runif(n()))

person_with_hh <- core_pop_shuffled %>%
  group_by(dong_code) %>%
  group_modify(function(df_person, key) {
    d_code <- key$dong_code[1]
    hh_tab <- hh_for_assign %>% filter(dong_code == d_code)
    
    if (nrow(hh_tab) == 0) {
      df_person$hh_id <- NA_character_
      df_person$gen_cat <- NA_character_
      df_person$nat_cat <- NA_character_
      return(df_person)
    }
    
    # create household slots with expected ages
    hh_slots <- hh_tab %>%
      arrange(desc(head_age_mid)) %>%
      purrr::pmap_dfr(function(hh_id, dong_code, hh_size_cat, hh_size_val,
                               nat_cat, head_age_band, head_age_mid, gen_cat) {
        
        slot_idx <- seq_len(hh_size_val)
        
        gap <- case_when(
          gen_cat == "gen1"   ~ 10,
          gen_cat == "gen2"   ~ 20,
          gen_cat == "gen3"   ~ 25,
          gen_cat == "size_1" ~ 0,
          gen_cat == "nonkin" ~ 15,
          TRUE ~ 15
        )
        
        slot_age_mid <- pmax(head_age_mid - (slot_idx - 1) * gap, 0)
        
        tibble(
          hh_id        = hh_id,
          dong_code    = dong_code,
          head_age_mid = head_age_mid,
          slot_age_mid = slot_age_mid,
          gen_cat      = gen_cat,
          nat_cat      = nat_cat
        )
      })
    
    n_person <- nrow(df_person)
    n_slot   <- nrow(hh_slots)
    
    # expand slots if needed
    if (n_slot < n_person) {
      extra_ids <- sample(hh_tab$hh_id, size = n_person - n_slot, replace = TRUE)
      extra_slots <- tibble(hh_id = extra_ids) %>%
        left_join(hh_tab %>% select(hh_id, head_age_mid, gen_cat, nat_cat), by = "hh_id") %>%
        mutate(slot_age_mid = pmax(head_age_mid - 10, 0))
      slot_df <- bind_rows(hh_slots, extra_slots)
    } else {
      slot_df <- hh_slots
    }
    
    slot_df <- slot_df %>%
      arrange(desc(slot_age_mid)) %>%
      slice(1:n_person)
    
    df_person$hh_id   <- slot_df$hh_id
    df_person$gen_cat <- slot_df$gen_cat
    df_person$nat_cat <- slot_df$nat_cat
    df_person
  }) %>%
  ungroup()

person_file <- person_with_hh %>%
  left_join(
    hh_for_assign %>% select(hh_id, hh_size_cat, head_age_band),
    by = "hh_id"
  ) %>%
  transmute(
    ind_id, hh_id, dong_code, gu, dong,
    sex_reg, age_reg_band, age_lp_band, age_mig_band, month,
    hh_size_cat, nat_cat, gen_cat, head_age_band
  )

assert_unique_key(person_file, c("ind_id"))

# =========================================================
# 10) Timetable panel generation (timetable_panel)
#     - Expand person_file by day_type
#     - Assign time_slot using time_slot_distributions
#     - Assign purpose using purpose_distributions
# =========================================================
set.seed(SEED)

person_basic <- person_file %>%
  select(ind_id, dong_code, sex_reg, age_lp_band, hh_id, gen_cat, head_age_band)

panel_base <- person_basic %>%
  tidyr::crossing(day_type = c("weekday", "weekend"))

all_slots <- c(
  "midnight_00_06", "morning_06_09", "midday_09_12",
  "afternoon_12_19","evening_19_21","late_evening_21_24"
)

panel_with_time <- panel_base %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type) %>%
  group_modify(function(df_person, key) {
    d_code <- key$dong_code[1]
    sex_i  <- key$sex_reg[1]
    age_i  <- key$age_lp_band[1]
    day_i  <- key$day_type[1]
    n_person <- nrow(df_person)
    
    w <- time_slot_distributions %>%
      filter(dong_code==d_code, sex_reg==sex_i, age_lp_band==age_i, day_type==day_i)
    
    if (nrow(w) == 0 || all(is.na(w$p_time)) || sum(w$p_time, na.rm=TRUE) == 0) {
      df_person$time_slot <- sample(all_slots, size = n_person, replace = TRUE)
      return(df_person)
    }
    
    w2 <- w %>% filter(!is.na(p_time))
    probs <- w2$p_time
    probs <- probs / sum(probs)
    
    df_person$time_slot <- sample(w2$time_slot, size = n_person, replace = TRUE, prob = probs)
    df_person
  }) %>%
  ungroup()

panel_with_purpose <- panel_with_time %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type, time_slot) %>%
  group_modify(function(df_person, key) {
    d_code <- key$dong_code[1]
    sex_i  <- key$sex_reg[1]
    age_i  <- key$age_lp_band[1]
    slot_i <- key$time_slot[1]
    n_person <- nrow(df_person)
    
    w <- purpose_distributions %>%
      filter(dong_code==d_code, sex_reg==sex_i, age_lp_band==age_i, time_slot==slot_i)
    
    if (nrow(w) == 0 || all(is.na(w$p_purpose)) || sum(w$p_purpose, na.rm=TRUE) == 0) {
      df_person$purpose_cat <- rep("etc", n_person)
      return(df_person)
    }
    
    w2 <- w %>% filter(!is.na(p_purpose))
    probs <- w2$p_purpose
    probs <- probs / sum(probs)
    
    df_person$purpose_cat <- sample(w2$purpose_cat, size = n_person, replace = TRUE, prob = probs)
    df_person
  }) %>%
  ungroup()

timetable_panel <- panel_with_purpose

# ---- Validation --------------------------------------------------------
source("code/R/validation.R")

val <- run_validations(
  timetable_panel   = timetable_panel,
  time_slot_weights = time_slot_weights,
  person_file       = person_file
)

print(val$temporal_metrics)
# Optional: preview
print(head(val$qq_data))
print(head(val$spatial_error))
print(head(val$household_age_gaps))


# =========================================================
# 11) Validation (panel-level, distributional, spatial, household age gaps)
# =========================================================

# 11-1) Panel vs. observed time probabilities
panel_agg <- timetable_panel %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type, time_slot) %>%
  summarise(n_panel = n(), .groups = "drop") %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type) %>%
  mutate(
    n_panel_total = sum(n_panel, na.rm = TRUE),
    p_time_panel  = if_else(n_panel_total > 0, n_panel / n_panel_total, NA_real_)
  ) %>%
  ungroup()

panel_valid <- panel_agg %>%
  left_join(time_slot_distributions, by = c("dong_code","sex_reg","age_lp_band","day_type","time_slot"))

validation_overall <- panel_metrics(panel_valid)

# 11-2) Scatter plot (Observed vs Synthetic)
p_scatter <- ggplot(panel_valid, aes(x = p_time, y = p_time_panel)) +
  geom_point(alpha = 0.05) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Observed vs. Synthetic Presence Probabilities",
    subtitle = paste0("r=", round(validation_overall$cor, 3),
                      " | MAE=", signif(validation_overall$mae, 3),
                      " | RMSE=", signif(validation_overall$rmse, 3)),
    x = "Observed probability (Living population)",
    y = "Synthetic probability (Timetable panel)"
  ) +
  theme_minimal()

# 11-3) Q-Q plot (quantiles)
probs <- seq(0, 1, 0.01)
qq_data <- tibble(
  observed_q  = quantile(panel_valid$p_time,       probs, na.rm = TRUE),
  synthetic_q = quantile(panel_valid$p_time_panel, probs, na.rm = TRUE)
)

p_qq <- ggplot(qq_data, aes(x = observed_q, y = synthetic_q)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(
    title = "Q–Q Plot: Distributional Consistency",
    x = "Observed quantiles",
    y = "Synthetic quantiles"
  ) +
  coord_fixed() +
  theme_minimal()

# 11-4) Household age-gap validation (head proxy = head_age_band midpoint)
age_gap_df <- person_file %>%
  filter(!is.na(hh_id), !is.na(head_age_band)) %>%
  mutate(
    head_age_val = get_age_mid(head_age_band),
    ind_age_val  = get_age_mid(age_reg_band),
    age_gap      = head_age_val - ind_age_val
  ) %>%
  filter(!is.na(age_gap), age_gap != 0)

p_agegap <- ggplot(age_gap_df, aes(x = age_gap)) +
  geom_histogram(binwidth = 5, alpha = 0.8, color = "white") +
  geom_vline(xintercept = 20, linetype = "dashed") +
  facet_wrap(~gen_cat, scales = "free_y") +
  labs(
    title = "Intra-household Age Gap Validation",
    subtitle = "Head age (proxy) minus member age, by generation category",
    x = "Age difference (years)",
    y = "Count"
  ) +
  theme_minimal()

# 11-5) Spatial MAE / Cor (dong-level)
spatial_valid <- panel_valid %>%
  mutate(dong_code = pad10(dong_code)) %>%
  group_by(dong_code) %>%
  summarise(
    mae = mean(abs(p_time_panel - p_time), na.rm = TRUE),
    cor = cor(p_time_panel, p_time, use = "complete.obs"),
    .groups = "drop"
  )

# Optional: join with shp (if available) and map MAE
have_shp <- file.exists(paths$data$shp_rds)
p_map_mae <- NULL
if (have_shp) {
  shp_joined <- readRDS(paths$data$shp_rds)
  if (!inherits(shp_joined, "sf")) shp_joined <- sf::st_as_sf(shp_joined)
  
  # Expect shp_joined has either dong_code or dong name. Prefer dong_code if present.
  if ("dong_code" %in% names(shp_joined)) {
    shp_map <- shp_joined %>%
      mutate(dong_code = pad10(dong_code)) %>%
      left_join(spatial_valid, by = "dong_code")
  } else {
    # fallback (name join) – requires a matching name column
    # Change these names if your shp uses different column names.
    if (!("dong_name" %in% names(shp_joined))) {
      warning("shp_joined.rds has no dong_code nor dong_name. Skipping spatial map.")
      shp_map <- NULL
    } else {
      dong_info_lookup <- person_file %>%
        mutate(dong_code = pad10(dong_code)) %>%
        group_by(dong_code) %>%
        summarise(dong_name = first(dong), .groups = "drop")
      
      spatial_labeled <- spatial_valid %>%
        left_join(dong_info_lookup, by = "dong_code") %>%
        mutate(dong_name = str_trim(dong_name))
      
      shp_map <- shp_joined %>%
        mutate(dong_name = str_trim(dong_name)) %>%
        left_join(spatial_labeled, by = "dong_name")
    }
  }
  
  if (!is.null(shp_map)) {
    p_map_mae <- ggplot(shp_map) +
      geom_sf(aes(fill = mae), color = "white", linewidth = 0.05) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90", name = "MAE") +
      labs(
        title = "Spatial Distribution of Generation Error",
        subtitle = "Dong-level MAE of presence probability (observed vs synthetic)"
      ) +
      theme_void() +
      theme(legend.position = "bottom")
  }
}

# =========================================================
# 12) Save outputs (data + tables + figures + logs)
# =========================================================
# Data
write_csv(core_pop,                 file.path(paths$out$data,   "core_synthetic_population.csv"))
write_csv(hh_file,                  file.path(paths$out$data,   "household_file.csv"))
write_csv(person_file,              file.path(paths$out$data,   "person_file.csv"))
write_csv(timetable_panel,          file.path(paths$out$data,   "timetable_panel.csv"))
write_csv(time_slot_distributions,  file.path(paths$out$data,   "time_slot_distributions.csv"))
write_csv(purpose_distributions,    file.path(paths$out$data,   "purpose_distributions.csv"))
write_csv(mobility_summary,         file.path(paths$out$data,   "mobility_summary.csv"))
write_csv(hh_size_dist,             file.path(paths$out$data,   "household_size_distribution.csv"))
write_csv(hh_type_dist,             file.path(paths$out$data,   "household_type_distribution.csv"))
write_csv(hh_head_age_dist,         file.path(paths$out$data,   "head_age_distribution_by_generation.csv"))
write_csv(hh_gen_multi_dist,        file.path(paths$out$data,   "generation_distribution_multi_person.csv"))

# QC tables
write_csv(qc_time,      file.path(paths$out$tables, "qc_time_slot_sum_to_one.csv"))
write_csv(qc_purpose,   file.path(paths$out$tables, "qc_purpose_sum_to_one.csv"))
write_csv(qc_hh_size,   file.path(paths$out$tables, "qc_household_size_sum_to_one.csv"))
write_csv(qc_hh_type,   file.path(paths$out$tables, "qc_household_type_sum_to_one.csv"))

# Validation tables
write_csv(validation_overall, file.path(paths$out$tables, "validation_overall_panel_time.csv"))
write_csv(spatial_valid,      file.path(paths$out$tables, "validation_spatial_dong_level.csv"))

# Data dictionaries
save_data_dictionary(core_pop,                file.path(paths$out$tables, "dict_core_synthetic_population.csv"))
save_data_dictionary(hh_file,                 file.path(paths$out$tables, "dict_household_file.csv"))
save_data_dictionary(person_file,             file.path(paths$out$tables, "dict_person_file.csv"))
save_data_dictionary(timetable_panel,         file.path(paths$out$tables, "dict_timetable_panel.csv"))
save_data_dictionary(time_slot_distributions, file.path(paths$out$tables, "dict_time_slot_distributions.csv"))
save_data_dictionary(purpose_distributions,   file.path(paths$out$tables, "dict_purpose_distributions.csv"))

# Figures (use stable width/height for paper)
ggsave(file.path(paths$out$figs, "validation_scatter_timeprob.png"), p_scatter, width = 6.5, height = 5.0, dpi = 300)
ggsave(file.path(paths$out$figs, "validation_qq_timeprob.png"),      p_qq,      width = 6.5, height = 5.0, dpi = 300)
ggsave(file.path(paths$out$figs, "validation_age_gap.png"),          p_agegap,  width = 9.0, height = 5.5, dpi = 300)

if (!is.null(p_map_mae)) {
  ggsave(file.path(paths$out$figs, "validation_spatial_mae.png"), p_map_mae, width = 7.5, height = 6.0, dpi = 300)
}

# Logs
save_session_log(paths, SEED)

message("✅ Pipeline completed.")
message("Outputs saved under: ", here("outputs"))
