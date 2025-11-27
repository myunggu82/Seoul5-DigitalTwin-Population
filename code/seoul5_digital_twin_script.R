rm(list = ls())

# =====================================================================
# 0. Libraries & basic settings
# =====================================================================

.libPaths("C:/Users/myung/Desktop/R")

library(tidyverse)
library(lubridate)
library(stringr)
library(readr)
library(purrr)

loc_kr <- readr::locale(encoding = "CP949")

target_gu <- c("금천구", "구로구", "관악구", "영등포구", "동작구")

set.seed(2025)

# =====================================================================
# 1. Load data
# =====================================================================

file_jumin    <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/ind_pop_final_jumin.csv"
file_lp       <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/lp_final.csv"
file_lp_long_for  <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/lp_final_long_for.csv"  # (not used in current script)
file_purpose  <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/purpose_final_kor.csv"
file_mig      <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/lp_in_out_korean.csv"
file_hh_type2 <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/hh_type_2.csv"
file_hn_final <- "C:/Users/myung/Desktop/research/DTP/Seoul/data/registered/clean/hn_final.csv"

jumin    <- readr::read_csv(file_jumin,    locale = loc_kr)
lp       <- readr::read_csv(file_lp,       locale = loc_kr)
# lp_long <- readr::read_csv(file_lp_long_for, locale = loc_kr)  # use if needed
purpose  <- readr::read_csv(file_purpose,  locale = loc_kr)
mig      <- readr::read_csv(file_mig,      locale = loc_kr)
hh_type2 <- readr::read_csv(file_hh_type2, locale = loc_kr)
hn_final <- readr::read_csv(file_hn_final, locale = loc_kr)

# =====================================================================
# 2. Dong–gu mapping (based on living-population data)
# =====================================================================

dong_lookup <- lp %>%
  distinct(
    시군구명,
    읍면동명,
    행정기관코드10
  ) %>%
  rename(
    지역        = 읍면동명,
    행정기관코드 = 행정기관코드10,
    구          = 시군구명
  )

# =====================================================================
# 3. Core synthetic population based on registered population
# =====================================================================

base_month <- "2024.06"

pop_reg <- jumin %>%
  left_join(dong_lookup, by = "지역") %>%
  filter(
    구 %in% target_gu,
    !is.na(행정기관코드),
    시기 == base_month,
    성별 != "계"
  ) %>%
  mutate(
    n = as.integer(인구수)
  ) %>%
  filter(n > 0)

# Expand to individual level
pop_ind <- pop_reg %>%
  tidyr::uncount(n, .remove = FALSE, .id = "ind_in_cell") %>%
  group_by(구, 지역) %>%
  mutate(ind_seq = row_number()) %>%
  ungroup() %>%
  transmute(
    gu           = 구,
    dong         = 지역,
    dong_code    = 행정기관코드,
    ind_id       = paste0(행정기관코드, "_", ind_seq),
    sex_reg      = 성별,
    age_reg_band = 연령,
    month        = 시기
  )

# =====================================================================
# 4. Age-band harmonization (registered → LP, migration)
# =====================================================================

recode_age_to_lp <- function(age_band_reg) {
  age_band_reg <- stringr::str_replace_all(age_band_reg, " ", "")
  dplyr::case_when(
    age_band_reg %in% c("0-4세", "5-9세")                  ~ "0-9세",
    age_band_reg %in% c("10-14세")                        ~ "10-14세",
    age_band_reg %in% c("15-19세")                        ~ "15-19세",
    age_band_reg %in% c("20-24세", "25-29세")             ~ "20-29세",
    age_band_reg %in% c("30-34세", "35-39세")             ~ "30-39세",
    age_band_reg %in% c("40-44세", "45-49세")             ~ "40-49세",
    age_band_reg %in% c("50-54세", "55-59세")             ~ "50-59세",
    age_band_reg %in% c("60-64세", "65-69세", "70세이상") ~ "60세이상",
    TRUE ~ NA_character_
  )
}

recode_age_to_mig <- function(age_band_reg) {
  age_num <- stringr::str_extract(age_band_reg, "^[0-9]+") %>% as.numeric()
  dplyr::case_when(
    is.na(age_num) ~ NA_character_,
    age_num < 20   ~ "00-19세",
    age_num < 40   ~ "20-39세",
    age_num < 60   ~ "40-59세",
    TRUE           ~ "60세+"
  )
}

core_pop <- pop_ind %>%
  mutate(
    age_lp_band  = recode_age_to_lp(age_reg_band),
    age_lp_band  = stringr::str_replace_all(age_lp_band, " ", ""),
    age_mig_band = recode_age_to_mig(age_reg_band),
    sex_reg      = dplyr::recode(
      sex_reg,
      "남" = "남자",
      "여" = "여자",
      .default = sex_reg
    )
  )

# =====================================================================
# 5. Living population: time-slot distributions (weekday / weekend)
#    → time_slot_weights table
# =====================================================================

time_slots_eng <- c(
  "midnight_00_06",
  "morning_06_09",
  "midday_09_12",
  "afternoon_12_19",
  "evening_19_21",
  "late_evening_21_24"
)

lp_time <- lp %>%
  filter(시군구명 %in% target_gu) %>%
  mutate(
    성별 = dplyr::recode(
      성별,
      "남" = "남자",
      "여" = "여자",
      .default = 성별
    ),
    연령 = stringr::str_replace_all(연령, " ", ""),
    time_slot = dplyr::case_when(
      시간대 == "심야(00-06)" ~ "midnight_00_06",
      시간대 == "아침(06-09)" ~ "morning_06_09",
      시간대 == "오전(09-12)" ~ "midday_09_12",
      시간대 == "오후(12-19)" ~ "afternoon_12_19",
      시간대 == "저녁(19-21)" ~ "evening_19_21",
      시간대 == "밤(21-24)"   ~ "late_evening_21_24",
      TRUE                    ~ NA_character_
    )
  )

## 5-1. Weekdays
lp_weekday <- lp_time %>%
  filter(요일구분 == "평일") %>%
  group_by(행정기관코드10, 성별, 연령, time_slot) %>%
  summarise(
    lp = sum(생활인구수, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(행정기관코드10, 성별, 연령) %>%
  mutate(
    lp_total = sum(lp, na.rm = TRUE),
    p_time   = if_else(lp_total > 0, lp / lp_total, 0)
  ) %>%
  ungroup() %>%
  rename(
    dong_code      = 행정기관코드10,
    sex_reg_lp     = 성별,
    age_lp_band_lp = 연령
  ) %>%
  transmute(
    dong_code,
    sex_reg    = sex_reg_lp,
    age_lp_band= age_lp_band_lp,
    day_type   = "weekday",
    time_slot,
    p_time
  )

## 5-2. Weekends
lp_weekend <- lp_time %>%
  filter(요일구분 == "주말") %>%
  group_by(행정기관코드10, 성별, 연령, time_slot) %>%
  summarise(
    lp = sum(생활인구수, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(행정기관코드10, 성별, 연령) %>%
  mutate(
    lp_total = sum(lp, na.rm = TRUE),
    p_time   = if_else(lp_total > 0, lp / lp_total, 0)
  ) %>%
  ungroup() %>%
  rename(
    dong_code      = 행정기관코드10,
    sex_reg_lp     = 성별,
    age_lp_band_lp = 연령
  ) %>%
  transmute(
    dong_code,
    sex_reg    = sex_reg_lp,
    age_lp_band= age_lp_band_lp,
    day_type   = "weekend",
    time_slot,
    p_time
  )

## 5-3. Combined time_slot_weights
time_slot_weights <- bind_rows(lp_weekday, lp_weekend) %>%
  arrange(dong_code, sex_reg, age_lp_band, day_type, time_slot)

# =====================================================================
# 6. Purpose distributions by time-slot → purpose_weights
# =====================================================================

purpose_for_join <- purpose %>%
  filter(시군구명 %in% target_gu) %>%
  mutate(
    성별 = dplyr::recode(
      성별,
      "남" = "남자",
      "여" = "여자",
      .default = 성별
    ),
    연령 = stringr::str_replace_all(연령, " ", ""),
    time_slot_eng = dplyr::case_when(
      시간대 == "심야" ~ "midnight_00_06",
      시간대 == "아침" ~ "morning_06_09",
      시간대 == "오전" ~ "midday_09_12",
      시간대 == "오후" ~ "afternoon_12_19",
      시간대 == "저녁" ~ "evening_19_21",
      시간대 == "밤"   ~ "late_evening_21_24",
      TRUE             ~ NA_character_
    )
  ) %>%
  rename(
    gu          = 시군구명,
    dong        = 행정동명,
    dong_code   = 행정기관코드,
    sex_reg     = 성별,
    age_lp_band = 연령,
    purpose_cat = 목적_eng
  ) %>%
  group_by(dong_code, sex_reg, age_lp_band, time_slot_eng, purpose_cat) %>%
  summarise(
    lp_purpose = sum(생활인구수, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  group_by(dong_code, sex_reg, age_lp_band, time_slot_eng) %>%
  mutate(
    total_lp_purpose = sum(lp_purpose, na.rm = TRUE),
    p_purpose        = if_else(total_lp_purpose > 0, lp_purpose / total_lp_purpose, 0)
  ) %>%
  ungroup()

purpose_weights <- purpose_for_join %>%
  transmute(
    dong_code,
    sex_reg,
    age_lp_band,
    time_slot = time_slot_eng,
    purpose_cat,
    p_purpose
  ) %>%
  arrange(dong_code, sex_reg, age_lp_band, time_slot, purpose_cat)

# =====================================================================
# 7. Mobility summary (by origin dong) → mobility_summary
# =====================================================================

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
  left_join(
    dong_lookup,
    by = c("gu" = "구", "dong" = "지역")
  ) %>%
  filter(!is.na(행정기관코드)) %>%
  transmute(
    dong_code = 행정기관코드,
    age_mig_band,
    total_move,
    mean_dist,
    mean_time
  ) %>%
  arrange(dong_code, age_mig_band)

# =====================================================================
# 8. Household characteristics: size & type distributions
#    → hh_size_weights, hh_type_weights
# =====================================================================

## 8-1) Dong-level household size distribution
hh_size_weights <- hn_final %>%
  filter(연도 == 2024) %>%
  mutate(지역 = str_squish(지역)) %>%
  left_join(
    dong_lookup %>% mutate(지역 = str_squish(지역)),
    by = "지역"
  ) %>%
  filter(
    구 %in% target_gu,
    !is.na(행정기관코드)
  ) %>%
  mutate(
    값_num = parse_number(as.character(값))
  ) %>%
  filter(!is.na(값_num)) %>%
  mutate(
    hh_size_cat = case_when(
      str_detect(유형, "1인가구") ~ "size_1",
      str_detect(유형, "2인가구") ~ "size_2",
      str_detect(유형, "3인가구") ~ "size_3",
      str_detect(유형, "4인가구") ~ "size_4",
      str_detect(유형, "5인가구") ~ "size_5plus",
      TRUE                        ~ NA_character_
    )
  ) %>%
  filter(!is.na(hh_size_cat)) %>%
  group_by(행정기관코드, hh_size_cat) %>%
  summarise(
    hh_n = sum(값_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(행정기관코드) %>%
  mutate(
    hh_n_total = sum(hh_n, na.rm = TRUE),
    p_hh_size  = if_else(hh_n_total > 0, hh_n / hh_n_total, 0)
  ) %>%
  ungroup() %>%
  transmute(
    dong_code = 행정기관코드,
    hh_size_cat,
    p_hh_size
  ) %>%
  arrange(dong_code, hh_size_cat)

## 8-2) District-level household "type" distribution
hh_type_weights <- hh_type2 %>%
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
    )
  ) %>%
  filter(!is.na(nat_cat)) %>%
  mutate(
    값_num = readr::parse_number(as.character(값))
  ) %>%
  group_by(지역, nat_cat) %>%
  summarise(
    n_person = sum(값_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(지역) %>%
  mutate(
    n_total  = sum(n_person, na.rm = TRUE),
    p_hh_type = if_else(n_total > 0, n_person / n_total, 0)
  ) %>%
  ungroup() %>%
  transmute(
    gu = 지역,
    nat_cat,
    p_hh_type
  ) %>%
  arrange(gu, nat_cat)

# =====================================================================
# 9. Household sampling: create hh_file
# =====================================================================

# 9-1. Map hh_size_cat → integer household size
hh_size_value_lut <- tibble::tribble(
  ~hh_size_cat, ~hh_size_val,
  "size_1",     1L,
  "size_2",     2L,
  "size_3",     3L,
  "size_4",     4L,
  "size_5plus", 5L   # can be adjusted (e.g., 5.5) if needed
)

hh_size_weights2 <- hh_size_weights %>%
  left_join(hh_size_value_lut, by = "hh_size_cat")

# 9-2. Number of persons & average household size per dong → required number of households n_hh
dong_person_n <- core_pop %>%
  count(dong_code, name = "n_person")

dong_hh_param <- dong_person_n %>%
  left_join(
    hh_size_weights2 %>%
      group_by(dong_code) %>%
      summarise(
        avg_hh_size = sum(p_hh_size * hh_size_val, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "dong_code"
  ) %>%
  mutate(
    avg_hh_size = if_else(is.na(avg_hh_size) | avg_hh_size <= 0, 2, avg_hh_size),
    n_hh        = ceiling(n_person / avg_hh_size)
  )

# 9-3. For each dong, sample n_hh households
dong_meta <- dong_lookup %>%
  distinct(행정기관코드, 구, 지역) %>%
  rename(
    dong_code = 행정기관코드,
    gu        = 구,
    dong      = 지역
  )

set.seed(2025)

hh_file <- dong_hh_param %>%
  split(.$dong_code) %>%
  imap_dfr(function(df_dong, d_code) {
    n_hh_i <- df_dong$n_hh[1]
    
    size_tab <- hh_size_weights2 %>%
      filter(dong_code == d_code)
    
    if (nrow(size_tab) == 0 || n_hh_i <= 0) return(tibble())
    
    hh_sizes <- sample(
      size_tab$hh_size_cat,
      size = n_hh_i,
      replace = TRUE,
      prob    = size_tab$p_hh_size
    )
    
    dong_info <- dong_meta %>%
      filter(dong_code == d_code) %>%
      slice(1)
    
    nat_tab <- hh_type_weights %>%
      filter(gu == dong_info$gu[1])
    
    if (nrow(nat_tab) == 0) {
      nat_cat_smpl <- rep(NA_character_, n_hh_i)
    } else {
      nat_cat_smpl <- sample(
        nat_tab$nat_cat,
        size = n_hh_i,
        replace = TRUE,
        prob    = nat_tab$p_hh_type
      )
    }
    
    tibble(
      hh_id       = paste0(d_code, "_HH", seq_len(n_hh_i)),
      dong_code   = d_code,
      gu          = dong_info$gu[1],
      dong        = dong_info$dong[1],
      hh_size_cat = hh_sizes,
      nat_cat     = nat_cat_smpl
    ) %>%
      left_join(hh_size_value_lut, by = "hh_size_cat")
  })

glimpse(hh_file)

# =====================================================================
# 10. Person file: link core_pop + hh_id
# =====================================================================

set.seed(2025)

core_pop_shuffled <- core_pop %>%
  arrange(dong_code, runif(n()))

hh_for_assign <- hh_file %>%
  select(hh_id, dong_code, hh_size_cat, hh_size_val, nat_cat)

person_with_hh <- core_pop_shuffled %>%
  group_by(dong_code) %>%
  group_modify(function(df_person, key) {
    d_code <- key$dong_code[1]
    
    hh_tab <- hh_for_assign %>%
      filter(dong_code == d_code)
    
    if (nrow(hh_tab) == 0) {
      df_person$hh_id <- NA_character_
      return(df_person)
    }
    
    prob_hh <- hh_tab$hh_size_val / sum(hh_tab$hh_size_val)
    
    hh_id_assigned <- sample(
      hh_tab$hh_id,
      size    = nrow(df_person),
      replace = TRUE,
      prob    = prob_hh
    )
    
    df_person$hh_id <- hh_id_assigned
    df_person
  }) %>%
  ungroup()

person_file <- person_with_hh %>%
  left_join(
    hh_for_assign %>% select(hh_id, hh_size_cat, nat_cat),
    by = "hh_id"
  ) %>%
  transmute(
    ind_id,
    hh_id,
    dong_code,
    gu,
    dong,
    sex_reg,
    age_reg_band,
    age_lp_band,
    age_mig_band,
    month,
    hh_size_cat,
    nat_cat
  )

glimpse(person_file)
table(person_file$age_lp_band)

# =====================================================================
# 11. Timetable sampling: weekday / weekend panel
# =====================================================================

set.seed(2025)

person_basic <- person_file %>%
  select(ind_id, dong_code, sex_reg, age_lp_band)

# 1) Create two rows per person (weekday / weekend)
panel_base <- person_basic %>%
  tidyr::crossing(day_type = c("weekday", "weekend"))

# 2) Create unique key combinations
uniq_keys <- panel_base %>%
  distinct(dong_code, sex_reg, age_lp_band, day_type)

# 3) For each combination, sample time_slot
all_slots <- c(
  "midnight_00_06",
  "morning_06_09",
  "midday_09_12",
  "afternoon_12_19",
  "evening_19_21",
  "night_21_24"
)

time_sample <- uniq_keys %>%
  left_join(time_slot_weights,
            by = c("dong_code", "sex_reg", "age_lp_band", "day_type")) %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type) %>%
  summarise(
    time_slot = {
      df <- cur_data()
      
      # If no candidate or only NA → uniform sampling fallback
      if (nrow(df) == 0 || all(is.na(df$p_time))) {
        sample(all_slots, size = 1)
      } else {
        df2 <- df %>% filter(!is.na(p_time))
        probs <- df2$p_time
        
        # If probability sum is 0 → uniform fallback
        if (sum(probs) == 0) {
          sample(all_slots, size = 1)
        } else {
          sample(df2$time_slot, size = 1, prob = probs)
        }
      }
    },
    .groups = "drop"
  )

# 4) Sample purpose for each time-slot
purpose_sample <- time_sample %>%
  left_join(purpose_weights,
            by = c("dong_code", "sex_reg", "age_lp_band", "time_slot")) %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type, time_slot) %>%
  summarise(
    purpose_cat = {
      df <- cur_data()
      
      # If no purpose data at all → fallback to "etc"
      if (nrow(df) == 0 || all(is.na(df$p_purpose))) {
        "etc"
      } else {
        df2 <- df %>% filter(!is.na(p_purpose))
        probs <- df2$p_purpose
        
        if (sum(probs) == 0) {
          "etc"
        } else {
          sample(df2$purpose_cat, size = 1, prob = probs)
        }
      }
    },
    .groups = "drop"
  )

# 5) Join back to person-level panel
timetable_panel <- panel_base %>%
  left_join(purpose_sample,
            by = c("dong_code", "sex_reg", "age_lp_band", "day_type"))

glimpse(timetable_panel)

# =====================================================================
# 12. Panel-level technical validation
# =====================================================================

# 12-1. Compute proportions by dong × sex × age × day_type × time_slot
panel_agg <- timetable_panel %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type, time_slot) %>%
  summarise(
    n_panel = n(),
    .groups = "drop"
  ) %>%
  group_by(dong_code, sex_reg, age_lp_band, day_type) %>%
  mutate(
    n_panel_total = sum(n_panel, na.rm = TRUE),
    p_time_panel  = if_else(n_panel_total > 0, n_panel / n_panel_total, NA_real_)
  ) %>%
  ungroup()

# 12-2. Compare panel distribution vs. time_slot_weights
panel_valid <- panel_agg %>%
  left_join(
    time_slot_weights,
    by = c("dong_code", "sex_reg", "age_lp_band", "day_type", "time_slot")
  )

panel_valid %>%
  summarise(
    cor_panel = cor(p_time_panel, p_time, use = "complete.obs"),
    mae_panel = mean(abs(p_time_panel - p_time), na.rm = TRUE),
    rmse_panel= sqrt(mean((p_time_panel - p_time)^2, na.rm = TRUE))
  )

# =====================================================================
# 13. First-stage validation summary (core_pop, time_slot_weights, etc.)
# =====================================================================

# Registered population consistency check
reg_check_src <- jumin %>%
  left_join(dong_lookup, by = "지역") %>%
  filter(
    구 %in% target_gu,
    시기 == base_month,
    성별 != "계",
    !is.na(행정기관코드)
  ) %>%
  mutate(n_src = as.integer(인구수)) %>%
  transmute(
    dong_code   = 행정기관코드,
    sex_reg     = dplyr::recode(성별, "남" = "남자", "여" = "여자"),
    age_reg_band= 연령,
    n_src
  )

reg_check_core <- core_pop %>%
  count(dong_code, sex_reg, age_reg_band, name = "n_core")

reg_diff <- reg_check_src %>%
  left_join(reg_check_core,
            by = c("dong_code", "sex_reg", "age_reg_band")) %>%
  mutate(diff = n_core - n_src)

summary(reg_diff$diff)
max(abs(reg_diff$diff), na.rm = TRUE)

# True time-slot distribution from original lp
lp_prop_true <- lp_time %>%
  group_by(행정기관코드10, 성별, 연령, 요일구분, time_slot) %>%
  summarise(lp = sum(생활인구수, na.rm = TRUE), .groups = "drop") %>%
  group_by(행정기관코드10, 성별, 연령, 요일구분) %>%
  mutate(
    lp_total   = sum(lp, na.rm = TRUE),
    p_time_true= if_else(lp_total > 0, lp / lp_total, 0)
  ) %>%
  ungroup() %>%
  transmute(
    dong_code   = 행정기관코드10,
    sex_reg     = 성별,
    age_lp_band = 연령,
    day_type    = if_else(요일구분 == "평일", "weekday", "weekend"),
    time_slot,
    p_time_true
  )

time_valid <- time_slot_weights %>%
  left_join(lp_prop_true,
            by = c("dong_code", "sex_reg", "age_lp_band", "day_type", "time_slot"))

time_valid %>%
  summarise(
    cor_overall = cor(p_time, p_time_true, use = "complete.obs"),
    mae         = mean(abs(p_time - p_time_true), na.rm = TRUE),
    rmse        = sqrt(mean((p_time - p_time_true)^2, na.rm = TRUE))
  )

purpose_valid <- purpose_weights %>%
  group_by(dong_code, sex_reg, age_lp_band, time_slot) %>%
  summarise(
    sum_p = sum(p_purpose, na.rm = TRUE),
    .groups = "drop"
  )

summary(purpose_valid$sum_p)

mobility_summary %>%
  group_by(age_mig_band) %>%
  summarise(
    mean_dist_mean = mean(mean_dist, na.rm = TRUE),
    mean_dist_sd   = sd(mean_dist, na.rm = TRUE),
    mean_time_mean = mean(mean_time, na.rm = TRUE)
  )

hh_size_weights %>%
  group_by(hh_size_cat) %>%
  summarise(
    mean_p = mean(p_hh_size, na.rm = TRUE),
    sd_p   = sd(p_hh_size, na.rm = TRUE)
  )

hh_type_weights %>%
  group_by(nat_cat) %>%
  summarise(
    mean_p = mean(p_hh_type, na.rm = TRUE),
    sd_p   = sd(p_hh_type, na.rm = TRUE)
  )

# =====================================================================
# (Optional) Save outputs
# =====================================================================

# write_csv(core_pop,          "seoul5_core_pop.csv")
# write_csv(time_slot_weights, "seoul5_time_slot_weights.csv")
# write_csv(purpose_weights,   "seoul5_purpose_weights.csv")
# write_csv(mobility_summary,  "seoul5_mobility_summary.csv")
# write_csv(hh_size_weights,   "seoul5_hh_size_weights.csv")
# write_csv(hh_type_weights,   "seoul5_hh_type_weights.csv")
# write_csv(hh_file,           "seoul5_household_file.csv")
# write_csv(person_file,       "seoul5_person_file.csv")
# write_csv(timetable_panel,   "seoul5_timetable_panel.csv")