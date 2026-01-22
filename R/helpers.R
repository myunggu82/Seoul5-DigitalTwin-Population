recode_age_to_lp <- function(age_band_reg) {
  a <- str_replace_all(age_band_reg, " ", "")
  start <- str_extract(a, "^[0-9]+") %>% as.numeric()
  case_when(
    is.na(start) ~ NA_character_,
    start < 10   ~ "0-9세",
    start < 15   ~ "10-14세",
    start < 20   ~ "15-19세",
    start < 30   ~ "20-29세",
    start < 40   ~ "30-39세",
    start < 50   ~ "40-49세",
    start < 60   ~ "50-59세",
    TRUE         ~ "60세이상"
  )
}

recode_age_to_mig <- function(age_band_reg) {
  start <- str_extract(age_band_reg, "^[0-9]+") %>% as.numeric()
  case_when(
    is.na(start) ~ NA_character_,
    start < 20   ~ "00-19세",
    start < 40   ~ "20-39세",
    start < 60   ~ "40-59세",
    TRUE         ~ "60세+"
  )
}

get_age_mid <- function(age_band) {
  if (is.null(age_band)) return(NA_real_)
  a <- str_replace_all(age_band, " ", "")
  start <- str_extract(a, "^[0-9]+") %>% as.numeric()
  end   <- str_extract(a, "(?<=~)[0-9]+") %>% as.numeric()
  case_when(
    is.na(start) ~ NA_real_,
    !is.na(end)  ~ (start + end) / 2,
    str_detect(a, "이상") ~ start + 5,
    TRUE ~ start
  )
}
