options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(readr)
  library(purrr)
  library(sf)
  library(ggplot2)
  library(viridis)
  library(here)
})

# -----------------------
# Reproducibility
# -----------------------
SEED <- 2025
set.seed(SEED)

# -----------------------
# Locale / Scope
# -----------------------
loc_kr <- readr::locale(encoding = "CP949")
target_gu <- c("금천구", "구로구", "관악구", "영등포구", "동작구")
base_month <- "2024.06"

# -----------------------
# Paths (NO absolute paths)
# -----------------------
paths <- list(
  data = list(
    jumin     = here("data", "processed", "ind_pop_final_jumin.csv"),
    lp        = here("data", "processed", "lp_final.csv"),
    purpose   = here("data", "processed", "purpose_final_kor.csv"),
    mig       = here("data", "processed", "lp_in_out_korean.csv"),
    hh_type2  = here("data", "processed", "hh_type_2.csv"),
    hn_final  = here("data", "processed", "hn_final.csv"),
    hh_gen    = here("data", "processed", "house_generation_level_2.xlsx"),
    # shp_dir = here("data", "spatial", "dong_shp")
    shp_rds   = here("data", "processed", "shp_joined.rds") 
  ),
  out = list(
    data   = here("outputs", "data"),
    tables = here("outputs", "tables"),
    figs   = here("outputs", "figures"),
    logs   = here("outputs", "logs")
  )
)

dir.create(paths$out$data, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$out$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$out$figs, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$out$logs, recursive = TRUE, showWarnings = FALSE)
