# ============================================================
# code/R/validation.R
# Validation functions for Seoul5-DigitalTwin-Population
# ------------------------------------------------------------
# Provides:
#  (1) Temporal validity (Correlation / MAE / RMSE) comparing
#      timetable panel vs Living Population time-slot probabilities
#  (2) Distributional consistency (QQ-ready quantiles)
#  (3) Intra-household age gap plausibility dataset + simple flags
#  (4) Spatial assessment (dong-level MAE + correlation)
#
# Expected inputs (objects created by your pipeline):
#  - timetable_panel: individual-level panel (ind_id × day_type × time_slot [+ purpose])
#  - time_slot_weights: LP-derived probabilities p_time by dong×sex×age×day×slot
#  - person_file: person-level file with hh_id, gen_cat, head_age_band, age_reg_band
#
# Notes:
#  - This file only defines functions. Call them from scripts/99_run_pipeline.R
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

# ------------------------------------------------------------
# Helper: age-band string -> approximate mid-age (numeric)
# Supports patterns like:
#  "20~24세", "60세이상", "70-74세" (start used), "0-9세" (start used)
# ------------------------------------------------------------
get_age_mid <- function(age_band) {
  if (is.null(age_band) || length(age_band) == 0) return(NA_real_)

  vapply(age_band, function(x) {
    if (is.na(x)) return(NA_real_)
    s <- stringr::str_replace_all(as.character(x), " ", "")

    # Extract start number
    start_num <- suppressWarnings(as.numeric(stringr::str_extract(s, "^[0-9]+")))
    if (is.na(start_num)) return(NA_real_)

    # Extract end number if "~" exists
    end_num <- suppressWarnings(as.numeric(stringr::str_extract(s, "(?<=~)[0-9]+")))

    dplyr::case_when(
      !is.na(end_num) ~ (start_num + end_num) / 2,
      stringr::str_detect(s, "이상") ~ start_num + 5,  # heuristic for open-ended group
      TRUE ~ start_num
    )
  }, numeric(1))
}

# ============================================================
# (1) Temporal validity: Correlation / MAE / RMSE
#    Compare generated panel time-slot shares vs LP probabilities
# ============================================================
validate_temporal_metrics <- function(
    timetable_panel,
    time_slot_weights,
    by = c("dong_code", "sex_reg", "age_lp_band", "day_type", "time_slot"),
    panel_prob_col = "p_time_panel",
    ref_prob_col   = "p_time"
) {

  # Build panel probabilities by cell
  panel_agg <- timetable_panel %>%
    group_by(across(all_of(by))) %>%
    summarise(n_panel = n(), .groups = "drop") %>%
    group_by(across(all_of(setdiff(by, "time_slot")))) %>%
    mutate(
      n_panel_total = sum(n_panel, na.rm = TRUE),
      !!panel_prob_col := if_else(n_panel_total > 0, n_panel / n_panel_total, NA_real_)
    ) %>%
    ungroup()

  panel_valid <- panel_agg %>%
    left_join(time_slot_weights, by = by)

  metrics <- panel_valid %>%
    summarise(
      correlation = cor(.data[[panel_prob_col]], .data[[ref_prob_col]], use = "complete.obs"),
      MAE  = mean(abs(.data[[panel_prob_col]] - .data[[ref_prob_col]]), na.rm = TRUE),
      RMSE = sqrt(mean((.data[[panel_prob_col]] - .data[[ref_prob_col]])^2, na.rm = TRUE))
    )

  list(
    panel_valid = panel_valid,
    metrics = metrics
  )
}

# ============================================================
# (2) Distributional consistency: QQ-ready quantiles
#     Use panel_valid returned above
# ============================================================
make_qq_data <- function(
    panel_valid,
    observed_col = "p_time",
    synthetic_col = "p_time_panel",
    probs = seq(0, 1, 0.01)
) {
  tibble(
    q = probs,
    observed_q  = as.numeric(stats::quantile(panel_valid[[observed_col]],  probs, na.rm = TRUE)),
    synthetic_q = as.numeric(stats::quantile(panel_valid[[synthetic_col]], probs, na.rm = TRUE))
  )
}

# ============================================================
# (3) Household plausibility: intra-household age gaps
#     Returns row-level age gaps for plotting/checks
# ============================================================
compute_household_age_gaps <- function(
    person_file,
    head_age_band_col = "head_age_band",
    member_age_band_col = "age_reg_band",
    hh_id_col = "hh_id",
    gen_cat_col = "gen_cat"
) {
  out <- person_file %>%
    filter(!is.na(.data[[hh_id_col]]), !is.na(.data[[head_age_band_col]])) %>%
    mutate(
      head_age_val = get_age_mid(.data[[head_age_band_col]]),
      ind_age_val  = get_age_mid(.data[[member_age_band_col]]),
      age_gap      = head_age_val - ind_age_val
    ) %>%
    filter(!is.na(age_gap), age_gap != 0) %>%
    mutate(
      flag_minor_head  = !is.na(head_age_val) & head_age_val < 18,
      flag_extreme_gap = !is.na(age_gap) & abs(age_gap) > 80
    ) %>%
    select(
      all_of(c(hh_id_col, gen_cat_col)),
      head_age_val, ind_age_val, age_gap,
      flag_minor_head, flag_extreme_gap
    )

  out
}

# ============================================================
# (4) Spatial assessment: dong-level MAE + correlation
#     Input: panel_valid (from validate_temporal_metrics)
# ============================================================
compute_spatial_error <- function(
    panel_valid,
    dong_code_col = "dong_code",
    observed_col = "p_time",
    synthetic_col = "p_time_panel",
    pad10 = TRUE
) {
  panel_valid %>%
    mutate(
      dong_code_std = as.character(.data[[dong_code_col]]),
      dong_code_std = if (pad10) stringr::str_pad(dong_code_std, 10, pad = "0") else dong_code_std
    ) %>%
    group_by(dong_code_std) %>%
    summarise(
      mae = mean(abs(.data[[synthetic_col]] - .data[[observed_col]]), na.rm = TRUE),
      cor = cor(.data[[synthetic_col]], .data[[observed_col]], use = "complete.obs"),
      .groups = "drop"
    ) %>%
    rename(dong_code = dong_code_std)
}

# ============================================================
# One-stop wrapper: run all validations
# ============================================================
run_validations <- function(
    timetable_panel,
    time_slot_weights,
    person_file
) {
  temporal <- validate_temporal_metrics(
    timetable_panel   = timetable_panel,
    time_slot_weights = time_slot_weights
  )

  qq_data <- make_qq_data(
    panel_valid = temporal$panel_valid,
    observed_col = "p_time",
    synthetic_col = "p_time_panel"
  )

  age_gaps <- compute_household_age_gaps(person_file = person_file)

  spatial <- compute_spatial_error(panel_valid = temporal$panel_valid)

  list(
    temporal_metrics   = temporal$metrics,
    panel_valid        = temporal$panel_valid,
    qq_data            = qq_data,
    household_age_gaps = age_gaps,
    spatial_error      = spatial
  )
}

# ============================================================
# Example (put this in scripts/99_run_pipeline.R):
#
# source("code/R/validation.R")
# val <- run_validations(timetable_panel, time_slot_weights, person_file)
# print(val$temporal_metrics)
# head(val$qq_data)
# head(val$household_age_gaps)
# head(val$spatial_error)
# ============================================================
