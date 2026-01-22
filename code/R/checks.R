assert_has_cols <- function(df, cols, df_name = deparse(substitute(df))) {
  miss <- setdiff(cols, names(df))
  if (length(miss) > 0) {
    stop(sprintf("[%s] Missing required columns: %s", df_name, paste(miss, collapse=", ")), call. = FALSE)
  }
  invisible(TRUE)
}

assert_unique_key <- function(df, key_cols, df_name = deparse(substitute(df))) {
  dup <- df %>% count(across(all_of(key_cols))) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop(sprintf("[%s] Key is not unique: %s", df_name, paste(key_cols, collapse=", ")), call. = FALSE)
  }
  invisible(TRUE)
}

pad10 <- function(x) stringr::str_pad(as.character(x), 10, pad = "0")

check_sum_to_one <- function(df, group_cols, prob_col, tol = 1e-6) {
  out <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(sum_p = sum(.data[[prob_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(ok = abs(sum_p - 1) <= tol)
  out
}
