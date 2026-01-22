read_inputs <- function(paths, loc_kr) {
  jumin    <- readr::read_csv(paths$data$jumin,    locale = loc_kr, show_col_types = FALSE)
  lp       <- readr::read_csv(paths$data$lp,       locale = loc_kr, show_col_types = FALSE)
  purpose  <- readr::read_csv(paths$data$purpose,  locale = loc_kr, show_col_types = FALSE)
  mig      <- readr::read_csv(paths$data$mig,      locale = loc_kr, show_col_types = FALSE)
  hh_type2 <- readr::read_csv(paths$data$hh_type2, locale = loc_kr, show_col_types = FALSE)
  hn_final <- readr::read_csv(paths$data$hn_final, locale = loc_kr, show_col_types = FALSE)
  hh_gen_raw <- readxl::read_excel(paths$data$hh_gen)
  
  list(
    jumin = jumin,
    lp = lp,
    purpose = purpose,
    mig = mig,
    hh_type2 = hh_type2,
    hn_final = hn_final,
    hh_gen_raw = hh_gen_raw
  )
}

validate_input_schema <- function(inp) {
  assert_has_cols(inp$jumin,    c("지역","시기","성별","연령","인구수"), "jumin")
  assert_has_cols(inp$lp,       c("시군구명","읍면동명","행정기관코드10","요일구분","시간대","성별","연령","생활인구수"), "lp")
  assert_has_cols(inp$purpose,  c("시군구명","행정동명","행정기관코드","성별","연령","시간대","목적_eng","생활인구수"), "purpose")
  assert_has_cols(inp$mig,      c("출발시군구명.x","출발행정동명.x","연령대","이동인구수","평균이동거리","평균이동시간"), "mig")
  assert_has_cols(inp$hn_final, c("연도","지역","유형","값"), "hn_final")
  assert_has_cols(inp$hh_type2, c("연도","연령","지역","유형","값"), "hh_type2")
  invisible(TRUE)
}
