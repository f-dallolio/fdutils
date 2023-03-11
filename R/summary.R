summary_stat <- function(x, ..., .f){
  foo <- as_mapper(.f)
  with(
    x,
    x %>%
      select(...) %>%
      as.list() %>%
      map(foo)
  )
}

summary_tbl <- function(tbl, .cols , .fns){
  out <- map(
    .fns,
    ~ summary_stat(tbl, {{ .cols }}, .f = .x)
  )
  out
}
