#' Title
#'
#' @param tbl
#' @param .cols
#' @param .fns
#'
#' @return
#' @export
#'
#' @examples

summary_tbl <- function(tbl, .cols , .fns){
  out <- map(
    .fns,
    ~ summary_stat(tbl, {{ .cols }}, .f = .x)
  )
  out
}
