#' Title
#'
#' @param x
#' @param ...
#' @param .f
#'
#' @return
#' @export
#'
#' @examples

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
