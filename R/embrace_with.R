#' Embrace with custom text
#'
#' @param .x
#' @param embrace
#'
#' @return
#' @export
#'
#' @examples
embrace_with <- function(.x, embrace = "({.x})"){
  out <-glue(embrace)
  out
}
