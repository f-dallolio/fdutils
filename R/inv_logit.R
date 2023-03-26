#' Inverse Logit transform
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
inv_logit <- function(x){
  x <- x[!is.na(x)]
  plogis(x)
}
