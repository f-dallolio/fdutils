#' Logit transform
#'
#' @param x
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
logit <- function(x, alpha = 0.1){
  stopifnot("x must be between 0 and 1" = dplyr::between(x, 0, 1))
  x <- x[!is.na(x)]
  if( min(x) == 0 | max(x) == 1 ){
    x <- minmax(x, min = 0, max = 1, alpha = 0.05)
  }
  qlogis(x)
}
