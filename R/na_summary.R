#' Summary of Missing Values
#'
#' @param x a vector.
#'
#' @return a number.
#' @export
#'
#' @examples
#' x <- c(1,2,3,NA,5,NA,7,8,9,10)
#' n_missing(x)
#' n_complete(x)
#' missing_pct(x)
#' complete_pct(x)
#'
n_missing <- function (x) {
  sum(is.na(x) | is.null(x))
}

#' @export
n_complete <- function (x) {
  sum(!is.na(x) & !is.null(x))
}

#' @export
missing_pct <- function (x){
  n_missing(x)/length(x)
}

#' @export
complete_pct <- function (x){
  1 - n_missing(x)/length(x)
}

