#' Number of consecutive occurences
#'
#' @param x_lgl logical vector. It indicates the condition for which the number of consective occurences is calculated.
#' @param show_where logical. It reports starting and ending positions of consecutive pattern.
#'
#' @return a numerical vector or a list. If show_where = FALSE the function returns only the lengths of consecutive occurences. If show_where = TRUE the function returns a list containg the lengths of consecutive occurences as well as their starting and ending postiions.
#' @export
#'
#' @examples
#
#' x <- c(1:3, rep(0,4), 5:7, rep(0,6), 9:10)
#' consecutive(x == 0)
#' consecutive(x == 0, show_where = TRUE)

consecutive <- function(x_lgl, show_where = FALSE) {
  stopifnot("'x_lgl' must be logical (TRUE/FALSE)" = is.logical(x_lgl))
  stopifnot("length(x_lgl) must be greater than 1" = length(x_lgl) > 1)
  x_rle <-rle(x_lgl)
  if(sum(x_rle$values) == 0){
    out <- 0
  } else {
    out <- x_rle$lengths[x_rle$values]
  }
  if(show_where){
    out_pos <- which(diff(x_lgl) == 1)
    list(
      "length" = out,
      "start" = out_pos,
      "end" = out_pos + out - 1
    )
  } else {
    out
  }
}
