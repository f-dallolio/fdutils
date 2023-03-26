#' Names of Vector, Table in a character vector
#'
#' @param x
#' @param values
#' @param negate
#' @param which
#'
#' @return
#' @export
#'
#' @examples
names_in <- function(x, values, negate = FALSE, which = FALSE){
  out <- names(x) %in% values
  if(negate){
    out <-  !out
  }
  if(which) return(which(out))
  out
}
