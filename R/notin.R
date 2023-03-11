#' Not in
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
`%notin%` <- function(x, table){
  !(x %in% table)
}

nomatch <- function(...) !match(...)

names_in <- function(x, values, negate = FALSE, which = FALSE){
  out <- names(x) %in% values
  if(negate){
    out <-  !out
  }
  if(which) return(which(out))
  out
}
