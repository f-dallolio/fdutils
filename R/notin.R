#' Not in
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#' x <- letters[1:5]
#' y <- c("4",2,"a")
#' x %notin% y
#' y %notin% x

`%notin%` <- function(x, y){
  !(x %in% y)
}
