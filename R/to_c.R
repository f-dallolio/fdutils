#' Unquotedtocharacter
#'
#' @param ... unquoted names.
#'
#' @return a character vector
#' @export
#'
#' @examples
#' to_c(a,b,c) # "a" "b" "c"
#'
to_c <- function(...){
  out <- rlang::enquos(..., .named = TRUE)
  names(out)
}
