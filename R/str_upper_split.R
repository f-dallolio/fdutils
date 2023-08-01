#' Split String at Uppercse
#'
#' @param string the string to modify
#' @param sep a string indicating the separator.
#' @param to_lower TRUE or FALSE
#'
#' @return a string
#' @export
#'
#' @examples

str_upper_split <- function(string, sep = "_", to_lower = TRUE){
  out <- gsub('([[:upper:]])', stringr::str_c(sep, '\\1'), string) |>
    stringr::str_sub(2,-1)
  if(to_lower){
    return(out |> tolower())
  } else{
    return(out)
  }
}
