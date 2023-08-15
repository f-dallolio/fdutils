#' Split String at Uppercse
#'
#' @param string the string to modify
#' @param sep a string indicating the separator.
#' @param to_lower TRUE or FALSE
#' @param exceptions list with exceptions.
#'
#' @return a string
#' @export
#'
#' @examples

str_upper_split <- function (string, sep = "_",  to_lower = TRUE, exceptions = NULL){
  out <- gsub(pattern = "([[:upper:]])",
              replacement = " \\1",
              x = string) %>%
    str_squish() %>%
    str_replace_all(pattern = " ",
                    replacement = sep)
  if (to_lower) {
    out <- tolower(out)
  }
  if(!is.null(exceptions)){
    for(i in seq_along(exceptions)){
      out <- out %>% str_replace_all(names(exceptions)[[i]], exceptions[[i]])
    }
  }
  out
}
