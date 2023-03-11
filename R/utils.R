#' Named lists
#'
#' @description Create a named list using specified names or, if names are omitted, using the names of the objects in the list. The code list(a = a, b = b) becomes nlist(a,b) and list(a = a, b = 2) becomes nlist(a, b = 2), etc.
#' @param ... Objects to include in the list.
#' @return A named list.
#' @import rlang
#' @export
#' @examples
list <- function ( ... , .named = TRUE, .base = FALSE)
{
  if(.base){
    return(base::list(...))
  } else {
    return(rlang::dots_list(..., .named = .named))
  }
}

