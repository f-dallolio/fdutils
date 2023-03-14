#' Get string between embrace
#'
#' @param .x
#' @param .left
#' @param .right
#'
#' @return
#' @export
#'
#' @examples
embrace_get <- function(.x, .left = "\\(", .right  = "\\)")
                        # , re_embrace = "(@.x@)")
  {
  x_match <- glue("(?<={.left}).+?(?={.right})")
  stringr::str_match_all(.x, x_match)[[1]][,1]
}
