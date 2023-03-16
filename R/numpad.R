#' Padding numeric values
#'
#' @param x an integer or a vector of integers.
#' @param pad a character input indicating what is padding the input.
#'
#' @return a string or character vector of padded values.
#' @export
#'
#' @examples
#' numpad(1:15)
#'
#'
numpad <- function(x, pad = "0") {
  x <- as.integer(x)
  stopifnot("x must be coercible to an integer vector" = is.integer(x))
  stopifnot("pad must be string" = is.character(pad))
  x <- as.character(x)
  width <- max(nchar(x))
  # max_nchar <- max(nchar(x))
  # if(is.null(width)){width <- max_nchar}
  # stopifnot("width must be >= max(nchar(x))", width < max_nchar)

  stringr::str_pad(
    string = x,
    width = width,
    side = "left",
    pad = pad
  )
}


# test_that("numpad works", {
#   expect_success(numpad(c))
#   expect_equal(cos(pi / 4), 1 / sqrt(2))
#   expect_equal(tan(pi / 4), 1)
# })
