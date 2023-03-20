#' Title
#'
#' @param x
#' @param out_list
#'
#' @return
#' @imports tibble, dplyr, tidyr
#' @export
#'
#' @examples
make_pairwise <- function (x, out_list = FALSE)
{
  stopifnot(`x must be matrix/dataframe/tibble with NCOL > 1` = NCOL(x) >
              1)
  no_tibble <- !(tibble::is_tibble(x))
  if (no_tibble) {
    x <- tibble::as_tibble(x)
  }
  seq_cols <- 1:NCOL(x)
  out <- tidyr::expand_grid(V1 = seq_cols, V2 = seq_cols) %>%
    group_by(V1) %>%
    filter(V2 > V1)
  if (out_list) {
    as.list(out)
  }
  else {
    out
  }
}
