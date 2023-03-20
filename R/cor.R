#' Title
#'
#' @param x
#'
#' @return
#' @import tibble dplyr purrr
#' @export
#'
#' @examples
#'
cor <- function (x,
                 out_list = FALSE,
                 original = FALSE,
                 use = "complete.obs",
                 ...)
{
  stopifnot(`x must be matrix/dataframe/tibble with NCOL > 1` =
              NCOL(x) > 1)

  no_tibble <- !tibble::is_tibble(x)

  if (no_tibble) {
    .tbl <- tibble::as_tibble(x)
  } else {
    .tbl <- x
  }

  nms <- colnames(.tbl)

  cor_mat <- stats::cor(.tbl)

  if (original) {
    return(cor_mat)
  }

  pair_tbl <- rlang::set_names(make_pairwise(.tbl), c("V1",
                                                      "V2"))
  out <- dplyr::mutate(pair_tbl, cor = purrr::map2_dbl(.x = V1,
                                                       .y = V2, .f = ~cor_mat[.x, .y]),
                       V1 = colnames(.tbl)[V1],
                       V2 = colnames(.tbl)[V2])
  if (out_list) {
    return(as.list(out))
  }
  out
}
