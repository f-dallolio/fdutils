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
cor <- function(x,
                    out_list = FALSE,
                    original = FALSE,
                    use = "complete.obs",
                    ...)
{
  stopifnot("x must be matrix/dataframe/tibble with NCOL > 1" = NCOL(x) > 1)
  no_tibble <- !tibble::is_tibble(x)
  if(no_tibble) {
    .tbl <- tibble::as_tibble(x)
  } else {
    .tbl <- x
  }
  nms <- colnames(.tbl)
  cor_mat <- stats::cor(.tbl)
  if(original){
    return(cor_mat)
  }
  pair_tbl <- make_pairwise(.tbl) |>
    rlang::set_names(c("x1", "x2"))
  out <- pair_tbl |>
    dplyr::mutate(
      cor = purrr::map2_dbl(
        .x = x1,
        .y = x2,
        .f = ~ cor_mat[.x, .y]),
      x1 = colnames(.tbl)["x1"],
      x2 = colnames(.tbl)["x2"],
    )
  if(out_list){ return(as.list(out)) }
  out
}
