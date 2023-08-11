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
cov <- function(x,
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
  cov_mat <- stats::cov(.tbl)
  if(original){
    return(cov_mat)
  }
  pair_tbl <- make_pairwise(.tbl) %>%
    rlang::set_names(c("x1", "x2"))
  out <- pair_tbl %>%
    dplyr::mutate(
      cov = purrr::map2(
        .x = x1, .y = x2,
        .f = ~ cov_mat[.x, .y] )
    ) %>%
    purrr::list_c()
  if(out_list){ return(as.list(out)) }
  out
}
