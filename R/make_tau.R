#' iversifications Measures
#'
#' @param num
#' @param hhi
#'
#' @return
#' @export
#'
#' @examples
make_tau <- function(num, hhi){
  hhi <- force_round(hhi, 3)
  cfx_num <- num * hhi - 1
  cfx_den <- num - 1
  cfx0 <- force_round(cfx_num / cfx_den)
  cfx <- max(c(0, cfx0))
  sqrt(cfx)
}

#' @export
make_cfx <- function(num, hhi){
  hhi <- force_round(hhi, 3)
  cfx_num <- num * hhi - 1
  cfx_den <- num - 1
  cfx0 <- force_round(cfx_num / cfx_den)
  cfx <- max(c(0, cfx0))
  cfx
}

#' @export
make_nfx <- function(num, hhi){
  hhi <- force_round(hhi, 3)
  cfx_num <- num * hhi - 1
  cfx_den <- num - 1
  # nfx <- if_else( cfx_den <= 0, 0, hhi - force_round(cfx_num / cfx_den) )
  nfx0 <- hhi - force_round(cfx_num / cfx_den)
  nfn = min(c(0.5, nfx0))
  nfx
}
