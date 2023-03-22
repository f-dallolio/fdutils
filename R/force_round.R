#' Round numerics
#'
#' @param x
#' @param decimals
#'
#' @return
#' @export
#'
#' @examples
force_round <- function(x, decimals = 3){
  if(
    (decimals %% 1) == 0 & decimals > 1
  ){
    decimals <- as.integer(3)
  }
  stopifnot( "decimals must be a positive integer" = is.integer(decimals) &&  decimals > 1 )

  multiplier <- 10 ^ decimals

  as.integer( round(x, decimals) * multiplier ) / multiplier
}

#' @export
force_ceiling <- function(x, decimals = 3){
  if(
    (decimals %% 1) == 0 & decimals > 1
  ){
    decimals <- as.integer(3)
  }
  stopifnot( "decimals must be a positive integer" = is.integer(decimals) &&  decimals > 1 )

  multiplier <- 10 ^ decimals

  as.integer(ceiling(x * multiplier)) / multiplier
}

#' @export
force_floor <- function(x, decimals = 3){
  if(
    (decimals %% 1) == 0 & decimals > 1
  ){
    decimals <- as.integer(3)
  }
  stopifnot( "decimals must be a positive integer" =
               is.integer(decimals) &&  decimals > 1 )

  multiplier <- 10 ^ decimals

  as.integer( floor(x * multiplier) ) / multiplier
}
