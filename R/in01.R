#' Between 0 and 1
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
logit <- function(x, ...){
  x <- x[!is.na(x)]
  qlogis(x)
}

#' @export
inv_logit <- function(x){
  x <- x[!is.na(x)]
  plogis(x)
}

#' @export
minmax <- function(x, min, max, x100 = FALSE, alpha = 0) {
  alpha1 = 1 - (alpha)
  x <- x[!is.na(x)]
  med <- median(x)
  out <-  (((x - med - .1) * alpha1 + med + .1)  / (max - min))
  if(x100){
    return(out * 100)
  }
  return(out)
}


