#' Mini-max - coerce to (0/1) or (0/100)
#'
#' @param x
#' @param min
#' @param max
#' @param x100
#' @param alpha
#'
#' @return
#' @export
#'
#' @examples
minmax <- function(x, min = NULL, max = NULL, x100 = FALSE, alpha = 0) {
  alpha1 = 1 - alpha
  if(is.null(min)){
    min <- min(x)
  }
  if(is.null(max)){
    max <- max(x)
  }
  med <- median(x)
  out <-  (((x - med) * alpha1 + med)  / (max - min))
  if(x100){
    return(out * 100)
  }
  return(out)
}


