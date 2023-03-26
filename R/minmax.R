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
minmax <- function(x, min, max, x100 = FALSE, alpha = 0) {
  alpha1 = 1 - alpha
  # x <- x[!is.na(x)]
  med <- median(x, na.rm = TRUE)
  out <-  (((x - med) * alpha1 + med)  / (max - min))
  if(x100){
    return(out * 100)
  }
  return(out)
}


