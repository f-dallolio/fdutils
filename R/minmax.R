#' Mini-max - coerce to (0/1) or (0/100)
#'
#' @param x
#' @param min
#' @param max
#' @param to_100
#'
#' @return
#' @export
#'
#' @examples
minmax <- function(x, min = NULL, max = NULL, to_100 = FALSE){
  if(is.null(min)){
    min = min(x, na.rm = TRUE)
  }
  if(is.null(max)){
    max = max(x, na.rm = TRUE)
  }

  out <- (x - min) / (max - min)
  if(to_100){
    return(out * 100)
  } else {
    return(out)
  }
}
