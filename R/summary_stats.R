#' Title
#'
#' @param x
#' @param .width
#'
#' @return
#'
#' @examples


mean <- function(x, ...) {
  tibble(
    # variable = names(x),
    stat = "mean",
    label = "Mean",
    value = DescTools::Mean(x, na.rm = TRUE, ...)
  )
}

sd <- function(x, ...){
  tibble(
    # variable = names(x),
    stat = "sd",
    label = "SD",
    value = DescTools::SD(x, na.rm = TRUE, ...)
  )
}

median <- function(x, ...){
  tibble(
    # variable = names(x),
    stat = "median",
    label = "Median",
    value = DescTools::Median(x, na.rm = TRUE, ...)
  )
}

iqr <- function(x, ...){
  tibble(
    # variable = names(x),
    stat = "iqr",
    label = "IQR",
    value = DescTools::IQRw(x, na.rm = TRUE, ...)
  )
}

quantile <- function(x, ...){
  tibble(
    # variable = names(x),
    stat = "quantile",
    label = "SD",
    value = DescTools::Quantile(x, names = TRUE, na.rm = TRUE, ...)
  )
}

hdi <- function (x, .width = 0.90){
  width_size <- length(.width)
  if (width_size > 1) {
    .width <- sort(.width)
    out <- map_dfr(.x = .width,
                   ~HDInterval::hdi(x, .x) %>%
                     as.vector() %>%
                     as_tibble() %>%
                     mutate(stat = "hdi",
                            label = "HDI",
                            .before = 1) %>%
                     mutate(type = c("Lower", "Upper"),
                            width = .x))
  }
  else {
    out <- tibble(
      stat = "hdi",
      value = hdi(x, .width)
    ) %>%
      as.vector() %>%
      as_tibble() %>%
      mutate(stat = "hdi",
             label = "HDI",
             .before = 1) %>%
      mutate(type = c("Lower", "Upper"),
             width = .width)
  }
  return(out)
}

# summary_vec <- function(
#     x,
#     ... ,
#     weights = NULL
#     # , .f = NULL
#   ){
#   foo <- as_mapper(.f)
#   with(
#     x,
#     x %>%
#       select(...) %>%
#       as.list() %>%
#       map(foo)
#   )
# }
