#' Predictions from a moving average model
#'
#' One step forward predictions from a moving average model
#'
#' @param var a log transformed time series. 
#' @param yrs years in the moving average. NULL to average the entire series.
#'
#' @return a matrix with columns "mean" and "median" providing predictions on the natural scale.
#'
#' @examples
#' pred_ma(var)
#'
#' @export
pred_ma <- function(var, yrs = 5){
  df <-
    lapply(1:length(var), function(x) {
      if (is.null(yrs)) {
        start <- 1
      }
      else {
        start <- max(1, x - yrs)
      }
      y <- var[start:(x - 1)]
      c(mean = exp(mean(y) + var(y)/2),
        median = exp(mean(y)))
    })
  do.call(rbind, df)
}