#' Moving average model estimates
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using a log scale moving average of the time series to be used in evaluating model performance in past seasons.
#'
#' @param var a log transformed time series. 
#' @param yrs years in the moving average. NULL to average the entire series.
#'
#' @return a matrix with columns "mean" and "median" providing predictions on the natural scale.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' pred_ma(dat6$age6_ln)
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