#' One-step ahead predictions from a exponential smoothing model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the an exponential smoothing ("ANN") model to be used in evaluating model performance in past seasons.
#'
#' @param x a time series.
#'
#' @return a vector of predictions.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' pred_es(dat6$age6_ln)
#'
#' @export
pred_es <- function(x){
  preds <- sapply(6:length(x), function(l){
    train <- x[1:(l-1)]
    newmod <- forecast::ets(train, "ANN")
    predict(newmod, h = 1)$mean})
  c(rep(NA, 5), exp(preds))
}
