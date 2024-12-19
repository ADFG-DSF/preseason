#' One-step ahead predictions from a exponential smoothing model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the an exponential smoothing ("ANN") model to be used in evaluating model performance in past seasons.
#'
#' @param x a time series.
#' @param model_0 A three-character string identifying model arg for forecast::ets. The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend type ("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected. In this usage "ANN" (the default) is general selected by forecast::ets but if the user notices a different specification this parameters allows them to specify that model during the hindcast.
#'
#' @return a vector of predictions.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' pred_es(dat6$age6_ln)
#'
#' @export
pred_es <- function(x, model_0 = "ANN"){
  preds <- sapply(6:length(x), function(l){
    train <- x[1:(l-1)]
    newmod <- forecast::ets(train, model = model_0)
    predict(newmod, h = 1)$mean})
  c(rep(NA, 5), exp(preds))
}
