#' One-step ahead predictions from a exponential smoothing model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the an exponential smoothing ("ANN") model to be used in evaluating model performance in past seasons.
#'
#' @param mod a class "ets" object
#'
#' @return a vector of predictions.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' pred_es(dat6$age6_ln)
#'
#' @export
pred_es <- function(mod){
  preds <- sapply(5:length(mod$x), function(l){
    train <- mod$x[1:l]
    newmod <- forecast::ets(train, model = paste0(mod$components[1:3], collapse = ""))
    predict(newmod, h = 1)$mean})
  c(rep(NA, 5), exp(preds))
}

fore_es <- function(mod){
  exp(predict(mod$x, h = 1)[["mean"]][1])
}