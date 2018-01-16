#' Predictions from a exponential smoothing model
#'
#' One step forward predictions from a exponential smoothing model
#'
#' @param mod a class "lm" object
#'
#' @return a vector of predictions.
#'
#' @examples
#' pred_lm(mod)
#'
#' @export
pred_es <- function(x){
  preds <- sapply(6:length(x), function(l){
    train <- x[1:(l-1)]
    newmod <- forecast::ets(train, "ANN")
    predict(newmod, h = 1)$mean})
  c(rep(NA, 5), exp(preds))
}
