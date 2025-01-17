#' One-step ahead predictions from a exponential smoothing model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the an exponential smoothing ("ANN") model to be used in evaluating model performance in past seasons.
#'
#' @param mod a class "ets" object
#'
#' @return a vector of predictions.
#'
#' @examples
#' es5 <- forecast::ets(dat5$age5_ln, "ANN") 
#' dat5$es_pred <- pred_es(es5)
#'
#' @export
pred_es <- function(mod){
  stopifnot(class(mod) == "ets")
  
  preds <- sapply(5:length(mod$x), function(l){
    train <- mod$x[1:l]
    newmod <- forecast::ets(train, model = paste0(mod$components[1:3], collapse = ""))
    predict(newmod, h = 1)$mean})
  c(rep(NA, 5), exp(preds))
}
