#' Predictions from a ARIMA model
#'
#' One step forward predictions from a ARIMA model.
#'
#' @param mod a class "lm" object
#'
#' @return a matrix where the first row is the prediction and the second row is the SE of the prediction.
#'
#' @examples
#' pred_lm(mod)
#'
#' @export
pred_arima <- function(mod, x, xreg = NULL){
  pred <- sapply(6:length(x), function(l){
    train <- x[1:(l-1)]
    train_xreg <- xreg[1:(l-1)]
    newmod <- update(mod, x = train, xreg = train_xreg)
    preds <- predict(newmod, newxreg = xreg[l])
    c(preds$pred, preds$se)})
  cbind(matrix(NA, 2, 5), pred)
}
