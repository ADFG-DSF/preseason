#' One-step ahead predictions from a ARIMA model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the selected ARIMA model to be used in evaluating model performance in past seasons.
#'
#' @param mod a class "Arima" object
#'
#' @return a matrix where the first row is the prediction and the second row is the SE of the prediction.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' (arima6 <- forecast::auto.arima(dat6$age6_ln))
#' dat6$arima_pred <- pred_arima(arima6)[, 1]
#'
#' @export
pred_arima <- function(mod){
  stopifnot("forecast_ARIMA" %in% class(mod))
  
  x <- mod$x
  xreg <- mod$xreg
  pred <- sapply(6:length(x), function(l){
    train <- x[1:(l-1)]
    train_xreg <- xreg[1:(l-1)]
    newmod <- update(mod, x = train, xreg = train_xreg)
    preds <- predict(newmod, newxreg = xreg[l])
    c(exp(preds$pred), preds$se)})
  rbind(matrix(NA, 5, 2), t(pred))
}

