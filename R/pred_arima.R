#' One-step ahead predictions from a ARIMA model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the selected ARIMA model to be used in evaluating model performance in past seasons.
#'
#' @param mod a class "Arima" object
#' @param x the time series used to create the model 
#' @param xreg Values of covariate used to create the model
#'
#' @return a matrix where the first row is the prediction and the second row is the SE of the prediction.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' ARIMA6_ar1 <- arima(dat6$age6_ln, order=c(1,0,0))
#' pred_arima(ARIMA6_ar1, x = dat6$age6_ln)
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
