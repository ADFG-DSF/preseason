#' One-step ahead predictions from a linear model
#'
#' Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the an exponential smoothing model to be used in evaluating model performance in past seasons.
#'
#' @param mod a class "lm" object
#'
#' @return a matrix where the first row is the prediction and the second row is the SE of the prediction.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' sib6 <- lm(age6_ln ~ age5_ln, data = dat6)
#' pred_lm(sib6)
#'
#' @export
pred_lm <- function(mod){
  sapply(1:nrow(mod$model), function(x){
    train <- mod$model[-x, ]
    test <- mod$model[x, ]
    mod <- update(mod, data = train)
    preds <- predict(mod, newdata = test, se = TRUE)
    c(preds$fit, preds$se.fit)})
}
