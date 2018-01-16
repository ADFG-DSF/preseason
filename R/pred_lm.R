#' Predictions from a linear model
#'
#' Leave-one out predictions from a linear model
#'
#' @param mod a class "lm" object
#'
#' @return a matrix where the first row is the prediction and the second row is the SE of the prediction.
#'
#' @examples
#' pred_lm(mod)
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
