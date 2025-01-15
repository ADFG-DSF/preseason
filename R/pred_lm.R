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
pred_lm <- function(mod, dat){
  pred <- sapply(6:nrow(mod$model), function(x){
    train <- mod$model[1:(x - 1), ]
    test <- mod$model[x, -1, drop = FALSE]
    mod <- update(mod, data = train)
    preds <- predict(mod, newdata = test, se = TRUE)
    c(exp(preds$fit), preds$se.fit)})
  forecast <- predict(mod, newdata = dat[nrow(mod$model) + 1, names(mod$model)[-1], drop = FALSE], se = TRUE)
  rbind(matrix(NA, 5, 2), t(pred), c(exp(forecast$fit), forecast$se.fit))
}

# pred_lm <- function(mod){
#   sapply(1:nrow(mod$model), function(x){
#     train <- mod$model[-x, ]
#     test <- mod$model[x, ]
#     mod <- update(mod, data = train)
#     preds <- predict(mod, newdata = test, se = TRUE)
#     c(exp(preds$fit), preds$se.fit)})
#   t(pred)
# }

fore_lm <- function(dat, mod){
  new_data <- data.frame(dat[nrow(dat), names(mod$model)])
  forecast <- predict(mod, newdata = new_data, se.fit = TRUE)
  c(forecast$fit, forecast$se.fit)
}
