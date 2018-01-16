#' Predictions from a moving average model
#'
#' One step forward predictions from a moving average model
#'
#' @param mod a class "lm" object
#'
#' @return a vector of predictions.
#'
#' @examples
#' pred_lm(mod)
#'
#' @export
pred_ma <- function(var, yrs = NULL){
  sapply(1:length(var), function(x){
    if(is.null(yrs)) {start <- 1} else {start <- max(1, x-yrs)}
    y <- var[start:(x-1)]
    exp(mean(y) + var(y)/2)
  })
}
