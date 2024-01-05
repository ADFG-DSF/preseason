#' Plot/table hindcast performance
#'
#' Plot/table hindcast estimates versus actual values and produce a table with hindcast performance metrics.
#'
#' @param dat A data frame with a column providing:\describe{
#'  \item{byr}{brood year}
#'  \item{age#}{one or more columns proving the total return of age # fish using the naming convention age#}
#'  \item{xxx_pred}{one or more columns containing the hindcast prediction for the return of fish of "comp_age" using the naming convention xxx_pred. The prefix "xxx" will be used to name the prediction in the output.}}
#' @param comp_age The total age of fish being forecast.
#' @param years The number of past years used to evaluate forecast accuracy.
#' @param metric The performance metrics used to evaluate forecast accuracy; "md" = "mean deviation", "mad" = "mean absolute deviation" or "mape" = "mean absolute percent error".
#'
#' @return A list containing one figure and one table.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' ARIMA6_ar1 <- arima(dat6$age6_ln, order=c(1,0,0))
#' dat6$ARIMA_pred <- exp(pred_arima(ARIMA6_ar1, x = dat6$age6_ln)[1,])
#' dat6$es_pred <- pred_es(dat6$age6_ln)
#' comp_models(dat = dat6, comp_age = 6, years = 5)
#'
#' @export
comp_models <- function(dat, comp_age, years = 5, metric = c("md", "mad")){
  stopifnot(sum(metric %in% c("mape", "md", "mad")) == length(metric))
  
  age <- paste0("age", comp_age)
  diff <- dat %>%
    dplyr::select(byr, !!age, dplyr::ends_with("pred")) %>%
    tidyr::gather(type, pred, -byr, -!!age) %>%
    dplyr::mutate(d = pred - (!!as.name(age)))

plot <- ggplot2::ggplot(diff, ggplot2::aes(x = byr, y = pred, color = type)) +
  ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  ggplot2::geom_point(ggplot2::aes_string(y = age), color = "black") +
  ggplot2::geom_line(ggplot2::aes_string(y = age), color = "black")

table0 <- diff %>%
  dplyr::group_by(type) %>%
  dplyr::top_n(years, byr)

  if("mape" %in% metric){
    table <- 
      table0 %>%
      dplyr::summarise(md = mean(d),
                       mad = mean(abs(d)),
                       mape = mean(abs(pe)))
  }else{
    table <- 
      table0 %>%
      dplyr::summarise(md = mean(d),
                       mad = mean(abs(d)))
  }

list(plot, table)
}
