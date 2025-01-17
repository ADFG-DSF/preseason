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
#' @param metric The performance metrics used to evaluate forecast accuracy; "md" = "mean deviation", "mad" = "mean absolute deviation", "mape" = "mean absolute percent error", "maape" = "mean absolute arctan percent error".
#'
#' @return A list containing one figure and one table.
#'
#' @examples
#' dat6 <- prep_brood(deshka, 4:6)
#' (arima6 <- forecast::auto.arima(dat6$age6_ln))
#' dat6$arima_pred <- pred_arima(arima6)[, 1]
#' dat6$es_pred <- pred_es(dat6$age6_ln)
#' comp_models(dat = dat6, comp_age = 6, years = 5)
#'
#' @export
comp_models <- function (dat, comp_age, years = 5, metric = c("md", "mad", "maape")) 
{
  stopifnot(sum(metric %in% c("maape", "mape", "md", "mad")) == length(metric))
  age <- paste0("age", comp_age)
  
  #This section if for getting a model average forecast (inverse maape)
  diff0 <- 
    dat %>% 
    dplyr::select(byr, !!age, dplyr::ends_with("pred")) %>% 
    tidyr::gather(type, pred, -byr, -!!age) %>%
    dplyr::mutate(d = pred - (!!as.name(age)), 
                  pe = d/(!!as.name(age)))
  
  mod_average0 <- 
    diff0 %>% 
    dplyr::group_by(type) %>% 
    dplyr::mutate(maape_1 = zoo::rollmeanr(atan(abs(pe)), 5, fill = NA),
                  maape = dplyr::lag(maape_1, 1),
                  inv_maape = 1 / maape) %>%
    dplyr::arrange(byr) %>%
    dplyr::group_by(byr) %>%
    dplyr::mutate(sum_inv_maape = sum(inv_maape),
                  weight = inv_maape / sum_inv_maape,
                  weight_forecast = pred * weight)
  
  mod_average <- 
    mod_average0 %>%
    dplyr::summarise(average_pred = sum(weight_forecast))
  
  #This section is calculating the errors associated with each model
  diff <- 
    dat %>% 
    dplyr::left_join(mod_average, by = "byr") %>%
    dplyr::select(byr, !!age, dplyr::ends_with("pred")) %>% 
    tidyr::gather(type, pred, -byr, -!!age) %>%
    dplyr::mutate(d = pred - (!!as.name(age)), 
                  pe = d/(!!as.name(age))) %>% 
    dplyr::arrange(type, byr) %>%
    dplyr::group_by(type) %>% 
    dplyr::mutate(md_1 = zoo::rollmeanr(d, years, fill = NA), #Error stats for this brood year and the prior "years" - 1 brood years.
                  mad_1 = zoo::rollmeanr(abs(d), years, fill = NA), #No used in mode selection bc it includes a year we would not know about when selecting the model.
                  mape_1 = zoo::rollmeanr(abs(pe), years, fill = NA),
                  maape_1 = zoo::rollmeanr(atan(abs(pe)), years, fill = NA),
                  md = dplyr::lag(md_1, 1), #Error stats for the prior "years" - 1;"years" - 5 brood years.
                  mad = dplyr::lag(mad_1, 1), #these would be used in model section
                  mape = dplyr::lag(mape_1, 1),
                  maape = dplyr::lag(maape_1, 1))
  
  plot <- 
    ggplot2::ggplot(diff, ggplot2::aes(x = byr, y = pred, color = type)) + 
    ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 3) + 
    ggplot2::geom_point(ggplot2::aes(y = .data[[age]]), color = "black") + 
    ggplot2::geom_line(ggplot2::aes(y = .data[[age]]), color = "black")
    
  
  table <- 
    diff %>% 
    dplyr::filter(byr == max(byr)) %>% #retain last year for model selection
    dplyr::group_by(type) %>% 
    dplyr::select(byr,
                  type,
                  !!metric,
                  pred)
  
  list(plot = plot, 
       table = table, 
       preds = 
         diff %>%
         mutate(age_numeric = gsub("age(\\d+)", "\\1", !!age)) %>%
         select(byr, age = age_numeric, R = !!age, type, md, mad, maape, pred) %>%
         left_join(mod_average0[, c("byr", "type", "weight")], by = c("byr", "type")) %>%
         arrange(byr, type)
  )
}
