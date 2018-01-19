#' Plot histogram and fitted lognorm/normal distribution
#'
#' Use to compare empirical data to fitted distibution.
#'
#' @param dat a vector
#'
#' @return a figure
#'
#' @examples
#' plot_dist(brood$age4)
#'
#' @export
comp_models <- function(dat, comp_age, years = 5){
  age <- paste0("age", comp_age)
  diff <- dat %>%
    dplyr::select(byr, !!age, dplyr::ends_with("pred")) %>%
    tidyr::gather(type, pred, -byr, -!!age) %>%
    dplyr::mutate(d = pred - (!!as.name(age)))

plot <- ggplot2::ggplot(diff, ggplot2::aes(x = byr, y = pred, color = type)) +
  ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 3) +
  ggplot2::geom_point(ggplot2::aes_string(y = age), color = "black") +
  ggplot2::geom_line(ggplot2::aes_string(y = age), color = "black")

table <- diff %>%
  dplyr::group_by(type) %>%
  dplyr::top_n(years, byr) %>%
  dplyr::summarise(md = mean(d),
                    mad = mean(abs(d)))#,rmse = sqrt(mean(d^2)))

list(plot, table)
}
