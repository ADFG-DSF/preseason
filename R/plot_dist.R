#' Plot histogram and fitted lognormal and normal distributions
#'
#' Use to compare empirical data to fitted distribution.
#'
#' @param dat a vector
#'
#' @return a figure
#'
#' @examples
#' plot_dist(deshka$age5)
#'
#'
#' @export
plot_dist <- function(dat){
  n_fit <- fitdistrplus::fitdist(dat[!is.na(dat)], "norm")
  n_line <- dnorm(1:(1.2*max(dat, na.rm = TRUE)), n_fit$estimate[1], n_fit$estimate[2])
  ln_fit <- fitdistrplus::fitdist(ifelse(dat == 0, 0.5, dat)[!is.na(dat)], "lnorm")
  ln_line <- dlnorm(1:(1.2*max(dat, na.rm = TRUE)), ln_fit$estimate[1], ln_fit$estimate[2])
  lim <- if(max(ln_line) > 3 * max(n_line)) NULL else c(0, max(ln_line))
  title <- deparse(substitute(dat))
  hist(dat, freq = FALSE, ylim = lim, main = title, xlab = "Return")
  lines(n_line, col = "green")
  lines(ln_line, col = "red")
  legend(x = "topright", legend = c("normal", "lognormal"), col = c("green", "red"), lty = 1)
}
