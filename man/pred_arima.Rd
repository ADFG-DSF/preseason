% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_arima.R
\name{pred_arima}
\alias{pred_arima}
\title{One-step ahead predictions from a ARIMA model}
\usage{
pred_arima(mod, x, xreg = NULL)
}
\arguments{
\item{mod}{a class "Arima" object}

\item{x}{the time series used to create the model}

\item{xreg}{Values of covariate used to create the model}
}
\value{
a matrix where the first row is the prediction and the second row is the SE of the prediction.
}
\description{
Creates a series of one-step ahead predictions for all years beyond the 5th year in the time series using the selected ARIMA model to be used in evaluating model performance in past seasons.
}
\examples{
dat6 <- prep_brood(deshka, 4:6)
ARIMA6_ar1 <- arima(dat6$age6_ln, order=c(1,0,0))
pred_arima(ARIMA6_ar1, x = dat6$age6_ln)

}
