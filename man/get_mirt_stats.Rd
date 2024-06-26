% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mirt.R
\name{get_mirt_stats}
\alias{get_mirt_stats}
\title{Extract item parameters from \code{mirt()} fit}
\usage{
get_mirt_stats(x)
}
\arguments{
\item{x}{A \code{mirt} fit object}
}
\value{
A data frame of item parameters
}
\description{
Extract item parameters from \code{mirt()} fit
}
\examples{
library(mirt)
data = simdata(seq(0.2, 2, length.out = 5), seq(-2, 2, length.out = 5), 1000, itemtype = "2PL")
fit = mirt(data, 1)
get_mirt_stats(fit)
}
