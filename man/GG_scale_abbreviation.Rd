% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{GG_scale_abbreviation}
\alias{GG_scale_abbreviation}
\title{Plot results from scale abbreviation}
\usage{
GG_scale_abbreviation(
  x,
  vars = c("reliability_frac", "mean_criterion_cors_frac", "r_full_score", "reliability"),
  add_lines_for_full_model = F
)
}
\arguments{
\item{x}{Output from \code{scale_abbreviation()}}

\item{vars}{Which variables to plot. Default is c("reliability_frac", "mean_criterion_cors_frac", "r_full_score", "reliability")}

\item{add_lines_for_full_model}{Add lines for the full model for comparison. Default is FALSE.}
}
\value{
A ggplot2 object
}
\description{
Plots results from the output of \code{scale_abbreviation()}.
}
\examples{
library(mirt)
#simulate some mirt data 2PL
set.seed(1)
dat = mirt::simdata(N = 1e3, itemtype = "2PL", a = runif(100, 0.5, 2), d = rnorm(100, sd = 0.5))
#fit the model
fit = mirt::mirt(dat, 1)
#scale abbreviation
short_scale = abbreviate_scale(as.data.frame(dat), method = "max_loading", item_target = 10)
#plot
GG_scale_abbreviation(short_scale)
}
