% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stereotypes.R
\name{score_by}
\alias{score_by}
\title{Score stereotypes by moderator}
\usage{
score_by(
  x,
  moderator,
  extrapolate_to = "minmax",
  method = lm,
  drop_unused_levels = T,
  long_format = F
)
}
\arguments{
\item{x}{Data to score (numerical)}

\item{moderator}{A moderator variable (vector input, same length as x rows)}

\item{extrapolate_to}{Values to extrapolate to if using a numerical moderator. Default is minimum and maximum values of observed moderator values.}

\item{method}{Which method to use for numerical extrapolation. Default is a linear model, but could be e.g. loess.}

\item{drop_unused_levels}{Whether to drop unused factor levels if categorical moderator (default is to drop them)}

\item{long_format}{Transposes the data so that each column has a set of estimates instead of each row.}
}
\value{
A dataframe with expected value by moderator levels or values sought.
}
\description{
Score stereotypes by moderator
}
\examples{
test_data = tibble(
a = c(1:4),
b = c(5:8)
)

#chr moderator
score_by(test_data, moderator = c("m", "m", "f", "f"))
#specified levels and thus order
score_by(test_data, moderator = c("m", "m", "f", "f") \%>\% factor(levels = c("m", "f")))
#numerical moderator
score_by(test_data, moderator = seq(0, 1, length.out = 4))
#long format
score_by(test_data, moderator = c("m", "m", "f", "f"), long_format = T)
score_by(test_data, moderator = seq(0, 1, length.out = 4), long_format = T)
}
