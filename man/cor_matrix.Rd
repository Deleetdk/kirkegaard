% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistics.R
\name{cor_matrix}
\alias{cor_matrix}
\title{Correlation matrix}
\usage{
cor_matrix(
  data,
  weights = NULL,
  by = NULL,
  reliabilities = NULL,
  CI = NULL,
  CI_template = "\%r [\%lower \%upper]",
  skip_nonnumeric = T,
  CI_round = 2,
  p_val = F,
  p_template = "\%r [\%p]",
  p_round = 3,
  rank_order = F,
  asterisks = c(0.01, 0.005, 0.001),
  asterisks_only = T
)
}
\arguments{
\item{data}{(data.frame or coercible into data.frame) The data.}

\item{weights}{(numeric vector, numeric matrix/data.frame or character scalar) Weights to use for the correlations. Can be a numeric vector with weights, the name of a variable in data, or a matrix/data.frame with weights for each variable. If the latter, then harmonic means are used. If none given, defaults to rep(1, nrow(data)).}

\item{by}{Grouping variable}

\item{reliabilities}{(num vector) Reliabities used to correct for measurement error. If not present, assumed to be 1.}

\item{CI}{(numeric scalar) The confidence level to use as a fraction.}

\item{CI_template}{(character scalar) A template to use for formatting the confidence intervals.}

\item{skip_nonnumeric}{(logical scalar) Whether to skip non-numeric variables. Defaults to TRUE.}

\item{CI_round}{(whole number scalar) If confidence intervals are used, how many digits should be shown?}

\item{p_val}{(log scalar) Add p values or not.}

\item{p_template}{(chr scalar) If p values are desired, the template to use.}

\item{p_round}{(int scalar) Number of digits to round p values to. Uses scientific notation for small numbers.}

\item{rank_order}{(lgl or chr) Whether to use rank ordered data so as to compute Spearman's correlations instead.}

\item{asterisks}{The thresholds to use for p value asterisks}

\item{asterisks_only}{Whether to only include astrisks not numerical values}
}
\description{
Outputs a correlation matrix. Supports weights, confidence intervals, correcting for measurement error and rounding.
}
\details{
Correction for measurement error is done using the standard Pearson formula: r_true = r_observed / sqrt(reliability_x * reliability_y).

Weighted correlations are calculated using wtd.cor or wtd.cors from weights package.

\code{rank_order} can take either a logical scalar or a character scalar. If given TRUE, it will use rank ranking method with the default settings (average ranks). If given a chr scalar, it will use that ranking method. If given FALSE, will not use rank data (default).

Confidence intervals are analytic confidence intervals based on the standard error.
}
\examples{
cor_matrix(iris) #just correlations
cor_matrix(iris, CI = .95) #with confidence intervals
cor_matrix(iris, CI = .99) #with 99\% confidence intervals
cor_matrix(iris, p_val = .95) #with p values
cor_matrix(iris, p_val = .95, p_template = "\%r (\%p)") #with p values, with an alternative template
cor_matrix(iris, reliabilities = c(.8, .9, .7, .75)) #correct for measurement error
cor_matrix(iris, reliabilities = c(.8, .9, .7, .75), CI = .95) #correct for measurement error + CI
cor_matrix(iris, rank_order = T) #rank order correlations, default method
cor_matrix(iris, rank_order = "first") #rank order correlations, specific method
cor_matrix(iris, weights = "Petal.Width") #weights from name
cor_matrix(iris, weights = 1:150) #weights from vector
#complex weights
cor_matrix(iris, weights = matrix(runif(nrow(iris) * 4), nrow = nrow(iris)))
cor_matrix(iris, weights = matrix(runif(nrow(iris) * 4), nrow = nrow(iris)), CI = .95)
#groups
cor_matrix(iris, by = iris$Species)
cor_matrix(iris, by = iris$Species, weights = 1:150)
}
