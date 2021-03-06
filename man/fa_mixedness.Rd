% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_analysis.R
\name{fa_mixedness}
\alias{fa_mixedness}
\title{Calculate mixedness metrics}
\usage{
fa_mixedness(df, ...)
}
\arguments{
\item{df}{A data.frame to calculate mixedness metrics for.}

\item{...}{Parameters to \code{\link{fa_MAR}}. These get passed on to \code{\link{fa}}.}
}
\description{
Returns 4 metrics that attempt to identify cases that are structural outliers/mixed in theor structure.
}
\details{
MAR, mean absolute residuals. Measures the how well indicator scores can be prediced from the factor scores.

CFS, change in factor size. Measures how much the factor size changes with direction.

ACFS, absolute change in factor size. Measures how much the factor size changes without direction.

MeanALC, mean absolute loading change. Measures how much loadings are affected in general.

MaxALC, max absolute loading change. Measures the maximal loading change.
}
\examples{
fa_mixedness(iris[-5])
}
