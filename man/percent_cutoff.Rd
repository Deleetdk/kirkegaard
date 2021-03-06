% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_math.R
\name{percent_cutoff}
\alias{percent_cutoff}
\title{Find percentage of numbers that are above given thresholds.}
\usage{
percent_cutoff(x, cutoffs = c(0.3, 0.5), digits = 2, below = F, inclusive = T)
}
\arguments{
\item{x}{(num vector) A vector of numbers.}

\item{cutoffs}{(num vector) A vector of thresholds. Default=(.30, .50)}

\item{digits}{(num scalar) The number of digits to round output to. Default=2.}

\item{below}{(log scalar) Whether to count values below the cutoff (default false).}

\item{inclusive}{(log scalar) Whether to include values at the cutoff (default true).}
}
\description{
Takes a numeric vector and a numeric vector of thresholds. Returns the percent of numbers in the first above each of the numbers in the second.
}
\examples{
percent_cutoff(iris$Sepal.Length, cutoffs = 4:8)
percent_cutoff(iris$Sepal.Length, cutoffs = 4:8, below = T) #reverse cutoff
percent_cutoff(c(1:3, NA, NaN, 4:6), cutoffs = 3) #ignores NA/NaN
}
