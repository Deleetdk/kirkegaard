% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{NA_to_X}
\alias{NA_to_X}
\title{\code{NA} to specified value}
\usage{
NA_to_X(x, replacement)
}
\arguments{
\item{x}{A vector}

\item{replacement}{The value to replace \code{NA} with}
}
\value{
A vector of the same length as x
}
\description{
Replace \code{NA} values in a vector with a specified value.
}
\examples{
c(1, 2, 3, 4) \%>\% NA_to_X(3)
}
