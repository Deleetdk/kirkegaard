% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{format_digits}
\alias{format_digits}
\title{Format number of digits}
\usage{
format_digits(x, digits = 2)
}
\arguments{
\item{x}{(numeric vector) The number(s) to format.}

\item{digits}{(whole number scalar) The number of digits to show.}
}
\value{
Returns a character vector of the same length as x.
}
\description{
A wrapper for \code{\link{format}} and \code{\link{round}} that makes sure that a certain number of digits are shown after rounding. Useful for outputting numbers for tables. Vectorized.
}
\examples{
format_digits(c(.1), 2)
format_digits(c(.1), 5)
format_digits(c(.12345), 2)
format_digits(c(.15555), 2)
}
