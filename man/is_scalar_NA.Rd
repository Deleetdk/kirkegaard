% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_are.R
\name{is_scalar_NA}
\alias{is_scalar_NA}
\title{Is scalar NA?}
\usage{
is_scalar_NA(x, error_on_null = T)
}
\arguments{
\item{x}{(obj) An object to check.}

\item{erro_on_null}{(lgl) Whether to throw an error on NULL, or treat as TRUE.}
}
\value{
Logical scalar.
}
\description{
Check whether an object is a scalar NA
}
\examples{
is_scalar_NA(NA)
is_scalar(1:3)
is_scalar_NA(c(1, NA))
is_scalar_NA(c(NA, 1))
}
