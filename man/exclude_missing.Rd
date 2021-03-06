% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_operations.R
\name{exclude_missing}
\alias{exclude_missing}
\title{Exclude missing datapoints}
\usage{
exclude_missing(x, .NA = T, .NULL. = T, .NaN = T, .Inf = T)
}
\arguments{
\item{x}{(an interatable object) An object to subset based on missingness.}

\item{.NA}{(logical scalar) Whether to exclude NA (default TRUE).}

\item{.NaN}{(logical scalar) Whether to exclude NaN (default TRUE).}

\item{.NULL}{(logical scalar) Whether to exclude NULL (default TRUE).}
}
\description{
Exclude datapoints that are NA, NULL or NaN.
}
\details{
Does not remove NA etc. recursively. See the complex list example.
}
\examples{
x = list(1, NA, 2, NULL, 3, NaN, 4, Inf)
exclude_missing(x)
exclude_missing(x, .NA = F)
exclude_missing(x, .NULL = F)
exclude_missing(x, .NaN = F)
exclude_missing(x, .Inf = F)
#complex list
x = list(1, NA, 2, NULL, 3, NaN, 4, Inf, 1:3, c(1, NA, 3))
exclude_missing(x) #does not remove NAs recursively
}
