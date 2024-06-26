% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_data.R
\name{miss_locf}
\alias{miss_locf}
\title{Last observation carried forward}
\usage{
miss_locf(x, reverse = F)
}
\arguments{
\item{x}{A vector}

\item{reverse}{Whether to do it in reverse}
}
\value{
A vector
}
\description{
Last observation carried forward
}
\examples{
c(NA, 1, NA, 2, NA) \%>\% miss_locf()
c(NA, 1, NA, 2, NA) \%>\% miss_locf(reverse = T)
c(NA, 1, NA, 2, NA, NA, NA) \%>\% miss_locf()
}
