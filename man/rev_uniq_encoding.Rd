% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{rev_uniq_encoding}
\alias{rev_uniq_encoding}
\title{Reverse unique encoding}
\usage{
rev_uniq_encoding(x)
}
\arguments{
\item{x}{Output from \code{\link[=uniq_encoding]{uniq_encoding()}}}
}
\value{
A vector
}
\description{
Reverse unique encoding
}
\examples{
c(1, 2, 2, 3, 3, 3) \%>\% uniq_encoding() \%>\% rev_uniq_encoding()
c(1, 2, 2, 3, 3, 3) \%>\% uniq_encoding() \%>\% rev_uniq_encoding()
}
