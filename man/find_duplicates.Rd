% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{find_duplicates}
\alias{find_duplicates}
\title{Find duplicated elements}
\usage{
find_duplicates(x)
}
\arguments{
\item{x}{(vector) A vector.}
}
\value{
a list
}
\description{
Find groups of duplicates elements in a vector.
}
\examples{
find_duplicates(c(1, 1, 2, 2, 3, 4, 5, 5))
}
