% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_are.R
\name{all_the_same}
\alias{all_the_same}
\title{Are all elements of a vector the same?}
\usage{
all_the_same(x)
}
\arguments{
\item{x}{(num vector) A numericor or character vector.}
}
\description{
Tests whether all elements of a simple vector are the same.
}
\examples{
all_the_same(rep(1, 100))
all_the_same(rnorm(100))
all_the_same(letters[rep(1, 10)])
all_the_same(letters[sample(1:10, size = 10)])
}
