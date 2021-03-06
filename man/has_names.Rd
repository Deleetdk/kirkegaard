% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_are.R
\name{has_names}
\alias{has_names}
\title{Does object have names?}
\usage{
has_names(x)
}
\arguments{
\item{x}{(any object) The object to test.}
}
\value{
Logical scalar.
}
\description{
A wrapper for names to test for existence of names.
}
\examples{
has_names(iris)
has_names(1:4)
}
