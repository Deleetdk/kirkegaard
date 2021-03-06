% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{get_dims}
\alias{get_dims}
\title{Get dimensions of object.}
\usage{
get_dims(x)
}
\arguments{
\item{x}{(an object) An object.}
}
\description{
Returns the dimensions of an object. Also works on atomic (1-d) objects for which base-r dim() returns NULL.
}
\examples{
v = 1:10
get_dims(v)
m = matrix(1:9, nrow=3)
get_dims(m)
}
