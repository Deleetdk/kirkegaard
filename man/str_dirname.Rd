% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_dirname}
\alias{str_dirname}
\title{Extract directory path from a file path.}
\usage{
str_dirname(x)
}
\arguments{
\item{x}{(chr) A path to a filename.}
}
\value{
A character.
}
\description{
Extract directory path from a file path.
}
\details{
Just a thin wrapper for \code{\link[base:basename]{base::dirname()}}.
}
\examples{
dir(getwd(), full.names = T) \%>\% str_dirname()
}
