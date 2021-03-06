% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strings.R
\name{str_filter}
\alias{str_filter}
\title{Filter string by pattern}
\usage{
str_filter(string, pattern, reverse = F)
}
\arguments{
\item{string}{(chr) A string.}

\item{pattern}{(chr) Pattern to look for (regex).}

\item{reverse}{(lgl) Whether to return the non-matched values instead (default no).}
}
\value{
A character vector.
}
\description{
A wrapper for stringr's \code{\link{str_detect}} that returns the matched values, or the non-matched values.
}
\examples{
str_filter(letters, pattern = "[aeiou]")
str_filter(letters, pattern = "[aeiou]", reverse = T)
}
