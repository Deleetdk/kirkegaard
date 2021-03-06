% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_frame_functions.R
\name{group_by_pattern}
\alias{group_by_pattern}
\title{Group by pattern}
\usage{
group_by_pattern(x, pattern)
}
\arguments{
\item{x}{(chr vctr) A character vector of values to group.}

\item{pattern}{(chr sclr) The regex pattern to group variables by.}
}
\value{
A list of integer vectors. Each vector is the indices of the variables that belong to each group.
}
\description{
Determines groups of strings using a regex pattern.
}
