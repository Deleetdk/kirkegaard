% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_IO.R
\name{merge_datasets}
\alias{merge_datasets}
\title{Improved dataset merger function}
\usage{
merge_datasets(
  DF1,
  DF2,
  join = "both",
  overwrite_NA = FALSE,
  restore_factors = FALSE
)
}
\arguments{
\item{DF1}{(data.frame) A data.frame to merge into.}

\item{DF2}{(data.frame) A data.frame with the new data.}

\item{join}{(character scalar) Which data.frame to use cases from. Options: left, right, both.}

\item{overwrite_NA}{(lgl scalar) Whether to overwrite with NA values.}

\item{restore_factors}{(lgl scalar) Whether to recreate factors in the merged data.frame. Does not keep levels.}
}
\description{
This function allows you to merge two data.frames by their overlapping rownames. About 15 times faster than the earlier version.
}
\examples{
merge_datasets(iris[1:4], iris[1:5]) #merge together two parts of iris
}
