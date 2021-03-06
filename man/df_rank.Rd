% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_frame_functions.R
\name{df_rank}
\alias{df_rank}
\title{Rank numeric variables of a data frame.}
\usage{
df_rank(df, ...)
}
\arguments{
\item{df}{(data frame) A data.frame.}

\item{...}{Other parameters for \code{\link{rank}}.}
}
\description{
Returns a data.frame where numeric variables have been converted to their ranks.
}
\examples{
head(df_rank(iris)) #rank iris data. Automatically skips non-numeric columns.
}
