% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_frame_functions.R
\name{df_add_delta}
\alias{df_add_delta}
\title{Add delta columns to a data.frame.}
\usage{
df_add_delta(
  df,
  primary_var,
  secondary_vars = NULL,
  prefix = "delta",
  sep = "_",
  subtract_from_primary = T,
  standardize = F
)
}
\arguments{
\item{df}{(data.frame) A data.frame.}

\item{primary_var}{(character or numeric vector) The primary variable to use.}

\item{secondary_vars}{(character or numeric vector) Which secondary variables to use. Defaults to all non-primary variables.}

\item{prefix}{(character) The prefix to use on the new variables. Default="delta".}

\item{sep}{(character) The separator to use for the new variables. Default="_".}

\item{subtract_from_primary}{(boolean) Whether to subtract from the primary variable. Defaults to T. If F, then the primary will be subtracted from the secondaries.}

\item{standardize}{(boolean) Whether to standardize the difference scores. Defaults to F.}
}
\description{
Adds delta (difference) columns to a data.frame. These are made from one primary variable and a number of secondary variables. Variables can be given either by indices or by name. If no secondary variables are given, all numeric variables are used.
}
\examples{
#add delta variables to iris between Sepal.Length and Sepal.Width
head(df_add_delta(iris, primary_var = 1, secondary_vars = 2))
#add delta variables to iris between Sepal.Length and all other numerical variables
head(df_add_delta(iris, primary_var = 1))
}
