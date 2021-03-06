% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_IO.R
\name{split_into_n_columns}
\alias{split_into_n_columns}
\title{Split data into n columns.}
\usage{
split_into_n_columns(
  data,
  split_times,
  pad_rows = T,
  include_rownames = T,
  rownames_var = "name",
  include_colnames = T
)
}
\arguments{
\item{data}{(data.frame, matrix, or something coercible into a matrix) The data to reshape.}

\item{split_times}{(whole number scalar) How many times to split the rows.}

\item{include_colnames}{(logical) Whether to include the column names in the output. Defaults to TRUE.}

\item{pad_columns}{(logical) Whether to pad empty columns to the data if the data and column dimensions do fit divide into a whole number. Defaults to TRUE.}

\item{rownames_colnames}{(character scalar) If adding colnames in rows, which rownames should these be given? Defaults to "name".}
}
\description{
Reshapes the data to a wider structure for easy use in documents. Pads empty lines between and can include their rownames and colnames as well. Outputs a character matrix.
}
\examples{
df = data.frame(small = letters[1:6], big = LETTERS[1:6], stringsAsFactors = F)
split_into_n_columns(df, 2) #ok
split_into_n_columns(df, 3) #ok
split_into_n_columns(df, 4) #stupid but no error!
}
