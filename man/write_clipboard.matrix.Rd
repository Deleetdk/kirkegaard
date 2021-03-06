% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_IO.R
\name{write_clipboard.matrix}
\alias{write_clipboard.matrix}
\title{Write matrix to clipboard}
\usage{
\method{write_clipboard}{matrix}(
  x,
  digits = 2,
  clean_names = T,
  clean_what = c("_", "\\\\."),
  pad_digits = T,
  print = T,
  .rownames = T,
  write_to_clipboard = interactive(),
  return_modified = F,
  capitalize_dimnames = T,
  na = ""
)
}
\description{
Write a matrix to the clipboard with clean formating. Calls \code{\link{write_clipboard.data.frame}}.
}
