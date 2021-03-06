% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{GG_contingency_table}
\alias{GG_contingency_table}
\title{Plot a contingency table with ggplot2}
\usage{
GG_contingency_table(data, var1, var2, margin = NULL)
}
\arguments{
\item{data}{(data.frame) The data.}

\item{var1}{(chr scalar) The name of the first variable (vertical)}

\item{var2}{(chr scalar) The name of the second variable (horizontal)}

\item{margin}{(NULL, 1, 2) Which margin to use. Default = NULL.}
}
\description{
Makes a pretty contingency table with ggplot2 using geom_tile.
}
\examples{
GG_contingency_table(mpg, "drv", "cyl")
GG_contingency_table(mpg, "drv", "cyl", margin = 1)
GG_contingency_table(mpg, "drv", "cyl", margin = 2)
}
