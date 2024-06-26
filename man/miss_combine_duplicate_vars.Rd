% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_data.R
\name{miss_combine_duplicate_vars}
\alias{miss_combine_duplicate_vars}
\title{Combine duplicate variables from a join back into single variables}
\usage{
miss_combine_duplicate_vars(x, vars = NULL, priority = "x")
}
\arguments{
\item{x}{A data frame}

\item{vars}{A character vector of variable names to consider. If NULL, all variables are considered.}

\item{priority}{Which variable to prioritize when combining. Default is "x".}
}
\value{
A data frame
}
\description{
Looks for variable pairs with names ending in ".x" and ".y" and combines them into a single variable. This is done so that no new missing data is introduced. Will not work for second order duplicates from joins, e.g. var.x.x.
}
\examples{
d1 = tibble(
id = 1:3,
y = c(1, 2, 3),
x = c(1, NA, NA)
)

d2 = tibble(
id = 1:3,
x = c(NA, 2, NA),
z = c(1, 2, 3)
)

d3 = tibble(
id = 1:3,
x = c(NA, NA, 3),
a = letters[1:3]
)

d1 \%>\%
  left_join(d2, by = "id") \%>\%
  miss_combine_duplicate_vars() \%>\%
  left_join(d3, by = "id") \%>\%
  miss_combine_duplicate_vars()
}
