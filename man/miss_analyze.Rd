% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_data.R
\name{miss_analyze}
\alias{miss_analyze}
\title{Missing data dependencies}
\usage{
miss_analyze(data, robust = F)
}
\arguments{
\item{data}{(df/mat) Data.}

\item{robust}{(lgl scalar) Whether to use robust measures (default false). If true, will use median/mad instead of mean/sd to calculate the standardized mean differences.}
}
\value{
A data frame of size n x n where n is the number of variables in data. The rows are the gropuing variable (missing vs. not-missing) and the columns are the outcome variables.
}
\description{
Calculates Cohen's d or equivalent for every variable pair. Larger values mean that cases with missing data in one variable differ from other cases in the other variable, thus that the data are not missing at random (MCAR).
}
\details{
This method was proposed by McKnight et al (2007) \link{Missing Data: A Gentle Introduction}.
}
\examples{
miss_analyze(miss_add_random(iris))
}
