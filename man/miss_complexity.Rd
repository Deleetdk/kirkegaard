% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/missing_data.R
\name{miss_complexity}
\alias{miss_complexity}
\title{Calculate the complexity of missing data}
\usage{
miss_complexity(data)
}
\arguments{
\item{data}{(data.frame/matrix) The data.}
}
\value{
A length 3 vector with the proportions.
}
\description{
Calculates the complexity of the patterns of missing data in the dataset.
}
\details{
This method was proposed by McKnight et al (2007) \link{Missing Data: A Gentle Introduction}.
}
\examples{
miss_complexity(miss_add_random(iris))
}
