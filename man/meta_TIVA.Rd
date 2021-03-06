% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta.R
\name{meta_TIVA}
\alias{meta_TIVA}
\title{Funnel plot with ggplot2}
\usage{
meta_TIVA(.rma, print_plot = T)
}
\arguments{
\item{.analysis}{(rma object) The rma analysis from \link{metafor}.}

\item{.CI}{(chr vector) Confidence interval to use.}

\item{.study_CI}{(lgl vector) Whether to plot confidence intervals for individual studies.}
}
\description{
Makes a pretty funnel plot using \link{ggplot2}.
}
\examples{
library(metafor)
data(european_ancestry)
meta = rma(european_ancestry$r, sei = european_ancestry$SE_r)
meta_TIVA(meta)
}
