#libs
if (!require("pacman")) {
  install.packages("pacman")
  library("pacman")
}
p_load(devtools, roxygen2, stringr, testthat)
options(digits = 2, scipen = 2)

#make documentation
document()

#install
#for some weird reason this sometimes begins reinstalling packages for no reason I can find
#install("../kirkegaard")
install("../kirkegaard", upgrade_dependencies = F)

#load
library(kirkegaard)


# installing from scratch -------------------------------------------------
#for whatever reason, it is hard to get it to install the dependencies automatically
#so here's a small call to manually install them

if (F) {
  #pacman
  if (require("pacman")) install.packages("pacman")

  #CRAN packages
  library(pacman)
  p_load(grid, ggplot2, scales, stringr, purrr, assertthat, readr, xml2, plyr, dplyr, tidyr, psych, gtools, robustbase, MASS, forcats, polycor, weights, devtools, VIM, lsr, compute.es, magrittr, tibble, psychometric, Hmisc, XLConnect, stringdist, geosphere, fields, rmngb, ape, glmnet)

  #github packages
  purrr::map(c("thomasp85/curry"), ~devtools::install_github(.))
}
