#libs

library(pacman)
p_load(devtools, roxygen2)

#make documentation
setwd("./kirkegaard")
document()

#install
setwd("..")
install("kirkegaard")

#load
library(kirkegaard)

#make datasets
source("kirkegaard/R/datasets.R")
