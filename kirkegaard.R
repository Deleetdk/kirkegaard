#libs
library(pacman)
p_load(devtools, roxygen2, stringr, testthat)
options(digits=2, scipen=2)

#make documentation
document()

#install
install("../kirkegaard")
# install("../kirkegaard", upgrade_dependencies = F)

#load
library(kirkegaard)

