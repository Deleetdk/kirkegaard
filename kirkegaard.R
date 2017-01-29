#libs
library(pacman)
p_load(devtools, roxygen2, stringr, testthat)

#make documentation
document()

#install
install("../kirkegaard")
# install("../kirkegaard", upgrade_dependencies = F)

#load
library(kirkegaard)

