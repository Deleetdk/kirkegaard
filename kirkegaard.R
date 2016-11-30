#libs
library(pacman)
p_load(devtools, roxygen2, stringr, testthat)

#make documentation
document()

#install
install("../kirkegaard")

#load
library(kirkegaard)

