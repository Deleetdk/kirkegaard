#libs
if (!require("pacman")) {
  install.packages("pacman")
  library("pacman")
}
pacman::p_load(devtools, roxygen2, stringr, testthat)
options(digits = 2, scipen = 2)

#make documentation
devtools::document()

#install
#for some weird reason this sometimes begins reinstalling packages for no reason I can find
#install("../kirkegaard")
devtools::install("../kirkegaard")

#load
pacman::p_load(kirkegaard, testthat)

