#libs
library(devtools)
library(roxygen2)
library(stringr)
library(testthat)

#options
options(digits = 2, scipen = 2)

#update package description version automatically
readr::read_lines("DESCRIPTION") %>%
  str_replace("Version: .+", str_glue("Version: {Sys.Date()}")) %>%
  readr::write_lines("DESCRIPTION")

#make documentation
devtools::document()

#install
#for some weird reason this sometimes begins reinstalling packages for no reason I can find
devtools::install("../kirkegaard")

#load
library(kirkegaard)
library(testthat)
