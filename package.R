#libs
if (!require("pacman")) {
  install.packages("pacman")
  library("pacman")
}
pacman::p_load(devtools, roxygen2, stringr, testthat)
options(digits = 2, scipen = 2)

#iupdate package description version automatically
readr::read_lines("DESCRIPTION") %>%
  str_replace("Version: .+", str_glue("Version: {Sys.Date()}")) %>%
  readr::write_lines("DESCRIPTION")

#make documentation
devtools::document()

#install
#for some weird reason this sometimes begins reinstalling packages for no reason I can find
#install("../kirkegaard")
devtools::install("../kirkegaard")

#load
pacman::p_load(kirkegaard, testthat)

