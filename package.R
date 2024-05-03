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
devtools::install("../kirkegaard", dependencies = F)

#load
library(kirkegaard)
library(testthat)

#first time installation
if (F) {
  # Function to extract packages from .R files
  find_used_packages <- function(file_paths) {
    # Container for all identified packages
    all_packages <- character()

    # Loop over each file
    ns_names = c()
    for (file in file_paths) {
      # Read the file
      lines <- readLines(file, warn = FALSE)

      # Find usage of explicit namespace prefixing
      ns_usage <- grep("::", lines, value = TRUE)
      if (length(ns_usage) > 0) {

        # Extract namespace names
        ns_names <- sapply(regmatches(ns_usage, gregexpr("\\b[a-zA-Z0-9.]+::", ns_usage)), function(x) gsub("::", "", x)) |> unlist() |>
          unique()
        #add to collection
        all_packages = c(all_packages, ns_names)
      }
    }

    # Remove duplicates and return
    return(unique(all_packages))
  }

  #scan all .R files, and add tidyverse
  (kirkegaard_packages = c(find_used_packages(dir(pattern = ".R$", recursive = T, full.names = T)), "tidyverse"))

  #not installed packages
  (not_installed_packages = kirkegaard_packages[!(kirkegaard_packages %in% installed.packages()[,"Package"])] |> setdiff("kirkegaard"))

  #install them
  if (length(not_installed_packages) > 0) {
    install.packages(not_installed_packages)
  }
}
