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
