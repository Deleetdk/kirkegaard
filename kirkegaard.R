#libs
library(pacman)
p_load(devtools, roxygen2)

#make documentation
try({setwd("./kirkegaard")}) #if this fails, it probably means we are already in the right dir
document()

#install
setwd("..")
install("kirkegaard")

#load
library(kirkegaard)
