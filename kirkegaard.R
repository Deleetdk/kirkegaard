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

#run the tests
#cannot use run using source() because the clipboard must be used to test one function
