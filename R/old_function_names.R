### old names for functions
#these are just small redirection functions

#function generator
defunct = function(msg = "This function is depreciated") function(...) return(stop(msg))
