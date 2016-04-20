### Functions for error handling

#' Is an object a try-error?
#'
#' Checks whether an object is a try-error. Returns a boolean.
#' @param x (an object) An object to check.
#' @keywords try, error
#' @export
#' @examples
#' is_error()
is_error = function(x) "try-error" %in% class(x)

#' Does call return an error?
#'
#' Parses a string as a call and determines whether it returns an error or not. Returns a boolean.
#' @param x (character scalar) Code to run in a string.
#' @param silent_try (log scalar) Whether to use a silent try (default true).
#' @export
#' @examples
#' throws_error("log('123')") #cannot take a logarithm of a string
throws_error = function(x, silent_try = T) {

  #if it is a string, try to evaluate it
  if (is.character(x)) {
    trial = try(eval(parse(text = x)), silent = silent_try)
  } else { #else, return an error
    stop("x wasn't a string! The code to call must be a string.")
    #trial = try(eval(parse(text = object_to_string(x))), silent = silent_try)
    #not sure why this doesn't work
  }

  return(is_error(trial))
}


#' Fail if input contains NA
#'
#' Inputs any object, checks if it contains NA. If yes, throws an error. If not, returns the input.
#' @param x (any object) The object to test.
#' @export
#' @examples
#' fail_if_NA(1:10) #no NAs
#' fail_if_NA(c(1:3, NA, 1:3)) #NAs
fail_if_NA = function(x) {
  #fail if any NA
  if (any(is.na(x))) stop("Input contained NA.")

  #return input
  x
}
