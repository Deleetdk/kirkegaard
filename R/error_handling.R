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
#' @keywords try, error
#' @export
#' @examples
#' throws_error()
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
