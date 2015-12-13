### Functions for easy subsetting

#the idea is to combine functional programming with subsetting for cases where one needs conditional subsetting and plyr's mapvalues cannot easily be used


# helper functions --------------------------------------------------------
#these could be defined with math_to_function but that would be slightly slower


#' Test if value is negative
#'
#' A wrapper for "<0", returns a boolean.
#' @param x (any object compatible with <) Some values to test.
#' @keywords negative, value, boolean
#' @export
#' @examples
#' is_negative()
is_negative = function(x) x < 0


#' Test if value is positive
#'
#' A wrapper for ">0", returns a boolean.
#' @param x (any object compatible with <) Some values to test.
#' @keywords positive, value, boolean
#' @export
#' @examples
#' is_positive()
is_positive = function(x) x > 0



#' Test if value is zero
#'
#' A wrapper for "==0", returns a boolean.
#' @param x (any object compatible with <) Some values to test.
#' @keywords zero, value, boolean
#' @export
#' @examples
#' is_zero()
is_zero = function(x) x == 0



#' Change values conditionally
#'
#' Change values based on a condition given as a function.
#' @param x (vector, data.frame, matrix or list) An object whose values to change.
#' @param func (function) A function that returns a boolean.
#' @param func_str (character scalar) A string to make a function from using math_to_function().
#' @param new_value (scalar vector) A value to use in the cases.
#' @param handle_NA (boolean) Whether to handle missing values. Default=T.
#' @keywords conditional, change, value
#' @export
#' @examples
#' conditional_change()
conditional_change = function(x, func, func_str, new_value, handle_NA = T) {
  #libs
  library(plyr)

  #checks
  if (missing("x")) stop("x is missing!")
  if (missing("func") & missing("func_str")) stop("Both func and func_str are missing. You must supply a function!")
  if (missing("new_value")) stop("new_value is missing. You must supply a replacement value!")
  if (!any(sapply(list(is.vector, is.data.frame, is.matrix, is.list), function(y) {
    y(x)
  }))) stop("x was not a vector, data.frame, matrix or list. Other objects are not supported!")
  if (!handle_NA) {
    if (any(is.na(x))) stop("Missing values present!")
  }

  #shorthands for functions
  if (!missing("func_str")) {
    if (func_str == "<0") func = is_negative
    if (func_str == ">0") func = is_positive
    if (func_str == "=0") func = is_zero
    if (func_str == "==0") func = is_zero
  }

  #make function
  if (missing("func")) func = math_to_function(func_str)

  #conditional

  trial = try({
    v_targets = func(x)

    #handle NA?
    if (handle_NA) v_targets = mapvalues(v_targets, NA, F, warn_missing = F) #map NA to F
  }, silent = T)
  #fails for lists


  #change by class
  if (is.vector(x) & !is.list(x)) { #simple vector
    x[v_targets] = new_value
  }

  if (is.data.frame(x)) {
    x[v_targets, ] = new_value
  }

  if (is.matrix(x)) {
    x[v_targets] = new_value
  }

  if (is.list(x) & !is.data.frame(x)) {
    #apply to each element of the list
    x = lapply(x, function(element) {
      conditional_change(element, func = func, new_value = new_value)
    })
  }

  return(x)
}



