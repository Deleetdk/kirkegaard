### Functional functions
#functions for functional programming

#' Object to string
#'
#' Converts an object to a string using deparse and substitute.
#' @param x (an object) An object to convert to a string.
#' @keywords object, call, string, convert
#' @export
#' @examples
#' object_to_string()
object_to_string = function(x) {
  deparse(substitute(x))
}


#' Convert a half-complete math condition to a function.
#'
#' Takes a string like "<0" and outputs a function to test for that condition. Returns a boolean.
#' @param str (a character scalar) A string of a half math conditional
#' @param convert_equal (boolean) Converts = to ==. Default=T.
#' @param silent_try (boolean) Whether to use a silent trial. Default=T. Change to F to get potentially useful debugging information.
#' @param test_function (boolean) Whether to test the function. This is done by trying on 0. Default=T.
#' @keywords math, condition
#' @export
#' @examples
#' math_to_function()
math_to_function = function(str, convert_equal = T, silent_try = T, test_function = T) {
  library(stringr)

  #deak with =
  #here we assume that the user meant ==, i.e. is equal to rather than the assign operator
  if (convert_equal) {
    equal_count = str_count(str, "=")
    if (equal_count == 1) {
      str = str_replace(str, "=", "==")
    }
  }

  #fetch
  number = str_match(str, "\\d+")
  operator = str_match(str, "\\D+")

  #change string
  str2 = str_c("x ", operator, " ", number)

  #make function
  trial = try({
    func = function(x) {
      eval(parse(text=str2))
    }
  })
  if ("try-error" %in% class(trial)) stop("Could not create a function from the string!")

  #check function
  if (test_function) {
    trial = try(func(0), silent = silent_try)
    if ("try-error" %in% class(trial)) stop("Function returns an error!")
  }

  return(func)
}
