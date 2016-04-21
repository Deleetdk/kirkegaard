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
#' @param str (chr scalar) A string of a half math conditional
#' @param convert_equal (log scalar) Converts = to ==. Default=T.
#' @param silent_try (log scalar) Whether to use a silent trial. Default=T. Change to F to get potentially useful debugging information.
#' @param test_function (log scalar) Whether to test the function. This is done by trying on 0. Default=T.
#' @export
#' @examples
#' math_to_function("<0")(-1) #check if -1<0
#' math_to_function("=0")(1234) #check if 1235=0
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



#' Check if arguments are missing and raise error if they are
#'
#' Checks if arguments are missing and raises an error if they are. Put this in the beginning of your functions to check the input and give useful errors without writing checking code over and over again.
#' @param var_names (chr vector) Names of variables to check.
#' @param error_msg (chr scalar) A template of the error message to show.
#' @export
#' @examples
#' test_func = function(y) {
#' check_missing("y")
#' T
#' }
#' test_func(y = ) #throws error
#' test_func(y = 1) #returns true
check_missing = function(var_names, error_msg = "[VAR] was missing! Please supply the input and try again.") {

  #parent.frame as list
  pf = as.list(parent.frame())

  #check each if missing
  for (name in var_names) {
    #is it there at all?
    if (!name %in% names(pf)) {
      stop(name + " is not even found in the parent.frame! Check the variable names.", call. = F)
    }

    #check if missing
    if (is.list(pf[[name]])) return(invisible(NULL)) #evade bug?
    if (are_equal(pf[[name]], quote(expr = ))) {
      stop(str_replace(error_msg, pattern = "\\[VAR\\]", name), call. = F)
    }
  }

  #all fine
  return(invisible(NULL))
}



