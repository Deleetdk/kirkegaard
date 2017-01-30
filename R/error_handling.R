### Functions for error handling

#' Is an object a try-error?
#'
#' Checks whether an object is a try-error. Returns a boolean.
#' @param x (an object) An object to check.
#' @export
is_error = function(x) inherits(x, "try-error")


#' Does call return an error?
#'
#' Parses a string as a call and determines whether it returns an error or not. Returns a boolean.
#' @param x (expr) Expression.
#' @param silent_try (lgl scalar) Whether to use a silent try (default true).
#' @export
#' @examples
#' throws_error(log("")) #cannot take a logarithm of a string
#' throws_error(log(0)) #can take logarithm of 0
#' throws_error("!!!!/83") #can test syntax errors if calls are strings
#' throws_error("!!!!/83") #can test syntax errors if calls are strings
throws_error = function(expr, silent_try = T) {

  #quote
  qexpr = quote(expr)

  #chr?
  if (is.character(qexpr)) {
    #try as string
    trial = try({
      eval(parse(qexpr), envir = parent.frame())
    }, silent = silent_try)
  } else {
    #try as expression
    trial = try({
      eval(substitute(expr), envir = parent.frame())
    }, silent = silent_try)
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


#' Browse on error
#'
#' Browse on error. Calls browse in the parent.frame so that one can see the context the for error.
#' @param expr (expr) Any expression that might cause an error.
#' @export
#' @examples
#' try_browse({log("p")})
try_browse = function(expr) {
  #try
  .trial = try({
    y = eval(substitute(expr), parent.frame())
  })

  #catch
  if (inherits(.trial, "try-error")) eval(quote(browser()), parent.frame(n = 1))

  y
}


#' Browse on condition
#'
#' Browse on condition. Calls browse in the parent.frame so that one can see the context. Made for easier debugging. Pipe-friendly: returns output if condition is false.
#' @param expr (expr) An expression.
#' @param condition (expr) A condition to evaluate. y. is the output of expr.
#' @export
#' @examples
#' browse_if(1+1, .y == 2)
browse_if = function(expr, condition) {
  #evaluate in parent.frame
  .y = eval(substitute(expr), parent.frame())

  #test condition
  .cond = eval(substitute(condition))

  #browse?
  if (.cond) eval(quote(browser()), parent.frame(n = 1))

  #return output
  invisible(.y)
}

#' Browse if equal to
#'
#' Browse on equals condition. Calls browse in the parent.frame so that one can see the context. Made for easier debugging. Pipe-friendly: returns output if condition is false. Wrapper for browse_if.
#' @param expr (expr) An expression.
#' @param equal_to (any object) Any object to test for output equality.
#' @export
#' @examples
#' browse_if(1+1, 2)
browse_if_equals = function(expr, equal_to) {
  #evaluate in parent.frame
  .y = eval(substitute(expr), parent.frame())

  #test condition
  .cond = kirkegaard::are_equal(.y, equal_to)

  #browse?
  if (.cond) eval(quote(browser()), parent.frame(n = 1))

  #return output
  invisible(.y)
}
