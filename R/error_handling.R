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
#' @param expr (expr) An expression.
#' @param silent_try (lgl scalar) Whether to use a silent try.
#' @export
#' @examples
#' throws_error(log("")) #cannot take a logarithm of a string
#' throws_error(log(0)) #can take logarithm of 0
#' throws_error("log(0)") #works on strings too
#' throws_error("log(NULL)") #works on strings too
throws_error = function(expr, silent_try = T) {

  #chr?
  #try to check for chr, if true, it is a chr, if false, it MAY be an error
  try_chr = trial = try({is.character(expr)}, silent = T)

  #chr?
  if (isTRUE(try_chr)) {
    #try as string
    trial = try({
      eval(parse(text = expr), envir = parent.frame())
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


#' Try and browse on error
#'
#' Opens the browser in the calling envirionment so you can see what went wrong. Useful to putting inside loops and only opening browser on the iterations that cause errors.
#' @export
try_browse = function(expr) {
  #try
  .trial = try({
    y = eval(substitute(expr), parent.frame())
  })

  #catch
  if (inherits(.trial, "try-error")) eval(quote(browser()), parent.frame(n = 1))

  y
}


