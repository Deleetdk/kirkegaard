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



#' Fail conditionally
#'
#' Fail on not TRUE and give a custom error message.
#' @param x (any object) The object to test.
#' @param msg (chr) An error message.
#' @param extended (lgl) Whether to convert to logical.
#' @export fail_if stop_if
#' @aliases stop_if
#' @return Invisibly returns the input.
#' @examples
#' fail_if(T)
#' fail_if(1) #does not convert types
#' fail_if(1, extended = T)
fail_if = function(x, msg = "There was an error", extended = F) {
  #fail if true
  if (isTRUE(x)) stop(msg, call. = F)
  if (extended && x) stop(msg, call. = F)

  #return input
  invisible(x)
}

#useful synonym
stop_if = fail_if


#' Fail if input contains NA
#'
#' Inputs any object, checks if it contains NA. If yes, throws an error. If not, returns the input.
#' @param x (any object) The object to test.
#' @param msg (chr) An error message.
#' @export fail_if_NA stop_if_NA
#' @aliases stop_if_NA
#' @return Invisibly returns the input.
#' @examples
#' fail_if_NA(1:10) #no NAs
#' fail_if_NA(c(1:3, NA, 1:3)) #NAs
fail_if_NA = function(x, msg = "Input contained NA.") {
  #fail if any NA
  if (any(is.na(x))) stop(msg, call. = F)

  #return input
  invisible(x)
}

stop_if_NA = fail_if_NA


#' Browse on error
#'
#' Browse on error. Calls browse in the parent.frame so that one can see the context the for error.
#' @param expr (expr) Any expression that might cause an error.
#' @export
#' @return Invisibly returns result of evaluating the expression.
#' @examples
#' try_browse({log("p")})
try_browse = function(expr) {
  #try
  .trial = try({
    y = eval(substitute(expr), parent.frame())
  })

  #catch
  if (inherits(.trial, "try-error")) eval(quote(browser()), parent.frame(n = 1))

  invisible(y)
}


#' Browse if equal to
#'
#' Browse on equals condition. Calls browse in the parent.frame so that one can see the context. Made for easier debugging. Pipe-friendly: returns output if condition is false. Wrapper for browse_if.
#' @param x (object) Any object.
#' @param equal_to (any object) Any object to test for output equality.
#' @param check.names (lgl) Whether to check names.
#' @param check.attributes (lgl) Whether to check all attributes.
#' @export
#' @examples
#' browse_if(1+1, 2)
browse_if_equals = function(x, equal_to, check.names = T, check.attributes = T) {
  #test condition
  .cond = kirkegaard::are_equal(x, equal_to, check.names = check.names, check.attributes = check.attributes)

  #browse?
  if (.cond) eval(quote(browser()), parent.frame(n = 1))

  #return output
  invisible(.y)
}


#' Browse if equal to
#'
#' Browse on equals condition. Calls browse in the parent.frame so that one can see the context. Made for easier debugging. Pipe-friendly: returns output if condition is false. Wrapper for browse_if.
#' @param condition (lgl) A condition
#' @param extended (lgl) Whether to convert to logical.
#' @export
#' @examples
#' browse_if(T) #only
#' browse_if(F) #
#' browse_if(1) #does not automatically type convert
#' browse_if(1, extended = T) #can if you want it
browse_if = function(condition, extended = F) {
  #browse?
  if (isTRUE(condition)) eval(quote(browser()), parent.frame(n = 1))
  if (extended && as.logical(condition)) eval(quote(browser()), parent.frame(n = 1))

  #return output
  invisible(T)
}
