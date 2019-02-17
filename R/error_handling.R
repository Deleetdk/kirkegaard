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


#' Try and fail with a default value
#'
#' @param expr (expression) An expression to execute.
#' @param else. (any) Value returned on error.
#' @param silent (lgl) Whether to silence the error messages.
#'
#' @return The output from the expression or the else value.
#' @export
#'
#' @examples
#' try_else(log(1))
#' try_else(log("abc"))
#' try_else(log("abc"), silent = F)
try_else = function(expr, else. = NULL, silent = T) {
  #execute in parent frame envir
  .trial = try({
    y = eval(substitute(expr), parent.frame())
  }, silent = silent)

  #error?
  if (inherits(.trial, "try-error")) return(else.)

  #return as normal
  y
}

#' Retry while error
#'
#' Run an expression repeatedly until it doesn't produce an error or a max number of attempts has been reached.
#' @param expr An expression.
#' @param retry_interval (num) Time interval between retries in seconds.
#' @param silent (lgl) Whether to output dated errors while trying is ongoing.
#' @param max_tries (num) Max number of tries.
#' @param max_time (num) Max time to try, including run and sleep time.
#'
#' @return The output of expr.
#' @export
#'
#' @examples
#' #this one eventually succeeds
#' retry_while_error(log(unlist(sample(list("", 0), size = 1, prob = c(1, .01)))), retry_interval = 0, max_time = Inf, max_tries = Inf, silent = T)
#'
#' #note: these produce errors
#' retry_while_error(log(""), retry_interval = 0)
#' retry_while_error(log(""), retry_interval = 0, max_time = 1, max_tries = Inf)
retry_while_error = function(expr, retry_interval = 60, silent = F, max_tries = 10, max_time = Inf) {
  #begin error log
  tibble = data_frame(
    datetime = lubridate::as_datetime(character()),
    error = character()
  )

  #tries
  tries = 0

  #start of trying
  time_start = lubridate::now()

  #keep trying
  while (T) {
    #increment tries
    tries = tries + 1

    #try, silently
    trial = try({
      y = eval(substitute(expr), parent.frame())
    }, silent = T)

    #error?
    if (is_error(trial)) {
      #make error data frame
      this_error = tibble(
        datetime = lubridate::now(),
        error = trial[1]
      )

      #output if desired
      if (!silent) message(sprintf("%s -- %s", lubridate::now() %>% as.character(), trial[1]))

      #log error
      error_log = rbind(
        error_log,
        this_error
      )

      #max tries reached?
      if (tries == max_tries) {
        stop(sprintf("Stopped trying after %d tries", tries), call. = F)
      }

      #max time reached?
      time_tried = lubridate::now() - time_start
      if (time_tried > max_time) {
        stop(sprintf("Stopped trying after %d seconds", as.integer(time_tried)), call. = F)
      }

      #sleep
      Sys.sleep(retry_interval)

      #start over
      next
    }

    #break
    break
  }

  #attach error log as attr
  attr(trial, "error_log") = error_log

  #return output
  trial
}


