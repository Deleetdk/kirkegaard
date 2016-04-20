#' Is object a simple vector?
#'
#' A simple wrapper for is.vector and is.list. The normal is.vector function returns true for lists which is undesirable. Returns a boolean.
#' @param x (any object) An object to test.
#' @keywords vector, list
#' @export
#' @examples
#' l = list(1:10)
#' v = 1:10
#' is.vector(v)
#' is.vector(l)
#' is_simple_vector(v)
#' is_simple_vector(l)
is_simple_vector = function(x) {
  is.vector(x) & !is.list(x)
}


#' Are all elements of a vector the same?
#'
#' Tests whether all elements of a vector are the same. Uses the max/min method mentioned at .
#' @param x (expression) Some expression to run.
#' @keywords vector, same, identical, equal
#' @export
#' @examples
#' all_the_same(rep(1, 100))
#' all_the_same(rnorm(100))
all_the_same = function(x) {
  #for numeric data, a faster method
  if (is.numeric(x)) {
    return(max(x) == min(x))
  }
  #for non-numeric data, a slower method
  return(length(unique(x)) == 1)
}


#' Check numericalness by column.
#'
#' A simple wrapper for \code{vapply}.
#' @param x (something coercible to a data.frame) An object to test.
#' @return Returns a logical vector the same length as the number of columns in x.
#' @export
#' @examples
#' is_numeric_by_col(iris)
is_numeric_by_col = function(df) {
  df = as.data.frame(df)
  vapply(df, FUN = is.numeric, FUN.VALUE = logical(1))
}


#' Is object thoroughly numeric?
#'
#' A more advanced version of \code{\link{is.numeric}}. It wraps the base-r function, but allows for recursive checking inside lists and hence data.frames as well.
#' @param x (any object) An object to test.
#' @param recursive (logical scalar) Whether to use recursive checking. Default=TRUE.
#' @return Returns a logical scalar indicating whether the object is thoroughly numeric.
#' @export
#' @examples
#' is_numeric(iris)
#' is_numeric(iris[-5])
is_numeric = function(x, recursive = TRUE) {
  #vector
  if (is_simple_vector(x)) return(is.numeric(x))

  #array or matrix
  if (is.array(x) || is.matrix(x)) return(is.numeric(x))

  #factor
  if (is.factor(x)) return(FALSE)

  #recursive test?
  if (recursive) {
    #test all elements
    return(all(sapply(x, is_numeric)))
  }

  #otherwise assume FALSE
  FALSE
}


#' Are objects equal?
#'
#' A wrapper for \code{\link{all.equal}} that returns a logical scalar.
#' @param x (any object) The first object.
#' @param y (any object) The second object.
#' @param ... (other named parameters) Further parameters to pass to \code{all.equal}.
#' @return Returns a logical scalar indicating whether the objects are equal.
#' @export
#' @examples
#' are_equal(iris[1:4], iris[-5])
are_equal = function(x, y, ...) {
  test = all.equal(x, y, ...)
  if (is.logical(test)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Does object have names?
#'
#' A wrapper for names to test for existence of names.
#' @param x (any object) The object to test.
#' @return Logical scalar.
#' @export
#' @examples
#' has_names(iris)
#' has_names(1:4)
has_names = function(x) {
  !is.null(names(x))
}


#' Do the objects have the same lengths?
#'
#' Checks whether the nth dimension lengths match for the given objects.
#'
#' Vectors are treated as 1-dimensional.
#' @param ... (objects) The objects to test.
#' @param dimension (num scalar) The dimension to test (default 1).
#' @return Logical scalar.
#' @export
#' @examples
#' lengths_match(1:4, 5:8) #same lengths
#' lengths_match(iris, iris[1:2]) #same nrow
#' lengths_match(iris, iris[1:2], dimension = 2) #different ncol
lengths_match = function(..., dimension = 1) {
  library(magrittr)

  #try to get the nth dimension lengths
  trial = try({
    v_len = sapply(list(...), FUN = function(x) {
      get_dims(x)[dimension] %>% fail_if_NA()
    })
  }, silent = T)

  if (is_error(trial)) stop("Could not get dimension lengths! This probably means that at least one object does not have that many dimensions. E.g. if trying to get the second dimension from a vector.")

  #are they all the same?
  all_the_same(v_len)
}
