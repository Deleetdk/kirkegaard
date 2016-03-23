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


