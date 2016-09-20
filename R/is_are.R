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
#' Tests whether all elements of a simple vector are the same.
#' @param x (num vector) A numericor or character vector.
#' @export
#' @examples
#' all_the_same(rep(1, 100))
#' all_the_same(rnorm(100))
#' all_the_same(letters[rep(1, 10)])
#' all_the_same(letters[sample(1:10, size = 10)])
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
  test = all.equal(target = x, current = y, ...)
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


#' Check whether object is the right class, size and type
#'
#' Checks both the class, size and type of an object. Returns a logical or an error if false.
#'
#' Vectors are treated as 1-dimensional.
#' @param x (any object) The object to test.
#' @param class (chr vector) The accepted classes. This is checked with \code{\link{is}}.
#' @param size (num vector) The accepted lengths. This is checked with \code{\link{get_dims}}.
#' @param type (chr vector) The accepted types. This is checked with \code{\link{typeof}}.
#' @param error_on_false (log scalar) Whether to throw an error instead of returning false (default false).
#' @return Logical scalar or error.
#' @export
#' @examples
#' is_(iris, class = "data.frame") #check for one class
#' is_(iris, class = c("data.frame", "logical", "matrix")) #can check for multiple classes
#' is_(iris, class = "data.frame", size = c(150, 5)) #check for one class and size
#' is_(iris, size = c(150, 5)) #check for size
#' is_(iris, size = 1) #check for wrong size
#' is_(iris, type = "list") #check for type
#' is_(iris, type = "factor") #check for wrong type
#' is_(iris, class = "list", error_on_false = T) #check for one class, error
is_ = function(x, class, size, type, error_on_false = F) {

  #check x
  check_missing("x")

  #init
  v_type_check = v_size_check = v_class_check = T

  #check class
  if (!missing("class")) {
    v_class_check = any(sapply(X = class, FUN = function(class_i) {
      is(x, class2 = class_i)
    }))
  }

  #check size
  if (!missing("size")) {
    v_size_check = all(get_dims(x) == size)
  }

  #check type
  if (!missing("type")) {
    v_type_check = any(sapply(X = type, FUN = function(type_i) {
      typeof(x) == type_i #check type
    }))
  }

  #error?
  if (error_on_false) {
    if (!v_size_check) stop("Object " + deparse(substitute(x)) + " was not of the right size!", call. = F)
    if (!v_type_check) stop("Object " + deparse(substitute(x)) + " was not of the right type!", call. = F)
    if (!v_class_check) stop("Object " + deparse(substitute(x)) + " was not of the right class!", call. = F)
  }

  v_size_check & v_type_check & v_class_check
}


#' Check whether object is in a list/vector
#'
#' Checks whether an object is in a list/vector. If not, returns an informative error.
#' @param x (any object) The object to test.
#' @param list (list) The list of accepted values
#' @return Logical scalar or error.
#' @export
#' @examples
#' check_if_in("a", letters[1:10])
#' check_if_in("a", letters[2])
check_if_in = function(x, list) {
  #check
  if (! x %in% list) {
    stop(deparse(substitute(x)) + " was not among the accepted values!", call. = F)
  }

  #all fine
  return(invisible(NULL))
}


#' Check whether all elements of a list are identical
#'
#' Check whether all elements of a list are identical.
#' @param list (list) The list to check.
#' @return TRUE if all elements are identical, FALSE if not.
#' @export
#' @examples
#' #test lists
#' l_testlist_ok = list(1:3, 1:3, 1:3, 1:3, 1:3, 1:3)
#' l_testlist_bad = list(1:3, 1:3, 1:4, 1:3, 1:3, 1:3)
#' all_elements_the_same(l_testlist_ok) #TRUE
#' all_elements_the_same(l_testlist_bad) #FALSE
#' all_elements_the_same(list()) #TRUE if list is empty
all_elements_the_same = function(list) {
  #double loop solution
  for (i in seq_along(list)) {
    for (j in seq_along(list)) {
      #skip if comparing to self or if comparison already done
      if (i >= j) next

      #check
      if (!identical(list[[i]], list[[j]])) return(F)
    }
  }
  T
}
