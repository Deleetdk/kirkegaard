#' Is object a simple vector?
#'
#' A simple wrapper for is.vector and is.list. The normal is.vector function returns true for lists which is undesirable. Returns a boolean.
#' @param x (any object) An object to test.
#' @export
#' @examples
#' l = list(1:10)
#' v = 1:10
#' is.vector(v)
#' is.vector(l)
#' is_simple_vector(v)
#' is_simple_vector(l)
is_simple_vector = function(x) {
  is.vector(x) && !is.list(x)
}


#' Is object an unordered factor?
#'
#' A simple wrapper for is.factor and is.ordered.
#' @param x (any object) An object to test.
#' @export
#' @examples
#' is_unordered_factor(factor(1:3))
#' is_unordered_factor(ordered(1:3))
is_unordered_factor = function(x) {
  is.factor(x) && !is.ordered(x)
}


#' Are all elements of a vector or list the same?
#'
#' Tests whether all elements of a simple vector are the same.
#' @param x A vector or list to test.
#' @export
#' @examples
#' all_the_same(rep(1, 100))
#' all_the_same(rnorm(100))
#' all_the_same(letters[rep(1, 10)])
#' all_the_same(letters[sample(1:10, size = 10)])
#' all_the_same(list(1:10, 1:10, 1:10))
#' all_the_same(list(1:10, 1:10, 1:11))
#' #careful with NAs
#' all_the_same(c(1, 1, NA))
all_the_same = function(x) {
  #for numeric data, a faster method
  if (is.numeric(x)) {
    return(max(x, na.rm = T) == min(x, na.rm = T) && !anyNA(x))
  }

  #for non-numeric data, a slower method
  #is not a list?
  if (!is.list(x)) {
    return(length(unique(x)) == 1)
  }

  #if its a list, compare each element to the first
  all(purrr::map_lgl(x, function(x_i) {
    are_equal(x_i, x[[1]])
  }))
}


#' Are all elements of a vector or list different?
#'
#' @param x A vector or list to test.
#'
#' @return A boolean scalar.
#' @export
#'
#' @examples
#' all_different(rep(1, 100))
#' all_different(c(1:5, 5))
#' all_different(c(NA, NA))
#' all_different(1:5)
#' all_different(list(1:2, 1:3, 1:4))
#' all_different(list(1:2, 1:3, 1:4, NA))
#' all_different(list(1:2, 1:3, 1:4, 1:4))
all_different = function(x) {
  length(unique(x)) == length(x)
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
is_numeric = function(x, recursive = T) {
  #vector
  if (is_simple_vector(x)) return(is.numeric(x))

  #array or matrix
  if (is.array(x) || is.matrix(x)) return(is.numeric(x))

  #factor
  if (is.factor(x)) return(F)

  #recursive test?
  if (recursive) {
    #test all elements
    return(all(purrr::map_lgl(x, is_numeric)))
  }

  #otherwise assume FALSE
  FALSE
}


#' Are objects equal?
#'
#' A wrapper for \code{\link{all.equal}} that returns a logical scalar.
#' @param x (any object) The first object.
#' @param y (any object) The second object.
#' @param check.names (lgl) Whether to check names (default yes).
#' @param check.attributes (lgl) Whether to check attributes (default yes).
#' @param ... (other named parameters) Further parameters to pass to \code{all.equal}.
#' @return Returns a logical scalar indicating whether the objects are equal.
#' @export
#' @examples
#' are_equal(iris[1:4], iris[-5])
#' a = letters[1:3]
#' b = letters[1:3]
#' attr(b, "test") = "value"
#' are_equal(a, b)
#' are_equal(a, b, check.attributes = F)
are_equal = function(x, y, check.names = T, check.attributes = T, ...) {
  test = base::all.equal(target = x,
                  current = y,
                  check.names = check.names,
                  check.attributes = check.attributes,
                  ...)
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
#' @param dimension (num scalar) The dimension to test.
#' @return Logical scalar.
#' @export
#' @examples
#' lengths_match(1:4, 5:8) #same lengths
#' lengths_match(iris, iris[1:2]) #same nrow
#' lengths_match(iris, iris[1:2], dimension = 2) #different ncol
#' lengths_match(iris, 1:3, dimension = 2) #incompatible objects
lengths_match = function(..., dimension = 1) {

  #try to get the nth dimension lengths
  trial = try({
    v_len = purrr::map_int(list(...), function(x) {
      get_dims(x) %>% `[`(dimension) %>% fail_if_NA()
    })
  }, silent = T)

  if (is_error(trial)) {
    warning("At least one object does not have that many dimensions. Check input.")
    return(F)
  }

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
#' !is_(iris, type = "factor") #check for wrong type
#' is_(iris, class = "list", error_on_false = T) #check for one class, error
#' is_(1:3, class = "numeric")
is_ = function(x, class = NULL, size = NULL, type = NULL, error_on_false = F) {

  #check x
  x

  #x name
  x_name = deparse(substitute(x))

  #init
  v_type_check = v_size_check = v_class_check = T

  #check class
  if (!is.null(class)) {
    v_class_check = any(purrr::map_lgl(class, function(class_i) {
      #special case of numeric
      if (class_i == "numeric") return(is.numeric(x))

      #else test normally
      inherits(x, what = class_i)
    }))
  }

  #check size
  if (!is.null(size)) {
    v_size_check = all(get_dims(x) == size)
  }

  #check type
  if (!is.null(type)) {
    v_type_check = any(purrr::map_lgl(type, function(type_i) {
      typeof(x) == type_i #check type
    }))
  }

  #error?
  if (error_on_false) {
    if (!v_size_check) stop(sprintf("Object %s was not of the right size!", x_name), call. = F)
    if (!v_type_check) stop(sprintf("Object %s was not of the right type!", x_name), call. = F)
    if (!v_class_check) stop(sprintf("Object %s was not of the right class!", x_name), call. = F)
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

#' Is scalar?
#'
#' Check whether an object is a scalar
#' @param x (obj) An object to check.
#' @param erro_on_null (lgl) Whether to throw an error on NULL, or treat as TRUE.
#' @return Logical scalar.
#' @export
#' @examples
#' is_scalar(1)
#' is_scalar(1:3)
#' is_scallar(NULL, error_on_null = F)
is_scalar = function(x, error_on_null = T) {
  #deal with NULL
  if (is.null(x)) {
    if (error_on_null) stop("x was NULL") else x = NA
  }
  #main
  is.atomic(x) && length(x) == 1
}

#' Is scalar NA?
#'
#' Check whether an object is a scalar NA
#' @param x (obj) An object to check.
#' @param erro_on_null (lgl) Whether to throw an error on NULL, or treat as TRUE.
#' @return Logical scalar.
#' @export
#' @examples
#' is_scalar_NA(NA)
#' is_scalar(1:3)
#' is_scalar_NA(c(1, NA))
#' is_scalar_NA(c(NA, 1))
is_scalar_NA = function(x, error_on_null = T) {
  #deal with NULL
  if (is.null(x)) {
    if (error_on_null) stop("x was NULL") else x = NA
  }
  #main
  is.atomic(x) && length(x) == 1 && is.na(x)
}

#' Is logical?
#'
#' Check whether an object is logical
#' @param x (obj) An object to check.
#' @param scalar (lgl) Only accept scalar.
#' @return Logical scalar.
#' @export
#' @examples
#' is_scalar_NA(NA)
#' is_scalar(1:3)
#' is_scalar_NA(c(1, NA))
#' is_scalar_NA(c(NA, 1))
is_logical = function(x, scalar = F) {
  is.logical(x) && is_scalar(x)
}


#' Equality infix operator
#'
#' @param x An object
#' @param y An object
#'
#' @return A boolean scalar
#' @export
#'
#' @examples
#' #simple
#' 1 %equals% 1
#' 1 %equals% 2
#' #works for vectors too
#' 1:3 %equals% 1:3
#' #compare with
#' 1:3 == 1:3
`%equals%` = function(x, y) {
  kirkegaard::are_equal(x, y)
}

