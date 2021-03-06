### Functions for easy subsetting

#the idea is to combine functional programming with subsetting for cases where one needs conditional subsetting and plyr's mapvalues cannot easily be used



#' Change values conditionally
#'
#' Change values based on a condition given as a function.
#' @param x (vector, data.frame, matrix or list) An object whose values to change.
#' @param func (function) A function that returns a boolean.
#' @param func_str (character scalar) A string to make a function from using math_to_function().
#' @param new_value (scalar vector) A value to use in the cases.
#' @param handle_NA (boolean) Whether to handle missing values.
#' @export
conditional_change = function(x, func = NULL, func_str = NULL, new_value, handle_NA = T) {

  #checks
  x
  new_value
  if (is.null(func) && is.null(func_str)) stop("Both `func`` and `func_str` are missing. You must supply a function!")
  if (!any(sapply(list(is.vector, is.data.frame, is.matrix, is.list), function(y) {
    y(x)
  }))) stop("x was not a vector, data frame, matrix or list. Other objects are not supported!")
  if (!handle_NA) {
    if (any(is.na(x))) stop("Missing values present and not handled!")
  }

  #shorthands for functions
  if (!is.null(func_str)) {
    if (func_str == "<0") func = is_negative
    if (func_str == ">0") func = is_positive
    if (func_str == "=0") func = is_zero
    if (func_str == "==0") func = is_zero
  }

  #make function
  if (is.null(func)) func = math_to_function(func_str)

  #conditional

  trial = try({
    v_targets = func(x)

    #handle NA?
    if (handle_NA) v_targets = plyr::mapvalues(v_targets, NA, F, warn_missing = F) #map NA to F
  }, silent = T)
  #fails for lists


  #change by class
  if (is.vector(x) & !is.list(x)) { #simple vector
    x[v_targets] = new_value
  }

  if (is.data.frame(x)) {
    x[v_targets, ] = new_value
  }

  if (is.matrix(x)) {
    x[v_targets] = new_value
  }

  if (is.list(x) & !is.data.frame(x)) {
    #apply to each element of the list
    x = lapply(x, function(element) {
      conditional_change(element, func = func, new_value = new_value)
    })
  }

  return(x)
}



#' Extract non-numerical variables
#'
#' Extract the non-numerical variables from a data.frame or matrix.
#' @param data (data.frame or matrix) The data.
#' @return Returns the subset of the data while keeping the type (using drop = FALSE).
#' @export
#' @examples
#' extract_nonnum_vars(iris)
extract_nonnum_vars = function(data) {
  v_numerical = !purrr::map_lgl(data, is.numeric)
  data[, v_numerical, drop = FALSE]
}


#' Subset object using Python's negative indexing
#'
#' Subset object using Python's negative indexing, that is, by counting indices backwards.
#' @param x (vector, data.frame or matrix) Object to subset.
#' @param margin_1 (integer vector) Indices for the first (vertical) margin.
#' @param margin_2 (integer vector) Indices for the second (horizontal) margin.
#' @param drop (logical scalar) Whether to convert to a vector if there is only 1 column. Default=FALSE.
#' @return Returns the subset of the object.
#' @export
#' @examples
#' extract_last(iris, 1) # last row
#' extract_last(iris, , 1) # last column
#' extract_last(iris, 5:1, 2:1) #last 5 elements rows and last 2 columns
extract_last = function(x, margin_1 = NULL, margin_2 = NULL, drop = FALSE) {
  #check types
  if (!(is.vector(x) || is.matrix(x) || is.data.frame(x))) stop("x was an unsupported type (not a vector, matrix or data.frame)!")

  #vector
  if (is.vector(x)) return(rev(x)[margin_1])

  #get dims
  x_dims = dim(x)

  #make indices
  if (is.null(margin_1)) {
    margin_1 = 1:x_dims[1]
  } else {
    margin_1 = (x_dims[1] + 1) - margin_1
  }
  if (is.null(margin_2)) {
    margin_2 = 1:x_dims[2]
  } else {
    margin_2 = (x_dims[2] + 1) - margin_2
  }

  #subset
  return(x[margin_1, margin_2, drop = drop])
}


#' Extract numerical variables
#'
#' Extract the numerical variables from a data.frame or matrix.
#' @param data (data.frame or matrix) The data.
#' @return Returns the subset of the data while keeping the type (using drop = FALSE).
#' @export
#' @examples
#' extract_num_vars(iris)
extract_num_vars = function(data) {
  v_numerical = sapply(data, is.numeric)
  data[, v_numerical, drop = FALSE]
}

