### Functions for easy subsetting

#the idea is to combine functional programming with subsetting for cases where one needs conditional subsetting and plyr's mapvalues cannot easily be used



#' Change values conditionally
#'
#' Change values based on a condition given as a function.
#' @param x (vector, data.frame, matrix or list) An object whose values to change.
#' @param func (function) A function that returns a boolean.
#' @param func_str (character scalar) A string to make a function from using math_to_function().
#' @param new_value (scalar vector) A value to use in the cases.
#' @param handle_NA (boolean) Whether to handle missing values. Default=T.
#' @keywords conditional, change, value
#' @export
#' @examples
#' conditional_change()
conditional_change = function(x, func, func_str, new_value, handle_NA = T) {
  #libs
  library(plyr)

  #checks
  if (missing("x")) stop("x is missing!")
  if (missing("func") & missing("func_str")) stop("Both func and func_str are missing. You must supply a function!")
  if (missing("new_value")) stop("new_value is missing. You must supply a replacement value!")
  if (!any(sapply(list(is.vector, is.data.frame, is.matrix, is.list), function(y) {
    y(x)
  }))) stop("x was not a vector, data.frame, matrix or list. Other objects are not supported!")
  if (!handle_NA) {
    if (any(is.na(x))) stop("Missing values present!")
  }

  #shorthands for functions
  if (!missing("func_str")) {
    if (func_str == "<0") func = is_negative
    if (func_str == ">0") func = is_positive
    if (func_str == "=0") func = is_zero
    if (func_str == "==0") func = is_zero
  }

  #make function
  if (missing("func")) func = math_to_function(func_str)

  #conditional

  trial = try({
    v_targets = func(x)

    #handle NA?
    if (handle_NA) v_targets = mapvalues(v_targets, NA, F, warn_missing = F) #map NA to F
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


#' Filter data by missing values per row.
#'
#' Counts the number of missing values per row and then keeps rows that have at most a chosen number of missing values.
#' @param data (data.frame or something coersible to a data.frame) The data.
#' @param missing (whole number scalar) The maximum number of missing values in cases. Defaults to 0 (keep only cases with no missing values).
#' @keywords missing values, subset
#' @export
#' @examples
#' df = data.frame(1:10, letters[1:10])
#' df = df_addNA(df)
#' filter_by_missing_values(df)
filter_by_missing_values = function(data, missing = 0) {
  #initial
  if (!is_whole_number(missing)) stop("missing must be a whole number!")
  data = as.data.frame(data)

  #keep cases with that number of missing datapoints or fewer
  data = data[miss_case(data) <= missing, ]
  return(data)
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


#' Extract non-numerical variables
#'
#' Extract the non-numerical variables from a data.frame or matrix.
#' @param data (data.frame or matrix) The data.
#' @return Returns the subset of the data while keeping the type (using drop = FALSE).
#' @export
#' @examples
#' extract_nonnum_vars(iris)
extract_nonnum_vars = function(data) {
  v_numerical = !sapply(data, is.numeric)
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
extract_last = function(x, margin_1, margin_2, drop = FALSE) {
  #check types
  if (!(is.vector(x) || is.matrix(x) || is.data.frame(x))) stop("x was an unsupported type (not a vector, matrix or data.frame)!")

  #vector
  if (is.vector(x)) return(rev(x)[margin_1])

  #get dims
  x_dims = dim(x)

  #make indices
  if (missing("margin_1")) {
    margin_1 = 1:x_dims[1]
  } else {
    margin_1 = (x_dims[1] + 1) - margin_1
  }
  if (missing("margin_2")) {
    margin_2 = 1:x_dims[2]
  } else {
    margin_2 = (x_dims[2] + 1) - margin_2
  }

  #subset
  return(x[margin_1, margin_2, drop = drop])
}


#' Subset by pattern
#'
#' Subset a data.frame or matrix by a pattern in the column names. A simple wrapper using str_detect().
#'
#' Subsets using [] and with drop=FALSE.
#' @param data (data.frame or matrix) Object to subset.
#' @param pattern (character scalar) A regex pattern.
#' @param inverse (logical scalar) Whther to keep the non-matches instead.
#' @return Returns the subset of the object.
#' @export
#' @examples
#' subset_by_pattern(iris, "Length") # length columns
#' subset_by_pattern(iris, "Length", T) # non-length columns
subset_by_pattern = function(data, pattern, inverse = FALSE) {
  library(stringr)

  if (!inverse) return(data[, str_detect(colnames(data), as.character(pattern)), drop = FALSE])
  if (inverse) return(data[, !str_detect(colnames(data), as.character(pattern)), drop = FALSE])
}
