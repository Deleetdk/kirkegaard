#' Find percentage of numbers that are above given thresholds.
#'
#' Takes a numeric vector and a numeric vector of thresholds. Returns the percent of numbers in the first above each of the numbers in the second.
#' @param x (num vector) A vector of numbers.
#' @param cutoffs (num vector) A vector of thresholds. Default=(.30, .50)
#' @param digits (num scalar) The number of digits to round output to. Default=2.
#' @param below (log scalar) Whether to count values below the cutoff (default false).
#' @param inclusive (log scalar) Whether to include values at the cutoff (default true).
#' @export
#' @examples
#' percent_cutoff(iris$Sepal.Length, cutoffs = 4:8)
#' percent_cutoff(iris$Sepal.Length, cutoffs = 4:8, below = T) #reverse cutoff
#' percent_cutoff(c(1:3, NA, NaN, 4:6), cutoffs = 3) #ignores NA/NaN
percent_cutoff = function(x, cutoffs = c(.30, .50), digits = 2, below = F, inclusive = T) {
  library(magrittr)

  #convert
  x = as.numeric(x)
  cutoffs = as.numeric(cutoffs)

  v_res = numeric()
  for (idx in seq_along(cutoffs)) {
    #count
    if (!below & !inclusive) {
      v_res[idx] = (x > cutoffs[idx]) %>%
        (function(x) {
          sum(x, na.rm = T) / (na.omit(x) %>% length)
        })
    }
    if (!below & inclusive) {
      v_res[idx] = (x >= cutoffs[idx]) %>%
        (function(x) {
          sum(x, na.rm = T) / (na.omit(x) %>% length)
        })
    }
    if (below & !inclusive) {
      v_res[idx] = (x < cutoffs[idx]) %>%
        (function(x) {
          sum(x, na.rm = T) / (na.omit(x) %>% length)
        })
    }
    if (below & inclusive) {
      v_res[idx] = (x <= cutoffs[idx]) %>%
        (function(x) {
          sum(x, na.rm = T) / (na.omit(x) %>% length)
        })
    }
  }

  #names
  names(v_res) = cutoffs

  #round
  if (!missing("digits")) v_res = round(v_res, digits = digits)

  return(v_res)
}


##Function to find the cell or index with the highest value
# Works on matrices and data.frames too
#Credit: http://r.789695.n4.nabble.com/returning-the-largest-element-in-an-array-matrix-td795214.html
#' Generalized which.max(). Returns the index of the cell or number with the highest value.
#'
#' A generalization of which.max() that works on matrices and data.frames too.
#' @param x a numeric vector, matrix or data.frame.
#' @keywords max, index
#' @export
#' @examples
#' m = matrix(runif(9), nrow=3)
#' m
#' which_max2(m)
which_max2 = function(x) {
  #check class
  if (!(is.numeric(x) | is.data.frame(x) | is.matrix(x) )) {
    stop(paste0("x was not numeric, data frame or matrix, but instead ", class(x)))
  }
  if (is.matrix(x)) {
    wm = which.max(x) #index of max
    return(c(row(x)[wm], col(x)[wm])) #return indices of data.frame / matrix
  }
  if (is.data.frame(x)) {
    x = as.matrix(x) #convert to matrix
    wm = which.max(x) #index of max
    return(c(row(x)[wm], col(x)[wm])) #return indices of data.frame / matrix
  }
  return(which.max(x)) #index of max
}



#' Generalized which.min(). Returns the index of the cell or number with the lowest value.
#'
#' A generalization of which.min() that works on matrices and data.frames too.
#' @param x a numeric vector, matrix or data.frame.
#' @keywords min, index
#' @export
#' @examples
#' m = matrix(runif(9), nrow=3)
#' m
#' which_min2(m)
which_min2 = function(x) {
  #check class
  if (!(is.numeric(x) | is.data.frame(x) | is.matrix(x) )) {
    stop(paste0("x was not numeric, data frame or matrix, but instead ",class(x)))
  }
  if (is.matrix(x)) {
    wm = which.min(x) #index of max
    return(c(row(x)[wm], col(x)[wm])) #return indices of data.frame / matrix
  }
  if (is.data.frame(x)) {
    x = as.matrix(x) #convert to matrix
    wm = which.min(x) #index of max
    return(c(row(x)[wm], col(x)[wm])) #return indices of data.frame / matrix
  }
  return(which.min(x)) #index of max
}


#' Is vector a whole number?
#'
#' Tests via x = is.integer(x).
#' @param x (numeric vector) A numeric vector.
#' @keywords vector, whole number, integer
#' @export
#' @examples
#' is_whole_number(1L)
#' is.integer(1)
#' is_whole_number(1)
is_whole_number = function(x) {
  x == as.integer(x)
}


#' Are values negative?
#'
#' A wrapper for "<0", returns a boolean. Vectorized.
#' @param x (any object compatible with <) Some values to test.
#' @keywords negative, value, boolean
#' @export
#' @examples
#' is_negative()
is_negative = function(x) {
  vapply(x, function(y) {
    y < 0
  }, FUN.VALUE = logical(1))
}


#' Are valyes positive?
#'
#' A wrapper for ">0", returns a boolean. Vectorized.
#' @param x (any object compatible with <) Some values to test.
#' @keywords positive, value, boolean
#' @export
#' @examples
#' is_positive()
is_positive = function(x) {
  vapply(x, function(y) {
    y > 0
  }, FUN.VALUE = logical(1))
}



#' Are values zero?
#'
#' A wrapper for "==0", returns a boolean. Vectorized.
#' @param x (any object compatible with <) Some values to test.
#' @keywords zero, value, boolean
#' @export
#' @examples
#' is_zero()
is_zero = function(x) {
  vapply(x, function(y) {
    y == 0
  }, FUN.VALUE = logical(1))
}


#' Check whether numbers are between two other numbers.
#'
#' Returns a logical vector of the same length as x. Can use inclusive or exclusive limits.
#' @param x (numeric vector) A vector of values to test.
#' @param a (numeric scalar) The lower limit.
#' @param b (numeric scalar) The upper limit.
#' @param include_lower (boolean) Whether to include the lower limit itself. Defaults to TRUE.
#' @param include_upper (boolean) Whether to include the upper limit itself. Defaults to TRUE.
#' @keywords limit, between
#' @export
#' @examples
#' is_between(1:10, 3, 6)
is_between = function(x, a, b, include_lower = TRUE, include_upper = TRUE) {
  vapply(x, FUN = function(x) {
    if (include_lower && include_upper) return(x >= a && x <= b)
    if (include_lower && !include_upper) return(x >= a && x < b)
    if (!include_lower && include_upper) return(x > a && x <= b)
    if (!include_lower && !include_upper) return(x > a && x < b)
  }, FUN.VALUE = logical(1))
}


#' Winsorise numbers
#'
#' Winsorizes the numbers, that is, reduces any number above a given limit to that number and likewise for a lower limit.
#' @param x (numeric vector) A vector of values.
#' @param lower (numeric scalar) The lower limit.
#' @param upper (numeric scalar) The upper limit.
#' @export
#' @examples
#' winsorise(seq(-3, 13), 10, 0)
winsorise = function(x, upper, lower) {
  x[x > upper] = upper
  x[x < lower] = lower
  x
}


#' Reverse a scale
#'
#' Reverses a scale so that the largest number becomes the smallest, next largest becomes next smallest, etc. Can be given scale limits but will determine them empirically if not given.
#' @param x (numeric vector) A vector of values.
#' @param .min (numeric scalar) The scale maximum.
#' @param .max (numeric scalar) The scale minimum.
#' @export
#' @examples
#' x = c(2, 1, 3, 0, -1) #some scale ranging from -1 to 3
#' reverse_scale(x) #reverse scores: 3 becomes -3
#' x == reverse_scale(reverse_scale(x)) #reverse twice and get back
#' reverse_scale(x, .min = -3, .max = 10) #supply other min/max
reverse_scale = function(x, .min = min(x), .max = max(x)) {
  (.max - x) + .min
}


