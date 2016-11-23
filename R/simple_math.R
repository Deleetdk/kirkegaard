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
#' @export
#' @examples
#' is_whole_number(1L)
#' is.integer(1)
#' is_whole_number(1)
is_whole_number = function(x, scalar = F) {
  if (scalar) {
    return((x == as.integer(x)) && is_scalar(x))
  }
  x == as.integer(x)
}


#' Are values negative?
#'
#' A wrapper for "< 0".
#' @param x (numeric vector) Some values to test.
#' @export
#' @examples
#' is_negative(1)
#' is_negative(0)
#' is_negative(-1)
is_negative = function(x) {
  x < 0
}


#' Are values positive?
#'
#' A wrapper for "> 0".
#' @param x (numeric vector) Some values to test.
#' @export
#' @examples
#' is_positive(1)
#' is_positive(0)
#' is_positive(-1)
is_positive = function(x) {
  x > 0
}



#' Are values zero?
#'
#' A wrapper for "== 0".
#' @param x (numeric vector) Some values to test.
#' @export
#' @examples
#' is_zero(0)
#' is_zero(1)
is_zero = function(x) {
  (x == 0)
}


#' Check whether numbers are between two other numbers.
#'
#' Returns a logical vector of the same length as x. Can use inclusive or exclusive limits.
#' @param x (numeric vector) A vector of values to test.
#' @param a (numeric scalar) The lower limit.
#' @param b (numeric scalar) The upper limit.
#' @param include_lower (boolean) Whether to include the lower limit itself. Defaults to TRUE.
#' @param include_upper (boolean) Whether to include the upper limit itself. Defaults to TRUE.
#' @export
#' @examples
#' is_between(1:10, 3, 6)
is_between = function(x, a, b, include_lower = TRUE, include_upper = TRUE) {
  if (include_lower && include_upper) return(x >= a & x <= b)
  if (include_lower && !include_upper) return(x >= a & x < b)
  if (!include_lower && include_upper) return(x > a & x <= b)
  if (!include_lower && !include_upper) return(x > a & x < b)
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
#' @param .min (numeric scalar) The scale maximum. Defaults to the observed minimum.
#' @param .max (numeric scalar) The scale minimum. Defaults to the observed maximum.
#' @export
#' @examples
#' x = c(2, 1, 3, 0, -1) #some scale ranging from -1 to 3
#' reverse_scale(x) #reverse scores: 3 becomes -3
#' x == reverse_scale(reverse_scale(x)) #reverse twice and get back
#' reverse_scale(x, .min = -3, .max = 10) #supply other min/max
reverse_scale = function(x, .min = min(x, na.rm=T), .max = max(x, na.rm=T)) {
  (.max - x) + .min
}

#' Proportion true
#'
#' Given a suitable input (logical vector/matrix/array, numeric/integer vector/matrix/array), find the proportion of true values. Automatically removes NAs.
#' @param x (numeric vector) A vector of values.
#' @export
#' @examples
#' #sample some logical data
#' x = sample(c(T, F), size = 100, replace = T)
#' proportion_true(x)
#' #sample some logical data with NAs
#' x = sample(c(T, F, NA), size = 100, replace = T)
#' proportion_true(x) #no errors or NA output
#' #converts input to logical if possible
#' x = sample(c(1, 0), size = 100, replace = T)
#' proportion_true(x)
#' #throws errors if it gets a nonsensical input
#' "str" %>% proportion_true() #character
#' data.frame() %>% proportion_true() #data.frame
#' list() %>% proportion_true()
proportion_true = function(x) {
  if (is_(x, class = c("character", "list", "data.frame"))) stop("Cannot coerce data.frames, lists or characters into logical!")
  x = as.logical(x)
  #remoev NA
  x = na.omit(x)
  sum(x) / length(x)
}


#' Rescale numbers
#'
#' Rescales numbers from one scale to another. By default, assumes that the old scale has min and max values represented in the vector, but this can be overwritten.
#' @param x (num vectr) A vector of values.
#' @param new_min (num sclr) The new scale minimum.
#' @param new_max (num sclr) The new scale maximum.
#' @param old_min (num sclr) The old scale minimum. Default: min(x).
#' @param old_max (num sclr) The old scale maximum. Default: max(x).
#' @export
#' @examples
#' rescale(1:10, new_min = 0, new_max = 1) #converts to 0, ..., 1
#' rescale(1:10, new_min = 0, new_max = 5) #converts to 0, ..., 5
#' rescale(rnorm(10), new_min = 1, new_max = 100) %>% sort #converts to 1, ..., 1
#' rescale(c(.1, .5, 1), new_min = 10, new_max = 20, old_min = 0, old_max = 1) #assume old numbers belong to scale 0-1, rescale to 10-20 scale, i.e. 11, 15, 20
rescale = function(x, new_min, new_max, old_min = min(x), old_max = max(x)) {
  #check input
  is_(x, class = "numeric", error_on_false = T)
  is_(new_min, class = "numeric", size = 1, error_on_false = T)
  is_(new_max, class = "numeric", size = 1, error_on_false = T)
  is_(old_min, class = "numeric", size = 1, error_on_false = T)
  is_(old_min, class = "numeric", size = 1, error_on_false = T)

  #check if impossible old_min or old_max given
  if (min(x) < old_min) stop(sprintf("The min value in x is smaller than the minimum possible value given!: old_min = %f vs. min(x) = %f", old_min, min(x)))
  if (max(x) > old_max) stop(sprintf("The max value in x is larger than the maximum possible value given!: old_max = %f vs. max(x) = %f", old_max, max(x)))

  #calculate ranges
  old_range = old_max - old_min
  new_range = new_max - new_min

  #subtract old min
  x = x - old_min

  #divide by old_range; convert to proportion of old range
  x = x / old_range

  #multiply by new range; stretch to new range
  x = x * new_range

  #add new min
  x = x + new_min

  #return
  x
}


#' Averages
#'
#' Calculates a number of different types of averages for a vector.
#' @param x (num vectr) A vector of values.
#' @param trim (num vectr) Used for trimmed means. Default = .10.
#' @param types (chr vectr) Which types of averages to calculate?
#' @export
#' @examples
#' #simualte some data
#' set.seed(1)
#' x = sample(1:10, replace = T, size = 1000, prob = seq(.1, .5, length.out = 10))
#' #look at it
#' GG_denhist(x, vline = NULL)
#' #calcualte averages
#' averages(x)
#' #normal data
#' set.seed(1)
#' averages(rnorm(1000)) #some functions give warnings when there are negative numbers
averages = function(x,
                    trim = .1,
                    types = c("arithmetic", "geometric", "harmonic", "mode", "median", "trimmed", "midrange")) {

  #check input
  is_(x, class = "numeric", error_on_false = T)
  is_(trim, class = "numeric", size = 1, error_on_false = T)
  if (!is_between(trim, 0, .5)) stop("trim must be between 0.0 and 0.5!")
  is_(types, class = "character", error_on_false = T)

  #Mode func
  Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
  }

  #calculate
  res = numeric()

  if ("arithmetic" %in% types) res = c(res, "arithmetic" = mean(x, na.rm=T))
  if ("geometric" %in% types) res = c(res, "geometric" = psych::geometric.mean(x, na.rm=T))
  if ("harmonic" %in% types) res = c(res, "harmonic" = psych::harmonic.mean(x, na.rm=T))
  if ("mode" %in% types) res = c(res, "mode" = Mode(x))
  if ("median" %in% types) res = c(res, "median" = median(x, na.rm=T))
  if ("trimmed" %in% types) res = c(res, "trimmed" = mean(x, na.rm=T, trim = trim))
  if ("midrange" %in% types) res = c(res, "midrange" = ((max(x, na.rm=T) - min(x, na.rm=T)) / 2) + min(x, na.rm=T))

  #return
  res
}


#' Augment logarithmic values with intuitive additional breaks
#'
#' Adds intuitive middle values to a vector of values. Useful for when plotting data on logarithmnic plots.
#' @param x (num vectr) A vector of values.
#' @param factors (num vectr) Factors to use to create the new values. Default is 2.5 and 5.
#' @export
#' @examples
#' #standard log10 breaks
#' 10^(0:5)
#' #add extra
#' helper_breaks(10^(0:5))
helper_breaks = function(x, factors = c(2.5, 5)) {
  y = x
  for (fct in factors) {
    y = c(y, x * fct)
  }
  sort(y)
}

