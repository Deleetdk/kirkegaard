#' Merge vectors.
#'
#' Merge data from two vectors, either by names or by location. The second is merged into the first.
#' @param v1 (vector) The first vector.
#' @param v2 (vector) The second vector.
#' @param byname (logical scalar) Whether to merge by name. Default = FALSE.
#' @param overwrite_NA (logical scalar) When using byname, whether to overwrite NAs. Default = FALSE.
#' @export
#' @examples
#' #without names
#' t1 = c(NA, 2, 3, NA)
#' t2 = c(1, NA, NA, 4)
#' merge_vectors(t1, t2)
#' #with names
#' t1_n = c(NA, 2, 3, NA);names(t1_n) = letters[1:4]
#' t3_n = c(1, NA, NA, 4, 5);names(t3_n) = letters[1:5]
#' merge_vectors(t1_n, t3_n, byname = T)
merge_vectors = function(v1, v2, byname = FALSE, overwrite_NA = FALSE) {
  #checks
  if (!is.vector(v1)) stop("v1 was not a vector!")
  if (!is.vector(v2)) stop("v2 was not a vector!")

  #vectors of NA
  v1_NA = is.na(v1)
  v2_NA = is.na(v2)

  #use v1 as base
  v3 = v1

  #byname
  if (byname) {
    #check names
    if (!(has_names(v1) && has_names(v2))) stop("when using byname, vectors must have names!")

    #overwrite?
    if (overwrite_NA) {
      v3[names(v2)] = v2
      return(v3)
    }

    #not overwrite
    v2 = na.omit(v2) #remove NAs
    v3[names(v2)] = v2
    return(v3)
  }

  #not byname
  if (!(length(v1) == length(v2))) stop("when not using byrow, lengths must be identical!")

  #not overwrite
  #because overwriting serves no purpose when not using names, it's just replacing the entire vector
  v3[!v2_NA] = v2[!v2_NA]
  v3
}


#' Discretize a variable
#'
#' A wrapper for base-r's cut() with some improved features.
#' @param x (numeric vector) A vector of datapoints to discretize.
#' @param breaks (integer scalar or numeric vector) The number of levels to create, or a numeric vector of values to use as cut-offs.
#' @param equal_range (logical scalar) Whether to use equal ranges. If false, will use equal sizes. Defaults to TRUE.
#' @param labels (character scalar) Which labels to use. By default, it will use the intervals. Can also be "midpoint", which uses the interval midpoints, "numbers" will use integers.
#' @param include_end (logical scalar) Whether to include datapoints equal to the last break value. Default=TRUE.
#' @param right (logical scalar) Whether to use intervals that are closed on the right. Default=TRUE.
#' @param ordered_factor (logical scalar) Should the result be an ordered factor? Default=FALSE.
#' @param ... (additional arguments) Any other arguments passed to cut().
#' @export
#' @examples
#' x = discretize(rnorm(100), 5)
#' hist(x)
#' y = discretize(rnorm(100), 5, equal_range = F)
#' hist(y)
discretize = function(x, breaks, equal_range=T, labels = "numbers", include_end = T, right = T, ordered_factor = F, ...) {
  #init
  midpoints = F

  #handle breaks
  if (length(breaks) == 1) {
    #equal sizes
    if (!equal_range) {
      breaks = quantile(x, probs = seq(0, 1, length.out = breaks+1))
    }
  }

  #handle labels
  if (labels == "intervals") {
    labels = NULL #the default for cut
  } else if (labels == "numbers") {
    labels = F
  } else if (labels == "midpoints") {
    labels = NULL #the default for cut, then fix afterwards
    midpoints = T
  } else if (!labels %in% c("intervals", "numbers", "midpoints")) {
    stop("Unrecognized labels value!")
  }

  #cut
  y = cut(x = x, breaks = breaks, labels = labels, include.lowest = include_end, right = right, ordered_result = ordered_factor, ...)

  #fix labels
  if (midpoints) {
    #some code here
    library(stringr)
    str_match_all(string = y, pattern = "[\\d\\.]+")
  }

  y
}


#' Calculate mean absolute difference for values in a vector
#'
#' Calculates all the pairwise absolute differences, then averages them.
#' @param x (numeric vector) A vector of values.
#' @param na.rm (logical scalar) Whether to ignore missing values. Default=TRUE.
#' @export
#' @examples
#' mean_abs_diff(iris[[1]])
mean_abs_diff = function(x, na.rm = T) {
  library(magrittr)
  dist(unlist(as.vector(x))) %>% as.vector() %>% mean(., na.rm = na.rm)
}


#' Exclude missing datapoints
#'
#' Exclude datapoints that are NA, NULL or NaN.
#' @param x (an interatable object) An object to subset based on missingness.
#' @param .NA (logical scalar) Whether to exclude NA (default TRUE).
#' @param .NULL (logical scalar) Whether to exclude NULL (default TRUE).
#' @param .NaN (logical scalar) Whether to exclude NaN (default TRUE).
#' @export
#' @examples
#' x = list(1, NA, 2, NULL, 3, NaN, 4, Inf)
#' exclude_missing(x)
#' exclude_missing(x, .NA = F)
#' exclude_missing(x, .NULL = F)
#' exclude_missing(x, .NaN = F)
#' exclude_missing(x, .Inf = F)
exclude_missing = function(x, .NA = T, .NULL. = T, .NaN = T, .Inf = T) {
  # browser()
  #NULL
  if (.NULL.) x = x[!sapply(x, is.null)]

  #NA
  if (.NA) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    if (is.infinite(y)) return(T)
    if (is.nan(y)) return(T)
    !is.na(y)
  })]

  #NaN
  if (.NaN) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    !is.nan(y)
  })]

  #Inf
  if (.Inf) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    if (is.na(y)) return(T)
    is.finite(y)
  })]

  x
}
