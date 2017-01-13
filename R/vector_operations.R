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
      breaks = quantile(x, probs = seq(0, 1, length.out = breaks+1), na.rm = T)
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
    stringr::str_match_all(string = y, pattern = "[\\d\\.]+")
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
  dist(unlist(as.vector(x))) %>% as.vector() %>% mean(., na.rm = na.rm)
}


#' Exclude missing datapoints
#'
#' Exclude datapoints that are NA, NULL or NaN.
#'
#' Does not remove NA etc. recursively. See the complex list example.
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
#' #complex list
#' x = list(1, NA, 2, NULL, 3, NaN, 4, Inf, 1:3, c(1, NA, 3))
#' exclude_missing(x) #does not remove NAs recursively
exclude_missing = function(x, .NA = T, .NULL. = T, .NaN = T, .Inf = T) {
  #check empty
  if (length(x) == 0) return(x)

  #NULL
  if (.NULL.) x = x[!sapply(x, is.null)]

  #check empty
  if (length(x) == 0) return(x)

  #NA
  if (.NA) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    if (length(y) != 1) return(T) #if length isn't 1, the below functions give errors
    if (is.infinite(y)) return(T)
    if (is.nan(y)) return(T)
    !is.na(y)
  })]

  #check empty
  if (length(x) == 0) return(x)

  #NaN
  if (.NaN) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    if (length(y) != 1) return(T) #if length isn't 1, the below functions give errors
    !is.nan(y)
  })]

  #check empty
  if (length(x) == 0) return(x)

  #Inf
  if (.Inf) x = x[sapply(x, function(y) {
    if (is.null(y)) return(T)
    if (!is_simple_vector(y)) return(T) #the functions below fail on list objects
    if (length(y) != 1) return(T) #if length isn't 1, the below functions give errors
    if (is.na(y)) return(T)
    is.finite(y)
  })]

  x
}
