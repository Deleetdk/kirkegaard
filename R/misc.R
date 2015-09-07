## Misc other functions

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
#' which_max2()
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
  print("test")
  return(which.max(x)) #index of max
}

#' Generalized which.min(). Returns the index of the cell or number with the lowest value.
#'
#' A generalization of which.min() that works on matrices and data.frames too.
#' @param x a numeric vector, matrix or data.frame.
#' @keywords min, index
#' @export
#' @examples
#' which_min2()
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
  print("test")
  return(which.min(x)) #index of max
}

##Combines lower and upper part of two matrices into one. Additional option for the diagonal.
#Credit to: http://stackoverflow.com/questions/13115720/how-do-i-combine-the-upper-tri-of-one-matrix-with-the-lower-tri-of-another-in-r
#' Combine upper and lower part of a matrix.
#'
#' Returns a matrix composed of an upper and a upper part.
#' @param .upper.tri a matrix or data.frame for the upper trianagle.
#' @param .lower.tri a matrix or data.frame for the lower trianagle.
#' @param .diag a value to insert in the diagonal. Defaults to NA.
#' @keywords combine, matrix, data.frame, upper, lower
#' @export
#' @examples
#' combine_upperlower()
combine_upperlower = function(.upper.tri, .lower.tri, .diag = NA) {
  #detect wrong input
  if (!is.matrix(.upper.tri) & !is.data.frame(.upper.tri)) {
    stop("First parameter was not a matrix or data.frame.")
  }
  if (!is.matrix(.lower.tri) & !is.data.frame(.lower.tri)) {
    stop("Second parameter was not a matrix or data.frame.")
  }

  #inpute types
  both.df = is.data.frame(.lower.tri) & is.data.frame(.upper.tri) #are both df?
  #convert
  .lower.tri = as.matrix(.lower.tri)
  .upper.tri = as.matrix(.upper.tri)
  #make new
  new = .lower.tri #copy of lower
  diag(new) = .diag #diagonal to whatever
  new[upper.tri(new)] <- .upper.tri[upper.tri(.upper.tri)]
  if (both.df) {return(as.data.frame(new))} #convert back to df
  return(new) #return as matrix
}


#' Insert newlines into text every nth character.
#'
#' Returns a character string with newlines every nth character. See also add_newlines().
#' @param x A character string.
#' @param interval How often the newlines are added.
#' @keywords string, newline, label, text
#' @export
#' @examples
#' new_lines_adder()
new_lines_adder = function(x, interval) {
  #add spaces after /
  x = str_replace_all(x, "/", "/ ")
  #split at spaces
  x.split = strsplit(x, " ")[[1]]
  # get length of snippets, add one for space
  lens <- nchar(x.split) + 1
  # now the trick: split the text into lines with
  # length of at most interval + 1 (including the spaces)
  lines <- cumsum(lens) %/% (interval + 1)
  # construct the lines
  x.lines <- tapply(x.split, lines, function(line)
    paste0(paste(line, collapse=" "), "\n"), simplify = TRUE)
  # put everything into a single string
  result <- paste(x.lines, collapse="")
  #remove spaces we added after /
  result = str_replace_all(result, "/ ", "/")
  return(result)
}

#' Insert newlines into text every nth character.
#'
#' Returns a character string with newlines every nth character. Works for character vectors too.
#' @param x A character string or vector.
#' @param total.length The total length of each line of text. Defaults to 95 (suitable for ggplot2).
#' @keywords string, newline, label, text
#' @export
#' @examples
#' add_newlines()
add_newlines = function(x, total.length = 95) {
  # make sure, x is a character array
  x = as.character(x)
  #determine number of groups
  groups = length(x)
  # apply splitter to each
  t = sapply(x, FUN = new_lines_adder, interval = round(total.length/groups), USE.NAMES=FALSE)
  return(t)
}


#' Standardize data.frame
#'
#' Returns a standardized data.frame, i.e. one where every variable has mean 0 and sd 1.
#' @param df A data.frame or matrix.
#' @param exclude A character vector of variables to exclude from the process.
#' @keywords standardize, data.frame, z-score
#' @export
#' @examples
#' std_df()
std_df = function(df, exclude = "") {
  library(stringr)

  for (col_idx in 1:ncol(df)) {

    #skip if in exclusion vector
    if (colnames(df)[col_idx] %in% exclude) {
      next
    }

    #skip if factor
    if (class(unlist(df[col_idx])) == "factor") {
      s = str_c("Skipped ", colnames(df)[col_idx], " because it is a factor.")
      print(s)
      next
    }

    #skip if character
    if (class(unlist(df[col_idx])) == "character") {
      s = str_c("Skipped ", colnames(df)[col_idx], " because it is a character vector.")
      print(s)
      next
    }

    #otherwise standardize
    df[col_idx] = scale(df[col_idx])
  }

  return(df)
}


#' Round numeric variables of a data frame.
#'
#' Returns a data.frame where numeric variables have been rounded to the desired number of digits.
#' @param df A data.frame or matrix.
#' @param digits The number of digits to round to.
#' @keywords round, data.frame
#' @export
#' @examples
#' round_df()
round_df = function(df, digits=3) {
  df = as.data.frame(df) #convert to df
  for (idx in seq_along(df)) {
    num = is.numeric(df[ , idx, drop = T]) #have to drop to get the variable not a df
    if (num) {
      df[idx] = round(df[idx], digits)
    }
  }

  return(df)
}

#' Convert a data.frame to a numeric matrix, including factors.
#'
#' Returns a numeric matrix.
#' @param df A data.frame.
#' @keywords data.frame, numeric, factor, convert
#' @export
#' @examples
#' as_num_matrix()
as_num_matrix = function(df) {
  return(as.matrix(as.data.frame(lapply(df, as.numeric))))
}

#' Get every subset of the data where one case is missing.
#'
#' Return a list of data.frames.
#' @param df A data.frame.
#' @keywords data.frame, subset
#' @export
#' @examples
#' get_each_subset_minus_1()
get_each_subset_minus_1 = function(df){
  return_list = list()
  for (case in 1:nrow(df)){
    return_list[[case]] = df[-case, ] #save subset
  }

  return_list
}


#' Cut into bins and get proportions
#'
#' Cuts a vector into a specified number of equal sized bins and calculations the proportion of datapoints in each bin. Returns a data.frame.
#' @param x A numeric vector.
#' @param breaks_ The number of bins to use.
#' @keywords cut, bins, proportion, table
#' @export
#' @examples
#' get_prop_table()
get_prop_table = function(x, breaks_=20){
  library(magrittr)
  library(plyr)
  x_prop_table = cut(x, 20) %>% table(.) %>% prop.table %>% data.frame
  colnames(x_prop_table) = c("interval", "density")
  intervals = x_prop_table$interval %>% as.character
  fetch_numbers = str_extract_all(intervals, "\\d\\.\\d*")
  x_prop_table$means = laply(fetch_numbers, function(x) {
    x %>% as.numeric %>% mean
  })
  return(x_prop_table)
}


