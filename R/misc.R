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
    df[col_idx] = scale(df[col_idx]) %>% as.vector
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


#' Rank numeric variables of a data frame.
#'
#' Returns a data.frame where numeric variables have been converted to their ranks.
#' @param df A data.frame.
#' @param ... Other parameters for rank().
#' @keywords rank, data.frame
#' @export
#' @examples
#' rank_df()
rank_df = function(df, ...) {
  #for each column
  df2 = lapply(df, function(x) {
    #check what class it is, if numeric, then rank, otherwise, leave it as it is
    if (class(x) == "numeric") rank(x, ...) else return(x)
  }) %>% as.data.frame #transform into df again
  rownames(df2) = rownames(df)
  return(df2)
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


#' Convert convertible columns in a data.frame to numeric.
#'
#' Attempts to convert all columns in a data.frame to numeric. If that fails, keeps the original.
#' @param df A data.frame.
#' @param stringsAsFactors Whether to convert strings to factors. Default is F.
#' @param skip_factors Whether to skip factors. Default is T.
#' @param remove_commas Whether to remove commas from the cells first. If present, they will make the conversion fail. Defaults to T.
#' @keywords convert, as, numeric, data.frame
#' @export
#' @examples
#' as_num_df()
as_num_df = function (df, stringsAsFactors = F, skip_factors = T, remove_commas = T) {
  #convert to df from whatever
  df = as.data.frame(df)


  #check type
  if (!is.data.frame(df))
    stop("df isn't a data.frame!")

  #names
  rnames = rownames(df)

  #commas?
  if (remove_commas) {
    df = lapply(df, function(x) {
      if (str_detect(x, ",") %>% any(., na.rm = T)) { #if commas present
        return(str_replace_all(x, ",", "")) #replace commas
      } else { #if not
        return(x) #return as it was
      }
    }) %>% as.data.frame(stringsAsFactors = F)
  }

  #new df
  new_df = lapply(df, function(x) {
    if (skip_factors) {
      if (class(x) == "factor") {
        return(x)
      }
    }
    trial = tryCatch({
      as.numeric(x)
    }, warning = function(w) {
      w
    })
    if ("warning" %in% class(trial)) {
      return(x)
    }
    else {
      return(as.numeric(x))
    }
  }) %>% as.data.frame(stringsAsFactors = stringsAsFactors) #decide whether to make factors

  #set names again
  rownames(new_df) = rnames

  return(new_df)
}


#' Add delta columns to a data.frame.
#'
#' Adds delta (difference) columns to a data.frame. These are made from one primary variable and a number of secondary variables. Variables can be given either by indices or by name. If no secondary variables are given, all numeric variables are used.
#' @param df (data.frame) A data.frame.
#' @param primary_var (character or numeric vector) The primary variable to use.
#' @param secondary_vars (character or numeric vector) Which secondary variables to use. Defaults to all non-primary variables.
#' @param prefix (character) The prefix to use on the new variables. Default="delta".
#' @param sep (character) The separator to use for the new variables. Default="_".
#' @param subtract_from_primary (boolean) Whether to subtract from the primary variable. Defaults to T. If F, then the primary will be subtracted from the secondaries.
#' @param standardize (boolean) Whether to standardize the difference scores. Defaults to F.
#' @keywords date.frame, difference, delta
#' @export
#' @examples
#' df_add_delta()
df_add_delta = function(df, primary_var, secondary_vars, prefix = "delta", sep = "_", subtract_from_primary = T, standardize = F) {
  library(stringr)
  #browser()
  #checks
  df = as.data.frame(df)
  if (missing("primary_var")) stop("Primary var not given!")
  if (class(df[[primary_var]]) != "numeric") stop("Primary var must be numeric!")

  #non-numeric
  non_num_vars = sapply(df, is.numeric) %>% `!` %>% names(.)[.]
  #find the non-numeric variable names

  #convert
  if (is.double(primary_var)) primary_var = as.integer(primary_var)

  #primary
  if (is.integer(primary_var)) {
    if (primary_var < 1) stop("Primary var indice must be a positive, whole number!")

    primary_var = colnames(df)[primary_var]
  }

  #secondary
  #if secondaries not given, use all other vars
  if(missing(secondary_vars)) secondary_vars = setdiff(colnames(df), primary_var) %>% setdiff(non_num_vars)

  #convert
  if (is.double(secondary_vars)) secondary_vars = as.integer(secondary_vars)

  #if given integers
  if(is.integer(secondary_vars)) {
    #checks

    #neither all positive or all negative
    if (!(all(secondary_vars < 0) | all(secondary_vars > 0))) stop("When using indices for secondary vars, they must all be positive or all negative!")

    #outside of range
    if (any(secondary_vars > ncol(df))) stop("Secondary var indice outside range!")

    #all positive
    if (all(secondary_vars > 0)) {
      secondary_vars = colnames(df)[secondary_vars] #select vars using the indices

      #check if primary is among them
      if (primary_var %in% secondary_vars) stop("Primary var is among the secondary vars!")

      #check if any are non-numeric
      if (intersect(secondary_vars, non_num_vars) %>% length != 0) stop("Some secondary vars were non-numeric!")
    }

    #all negative
    if (all(secondary_vars < 0)) {
      #fetch secondary var names
      secondary_vars = colnames(df)[secondary_vars]

      #remove primary var if among
      secondary_vars = setdiff(secondary_vars, primary_var) %>%
        setdiff(., non_num_vars) #remove non-num vars
    }
  }

  #add delta vars
  for (var in secondary_vars) {
    tmp_delta_name = str_c(prefix, sep, primary_var, sep, var)

    #method
    if (!standardize) {
      if (subtract_from_primary) {
        df[tmp_delta_name] = df[primary_var] - df[var]
      } else {
        df[tmp_delta_name] = df[var] - df[primary_var]
      }
    } else {
      if (subtract_from_primary) {
        df[tmp_delta_name] = (df[primary_var] - df[var]) %>% scale() %>% as.vector
      } else {
        df[tmp_delta_name] = (df[var] - df[primary_var]) %>% scale() %>% as.vector
      }
    }

  }

  return(df)
}


#' Calculate summary statistics by row from multiple columns in a data.frame.
#'
#' Calculate mean/median/sd/etc values by row. Can be given multiple data.frames, matrices or vectors which are coerced into one data.frame. Can standardize data before calculating. Ignores missing data by default. Can also subset columns from a data.frame using regex of the colnames.
#' @param ... (data.frames, matrices, vectors) The variables to use. They will be coerced to a single data.frame.
#' @param standardize (boolean) Whether to standardize the data before calculating. Defaults to F.
#' @param func (function) Which base function to call. Can be mean, median, sd, var and any other suitable function. Default to mean.
#' @param pattern (string) A pattern to use for finding the columns names.
#' @param ignore_NA (boolean) Whether to ignore missing data. Defaults to T.
#' @keywords date.frame, mean, standardize, median, function
#' @export
#' @examples
#' df_func()
df_func = function(..., standardize = F, func = mean, pattern, ignore_NA = T) {
  library(stringr)

  #make df
  tmp_df = data.frame(...)

  ## if pattern not given
  if (missing("pattern")) {

    #check for numericness
    if(!all(sapply(tmp_df, class) == "numeric")) stop("Some variables were not numeric!")

    #standardize?
    if (standardize) tmp_df = std_df(tmp_df)

    #get results
    results = apply(tmp_df, 1, function(x) {
      get("func")(x, na.rm = ignore_NA)
    })

    return(results)
  }

  ## if pattern given

  #find cols to use
  cols = str_detect(colnames(tmp_df), pattern)

  #check if any cols were found
  if (all(!cols)) stop("No columns matched the pattern!")

  #get results by calling simpler function
  results = df_func(tmp_df[cols], standardize = standardize, func = func, ignore_NA = ignore_NA)

  return(results)
}

#func from https://trinkerrstuff.wordpress.com/2012/05/02/function-to-generate-a-random-data-set/

#' Insert random NAs into a data.frame.
#'
#' Inserts missing data into a data.frame at random, thus creating data that are Missing Completely At Random (MCAR). THis isn't how data usually are missing in the real world, but may be sufficient for some simulations.
#' @param df (data.frame) A data.frame.
#' @param prop (numeric) The proportion of NAs to add.
#' @keywords date.frame, missing data, NA, add, simulate
#' @export
#' @examples
#' df_addNA()
df_addNA <-  NAinsert <- function(df, prop = .1){
  n <- nrow(df)
  m <- ncol(df)
  num.to.na <- ceiling(prop*n*m)
  id <- sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows <- id %/% m + 1
  cols <- id %% m + 1
  sapply(seq(num.to.na), function(x){
    df[rows[x], cols[x]] <<- NA
  }
  )
  return(df)
}



# sort_df -----------------------------------------------------------------
#sort a df according to a variable
#just a minor edit of the function in reshape package.

#' Sort a data.frame by one or more variables.
#'
#' A wrapper for order(). Improved from the version in the reshape package.
#' @param df (data.frame) A data.frame.
#' @param vars (strings/integers) variables to use for sorting.
#' @param decreasing Whether to use decreasing order. Default=F.
#' @keywords date.frame, missing data, NA, add, simulate
#' @export
#' @examples
#' sort_df()
sort_df = function (df, vars = names(df), decreasing = F)
{
  if (length(vars) == 0 || is.null(vars))
    return(df)
  df[do.call("order", list(what = df[, vars, drop = FALSE], decreasing = decreasing)), , drop = FALSE]
}



# score_items -------------------------------------------------------------
#function to score multiple choice data

#' Score multiple choice items
#'
#' Score a data.frame with multiple choice items using an answer key. Each column has a answers for one question.
#' @param df (data.frame) A data.frame with responses.
#' @param key (vector) A vector with the correct responses.
#' @keywords date.frame, score, multiple choice
#' @export
#' @examples
#' score_items()
score_items = function(df, key) {
  library(magrittr)
  #checks
  if (ncol(df) != length(key)) stop("Key length does not match data.frame! There must be the same number of columns as keys.")

  #score
  df2 = lapply(seq_along(df), function(x) {
    df[[x]] == key[x] %>% as.numeric
  }) %>% as.data.frame

  colnames(df2) = colnames(df)

  return(df2)
}



