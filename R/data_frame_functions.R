#' Standardize data.frame
#'
#' Returns a standardized data.frame, i.e. one where every variable has mean 0 and sd 1.
#' @param df (data.frame) The data.frame.
#' @param exclude (character vector) Names of variables to not standardize.
#' @param messages (boolean) Whether to output messages.
#' @keywords standardize, data.frame, z-score
#' @export
#' @examples
#' df = data.frame(rnorm(5, 100, 15))
#' df
#' std_df(df)
std_df = function(df, exclude = "", messages = T) {
  library(stringr)

  for (col_idx in 1:ncol(df)) {

    #skip if in exclusion vector
    if (colnames(df)[col_idx] %in% exclude) {
      next
    }

    #skip if factor
    if (class(unlist(df[col_idx])) == "factor") {
      if (messages){
        s = str_c("Skipped ", colnames(df)[col_idx], " because it is a factor.")
        message(s)
      }
      next
    }

    #skip if character
    if (class(unlist(df[col_idx])) == "character") {
      if (messages) {
        s = str_c("Skipped ", colnames(df)[col_idx], " because it is a character vector.")
        message(s)
      }
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
#' df = data.frame(rnorm(5), runif(5))
#' df
#' round_df(df)
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
#' @export
#' @examples
#' df = data.frame(rnorm(5), runif(5))
#' df
#' rank_df(df)
rank_df = function(df, ...) {
  library(stringr)

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
#' Returns a numeric matrix. Ordered factors are converted to numbers, while non-ordered factors are split into dummy variables using the first level as the the reference.
#' @param df (data.frame) A data.frame with variables.
#' @param skip_chr (log scalar) Whether to skip character columns (default). If false, they are converted to non-ordered factors.
#' @export
#' @examples
#' as_num_matrix(iris)
as_num_matrix = function(df, skip_chr = T) {
  #init
  mat = matrix(nrow = nrow(df), ncol = 0)
  new_colnames = character()

  #loop over each variable
  for (i in 1:ncol(df)) {

    #character?
    if (is.character(df[[i]])) {
      if (skip_chr) next #skip
      if (!skip_chr) df[[i]] = as.factor(df[[i]]) #convert to unordered factor
    }

    #if ordered factor
    if (is.ordered(df[[i]])) {
      #convert to integers
      mat = cbind(mat, as.numeric(df[[i]]))

      #save name
      new_colnames = c(new_colnames, colnames(df)[i])
      next
    }

    #non-ordered factor
    if (is.factor(df[[i]])) {
      #loop over each level
      for (lvl in levels(df[[i]])) {
        #skip first level
        if (lvl == levels(df[[i]])[1]) next

        #if not first, add dummy variable
        mat = cbind(mat, (df[[i]] == lvl) %>% as.numeric())

        #save name
        new_colnames = c(new_colnames, colnames(df)[i] + "_" + lvl)
      }
      next
    }

    #not a factor, keep as it is
    mat = cbind(mat, df[[i]])

    #save name
    new_colnames = c(new_colnames, colnames(df)[i])
  }

  #set names
  rownames(mat) = rownames(df)
  colnames(mat) = new_colnames

  mat
}


#' Get every subset of the data where one case is missing.
#'
#' Return a list of data.frames.
#' @param df A data.frame.
#' @keywords data.frame, subset
#' @export
#' @examples
#' df = data.frame(1:5, letters[1:5])
#' get_each_subset_minus_1(df)
get_each_subset_minus_1 = function(df){
  return_list = list()
  for (case in 1:nrow(df)){
    return_list[[case]] = df[-case, ] #save subset
  }

  return_list
}

#' Convert convertible columns in a data.frame to numeric.
#'
#' Attempts to convert all columns in a data.frame to numeric. If that fails, keeps the original.
#' @param df A data.frame.
#' @param stringsAsFactors Whether to convert strings to factors. Default is F.
#' @param skip_factors Whether to skip factors. Default is T.
#' @param remove_commas Whether to remove commas from the cells first. If present, they will make the conversion fail. Defaults to T.
#' @export
#' @examples
#' iris_chr = as.data.frame(lapply(iris, as.character)) #convert iris to strings
#' as_num_df(iris_chr) #convert back
as_num_df = function (df, stringsAsFactors = F, skip_factors = T, remove_commas = T) {
  library(stringr)

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
    if(!all(sapply(tmp_df, class) %in% c("numeric", "integer"))) stop("Some variables were not numeric!")

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
#' df = data.frame(c(1:10), letters[1:10])
#' df_addNA(df)
df_addNA =  function(df, prop = .1){
  n = nrow(df)
  m = ncol(df)
  num.to.na = ceiling(prop*n*m)
  id = sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows = id %/% m + 1
  cols = id %% m + 1
  sapply(seq(num.to.na), function(x){
    df[rows[x], cols[x]] <<- NA
  }
  )
  return(df)
}


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
#' sort_df(iris, 1)
#' sort_df(iris, 1, T)
#' sort_df(iris, "Species")
sort_df = function (df, vars = names(df), decreasing = F){
  df = as.data.frame(df)
  if (length(vars) == 0 || is.null(vars))
    return(df)
  df[do.call("order", list(what = df[, vars, drop = FALSE], decreasing = decreasing)), , drop = FALSE]
}



#Thanks to: https://stat.ethz.ch/pipermail/r-help/2011-October/293842.html
#' Residualized data.frame.
#'
#' Returns a residualized data.frame given a set of variables to partial out.
#' @param data (data.frame or matrix) The data object.
#' @param resid.vars (character vector) The names of the variables to use for the residualization.
#' @param exclude.resid.vars (logical) Whether to exclude the residualization variables from residualization. Defaults to TRUE.
#' @param return.resid.vars (logical) Whether to include the residualization variables in the returned data.frame. Defaults to TRUE.
#' @param print.models (logical) Wether to print the lm models used in the process. Defaults to TRUE.
#' @param exclude_vars (character vector) Names of variables are that excluded from the residualization.
#' @keywords modeling, residualize, partialing
#' @export residualize_DF df_residualize
#' @aliases residualize_DF
#' @examples
#' df = data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5))
#' df_residualize(df, resid.vars = "c")
df_residualize = function(data, resid.vars, suffix = "", exclude.resid.vars = T, return.resid.vars = T, print.models = T, exclude_vars = NULL) {
  library("stringr")

  #save rownames
  v_rownames = rownames(data)

  #make exclusion vector
  v_excluded = exclude_vars
  if (exclude.resid.vars) {
    v_excluded = c(v_excluded, resid.vars)
  }

  #the residuals function
  lm_f = function(x) {
    x = residuals(lm(data = data, formula = update(x ~ 0, paste0("~", resid.vars)), na.action = na.exclude))
  }

  #calculate residuals
  resid = data.frame(matrix(nrow = nrow(data), ncol = ncol(data))) #make empty df same size as data
  colnames(resid) = colnames(data) #get colnames

  for (colname in colnames(data)) { #loop over colnames
    if (!colname %in% v_excluded) { #if colname isn't excluded
      f = str_c(colname, " ~ ", paste0(resid.vars)) #make formula
      if (print.models) message(f) #message if desired

      #get resids
      resid[, colname] = residuals(lm(data = data, formula = f, na.action = na.exclude))
    } else { #if it isnt excluded
      #save the original
      resid[, colname] = data[, colname]
    }
  }

  #this adds the suffix
  colnames(resid) = paste0(colnames(data), suffix)

  #remove resid vars if desired
  if (return.resid.vars == F) {
    for (resid.vars in resid.vars) {
      resid[, resid.vars] = NULL
    }
  }

  #set rownames again
  rownames(resid) = v_rownames

  return(resid)
}

#old name
residualize_DF = df_residualize


#' Merge rows in data.frame.
#'
#' Find blocks of rows with a matching key and merge them using a given function.
#' @param df (data.frame or matrix) The data object.
#' @param key (character scalar) The name of the key variable, which is the variable to merge rows by. If given ".rownames", then it will use the rownames.
#' @param func (function) The function to use. Note that if you set numeric = FALSE, then the function must be able to handle non-numeric data.  Defaults to sum.
#' @param numeric (logical scalar) Whether to apply the function only to the numeric columns. Default=TRUE.
#' @export
#' @examples
#' t = data.frame(key = c("a", "a", "b", "b", "c"), value = 1:5)
#' merge_rows(t, "key") #rows merged by sum
#' merge_rows(t, "key", func = mean) #rows merged by mean
merge_rows = function(df, key, func = sum, numeric = TRUE) {
  library(plyr)

  #checks
  df = as.data.frame(df)

  #check function
  if (!is.function(func)) stop("func must be a function!")

  #numeric columns
  if (numeric) {
    v_numeric = sapply(df, is.numeric) #detect numeric cols
  } else {
    v_numeric = 1:ncol(df) #use all
  }

  #do it
  df2 = ddply(df, key, function(row) {
    #skip if only 1 row
    if (nrow(row) == 1) return(row)

    #compute
    row_new = row[1, ] #copy content of the first row in the block
    row_new[v_numeric] = apply(row[v_numeric], 2, func, na.rm = TRUE) #subset to numerics, use func
    row_new #save the new row
  })

  df2
}


#' Merge rows in data.frame by name.
#'
#' Find blocks of rows with a rowname that matches a given vector of names and merge them into one.
#' @param df (data.frame or matrix) The data object.
#' @param names (character vector) The rownames to merge.
#' @param new_name (character scalar) The new rownames to use. Defaults to the first member of the names parameter.
#' @param func (function) The function to use. Note that if you set numeric = FALSE, then the function must be able to handle non-numeric data. Defaults to mean.
#' @param numeric (logical scalar) Whether to apply the function only to the numeric columns. Default=TRUE.
#' @export
#' @examples
#' #suppse you had a data.frame with data for multiple variables
#' #but accidentally, one observation was given two names, "C" and "D".
#' #and data has been dispersed among the rows
#' #we can move all the data into one row without data loss.
#' t1 = data.frame(X = c(1, 2, 3, NA), Y = c(1, 2, NA, 3));rownames(t1) = LETTERS[1:4]
#' #here the real values for the C observation are both 3, but it has accidentally been called
#' "D".
#' merge_rows_by_name(df = t1, names = c("C", "D"), func = mean)
merge_rows_by_name = function(df, names, new_name, func = mean, numeric = TRUE) {
  library(stringr)
  library(plyr)

  #checks
  if (missing("df")) stop("df must be given!")
  if (missing("names")) stop("names must be given!")
  if (missing("new_name")) new_name = names[1]

  #make vectors for matches
  v_key = mapvalues(rownames(df), names, to = rep(new_name, length(names)))

  #make new df with rownames column
  df2 = cbind(df, "rownames__" = v_key)

  #call merge_rows
  df2 = merge_rows(df2, key = "rownames__", func = func, numeric = numeric)

  #restore rownames
  rownames(df2) = df2$rownames__

  #remove added column
  df2$rownames__ = NULL

  #return
  df2
}



#' Copy columns between data.frames
#'
#' Copy columns from one data.frame to another by name or pattern.
#' @param from (data.frame) The source data.frame.
#' @param to (data.frame) The destination data.frame.
#' @param columns (character vector or numeric vector) The columns to copy. Can be either their names or whole numbers indicating their position in the source data.frame. By default, it will copy all columns.
#' @param pattern (character scalar) Alternatively, a regex pattern to use to match the desired columns. Uses the stringr package.
#' @export
#' @examples
#' merge_rows_by_name(df = t1, names = c("C", "D"), func = mean)
copy_columns = function(from, to, columns, pattern) {
  #checks
  from = as.data.frame(from); to = as.data.frame(to) #convert
  if (nrow(from) != nrow(to)) stop("Number of rows not identical in the two objects!")

  #columns not given
  if (missing("columns") && missing("pattern")) pattern = "." #match all columns

  #pattern
  if (!missing("pattern")) {
    library(stringr)
    columns = colnames(from)[str_detect(string = colnames(from), pattern = pattern)]
  }

  #columns
  if (is.numeric(columns)) columns = colnames(from)[columns] #get names

  #loop and copy
  for (col in columns) {
    to[col] = from[col]
  }

  #return
  to
}




# split unsplit functions -----------------------------------------------------------------
#built in functions have some problems


#' Transform data.frame to vector
#'
#' Appends each column after each other. A wrapper for unlist() with some extra features.
#' @param data (data.frame) The data.frame.
#' @param fact_to_chr (logical scalar) Convert factors to characters? (Default yes)
#' @export
#' @examples
#' df_to_v(iris)
df_to_v = function(data, fact_to_chr = T) {

  #to df
  data = as.data.frame(data)

  #convert?
  if (fact_to_chr) {
    for (col in colnames(data)) {
      if (is.factor(data[, col])) data[, col] = as.character(data[, col])
    }
  }

  unlist(data) %>% unname()
}


#' Transform data.frame to a list of data.frames
#'
#' Splits a data.frame by a variable and returns a named list.
#' @param data (data.frame) The data.frame.
#' @param by (character scalar or vector) The values to split by. If input is a character scalar, will use that variable from the data.frame. If it is a longer vector, will use that to split with.
#' @param remove_by (logical scalar) Whether to remove the vector used to split with from the data.frame. Only relevant when using a character scalar as input to by.
#' @export
#' @examples
#' df_to_ldf(iris, "Species")
df_to_ldf = function(data, by, remove_by = T) {
  #determine by
  if (!length(by) == nrow(data)) { #if length doesnt match
    if (length(by) == 1 && is.character(by)) { #check if its a character scalar
      by_name = by #if so, save name
      by = data[[by_name]] #fetch the split vector

      #remove by?
      if (remove_by) {
        data[by_name] = NULL
      }
    }
  }

  #split
  data2 = split(x = data, f = by)

  data2
}


#' Transform list of data.frames to a data.frame.
#'
#' Combines a split data.frame back to the original.
#' @param list (list of data.frames) The data.frames.
#' @param add_by (logical scalar) Whether to add a variable indicating the origin of each row.
#' @param by_name (character scalar) What should the added variable by called? Only used when add_by = TRUE.
#' @param rownames_to_var (logical scalar) Should rownames be saved in a variable? (Default no)
#' @param rownames_name (logical scalar) The variable name to save rownames under. Only used if rownames_to_var = TRUE.
#' @export
#' @examples
#' ldf_to_df(df_to_ldf(iris), "Species")
ldf_to_df = function(list, add_by = T, by_name = "group", rownames_to_var = F, rownames_name = "rownames") {
  # browser()
  #save list names
  list_names = names(list)

  #remove list names
  list = unname(list)

  #save rownames
  if (rownames_to_var) {
    list = lapply(list, FUN = function(data) {
      data[rownames_name] = rownames(data)
      rownames(data) = NULL
      data
    })
  }

  #combine
  data = do.call("rbind", args = list)

  #add by?
  if (add_by) {
    nrows = sapply(list, nrow)
    by = mapply(x = list_names, each = nrows, FUN = function(x, each) {
      rep(x, each)
    }, SIMPLIFY = F) %>% unlist()

    data[by_name] = by
  }

  data
}



# add_if ------------------------------------------------------------------
#simple function to add id to a data.frame


#' Add ID column to data.frame
#'
#' Add an ID column to a data.frame. Convenience function.
#' @param data (data.frame) The data.frame.
#' @param id (vector) A vector of values to use. Will be repeated silently if not long enough. Will throw an error if it's not possible.
#' @param id_var (character scalar) What to call the id column (default "ID").
#' @export
#' @examples
#' add_id(iris, "A")
add_id = function(data, id, id_var="ID") {
  data[id_var] = id
  data
}



#' Rename variables in a data.frame
#'
#' Rename variables in a data.frame and keep their positions.
#' @param data (data.frame) The data.frame.
#' @param current_names (chr vector) The current names of the variables.
#' @param new_names (chr scalar) The new names of the variables.
#' @export
#' @examples
#' str(df_rename_vars(iris, "Sepal.Length", "Sepal_Length"))
df_rename_vars = function(data, current_names, new_names) {
  library(magrittr)

  #data.frame input
  data = as.data.frame(data)
  #check that input are character vectors
  if (!(is.character(current_names) & is.character(new_names))) {
    stop("Both current_names and new_names must be character vectors!")
  }
  #check that lengths match
  if (length(current_names) != length(new_names)) {
    stop("The vectors of names must be equally long!")
  }
  #check if variables exist
  sapply(current_names, function(name) {
    if (!name %in% colnames(data)) {
      stop("The variable " + name + " was not in the data.frame!")
    }
  })

  #main loop
  for (i in seq_along(current_names)) {
    #current position
    colnames(data)[i] = new_names[i]
  }

  data
}


#' Rename variables from a data.frame by name
#'
#' Remove variables from a data.frame by name.
#' @param data (data.frame) The data.frame.
#' @param names (chr vector) The variables to remove.
#' @export
#' @examples
#' str(df_remove_vars(iris, c("Sepal.Length", "Species")))
df_remove_vars = function(data, names) {
  for (name in names) {
    data[[name]] = NULL #remove
  }

  data
}

