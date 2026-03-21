#' Standardize data.frame
#'
#' Standardize all variables in a data.frame. Can use weighted standardization. Wraps \code{\link{standardize}}.
#' @param df (data frame) The data frame.
#' @param exclude (chr vector) Names of variables to not standardize.
#' @param messages (lgl scalar) Whether to output messages.
#' @param exclude_logicals Exclude boolean/logical variables
#' @param exclude_range_01 Exclude variables that range between 0 and 1
#' @param w (num vector) Weights to use, if any.
#' @export
#' @return Returns a standardized data.frame, i.e. one where every variable has mean 0 and sd 1.
#' @examples
#' head(iris) #not standardized
#' head(df_standardize(iris)) #standardized
df_standardize = function(df, exclude = NULL, messages = T, exclude_logicals = T, exclude_range_01 = T, w = NULL) {
  df

  #weights
  if (is.null(w)) w = rep(1, nrow(df))

  for (col_idx in 1:ncol(df)) {
    colname = colnames(df)[col_idx]

    #skip if in exclusion vector
    if (colname %in% exclude) {
      if (messages) cli::cli_inform("Skipped {colname} because it is in the exclude list")
      next
    }

    #logical
    if (is.logical(df[[colname]])) {
      if (exclude_logicals) {
        if (messages) cli::cli_inform("Skipped {colname} because it is a logical (boolean)")
        next
      }

      #standardize
      df[[colname]] = standardize(df[[colname]], w = w)
      next
    }

    #0-1 range numeric
    if (is.numeric(df[[colname]]) && all(is_between(df[[colname]], a = 0, b = 1, include_lower = T, include_upper = T), na.rm = T)) {
      if (exclude_range_01) {
        if (messages) cli::cli_inform("Skipped {colname} because it is ranged from 0 to 1 (a proportion, maybe)")
        next
      }

      #standardize
      df[[colname]] = standardize(df[[colname]], w = w)
      next
    }

    #character
    if (class(unlist(df[colname])) == "character") {
      if (messages) {
        if (messages) cli::cli_inform("Skipped {colname} because it is a character (string)")
      }
      next
    }

    #standardize numericals
    if (is.numeric(df[[colname]])) {
      df[[colname]] = standardize(df[[colname]], w = w)
      next
    }

    #skip if it's something else
    if (messages) cli::cli_inform("Skipped {colname} because it is class {str_c(class(df[[colname]]), collapse = ', ')}")
  }

  return(df)
}



#' Round numeric variables of a data frame.
#'
#' Returns a data frame where numeric variables have been rounded to the desired number of digits.
#'
#' This method is intended for printing purposes. It preserves the number of digits after the decimal point even if they are redundant (e.g. round 2.100 to 2 digits yields 2.10, not 2.1). All columns are converted to character.
#' @param df (data frame) A data.frame or matrix.
#' @param digits (int) The number of digits to round to.
#' @param NA_as_empty_string (lgl) Whether to convert \code{NA}s to empty strings ("").
#' @param simple (lgl) Whether to just call \code{\link{round}} on all the numerical columns.
#' @export
#' @examples
#' #round to 2 digits by default
#' iris %>% head %>% df_round
#' #round to whole numbers
#' iris %>% head %>% df_round(digits = 0)
#' #NAs are converted to empty strings
#' data.frame(a = c(NA, 1.1, -2.2), b = c("abc", "def", NA)) %>% df_round
#' #but this behavior can be toggled
#' data.frame(a = c(NA, 1.1, -2.2), b = c("abc", "def", NA)) %>% df_round(NA_as_empty_string = F)
#' #if for whatever reason you want to just call \code{\link{round}} on all the numerical columns, use \code{simple=T}
#' iris %>% head %>% df_round(digits = 1, simple = T)
df_round = function(df, digits = 2, NA_as_empty_string = T, simple = F) {

  #simple?
  if (simple) {
    df[] = lapply(df, function(x) {
      if (is.numeric(x) & !is.integer(x)) round(x, digits = digits) else x
    })

    return(df)
  }

  #factors to characters
  df[] = lapply(df, function(x) {
    if (is.factor(x)) as.character(x) else x
  })

  #as strings
  #we want this is we want to print the output
  #otherwise R gets rid of the redundant 0's at the end
  num_cols = purrr::map_lgl(df, ~is.numeric(.) & !is.integer(.)) %>% which()

    for (idx in num_cols) {
      df[[idx]] = round(df[[idx]], digits =  digits) %>%
        format(nsmall = digits) %>%
        stringr::str_replace(" *NA", "") %>%
        plyr::mapvalues("", NA, F)
    }

  #NAs to empty strings?
  if (NA_as_empty_string) {
    df[] = lapply(df, plyr::mapvalues, from = NA, to = "", warn_missing = F)
  }

  return(df)
}



#' Rank numeric variables of a data frame.
#'
#' Returns a data.frame where numeric variables have been converted to their ranks.
#' @param df (data frame) A data.frame.
#' @param ... Other parameters for \code{\link{rank}}.
#' @export
#' @examples
#' head(df_rank(iris)) #rank iris data. Automatically skips non-numeric columns.
df_rank = function(df, ...) {
  #for each column
  df2 = lapply(df, function(x) {
    #check what class it is, if numeric, then rank, otherwise, leave it as it is
    if (class(x) == "numeric") rank(x, ...) else return(x)
  }) %>% as.data.frame #transform into df again
  rownames(df2) = rownames(df)
  return(df2)
}



#' Get every subset of the data where one case is missing.
#'
#' Returns a list of data.frames. This function is useful for doing leave one out cross validation (LOOCV).
#' @param df (df) A data.frame.
#' @export
#' @examples
#' df = data.frame(n = 1:5, a = letters[1:5])
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
#' @param smart_factor_conversion Whether to automatically detect numeric factors. Default=T. This is done by checking whether the levels can be converted to numeric values. If they can, the factor is converted.
#' @param always_skip_factors Whether to always skip non-ordered factors. Default=F.
#' @param remove_commas Whether to remove commas from the cells first. If present, they will make the conversion to numeric fail. Defaults to T.
#' @param skip_factors Depreciated.
#' @export
#' @examples
#' iris_chr = as.data.frame(lapply(iris, as.character)) #convert iris to strings
#' str(iris_chr)
#' str(df_as_num(iris_chr)) #convert back
#' all(iris == df_as_num(iris_chr)) #confirm identity
df_as_num = function (df, stringsAsFactors = F, smart_factor_conversion = T, always_skip_factors = F, remove_commas = T, skip_factors = NULL) {
  if (!is.null(skip_factors)) {
    cli::cli_abort("This parameter is depreciated. Use one of the two new ones.")
  }

  #convert to df from whatever
  df = as.data.frame(df)

  #check type
  if (!is.data.frame(df))
    cli::cli_abort("df isn't a data.frame!")

  #names
  rnames = rownames(df)

  #commas?
  if (remove_commas) {
    df = lapply(df, function(x) {
      if (stringr::str_detect(x, ",") %>% any(., na.rm = T)) { #if commas present
        return(stringr::str_replace_all(x, ",", "")) #replace commas
      } else { #if not
        return(x) #return as it was
      }
    }) %>% as.data.frame(stringsAsFactors = F)
  }

  #new df
  new_df = lapply(df, function(x) {
    #skip because always skip?
    if (always_skip_factors) {
      #skip it if it is unordered
      if (is_unordered_factor(x)) {
        return(x)
      }
    }

    #smart check
    if (smart_factor_conversion) {
      if (is_unordered_factor(x)) {
        trial = tryCatch(as.numeric(levels(x)),
                         warning = function(w) {
                           w
                         })
        if ("warning" %in% class(trial)) {
          return(x)
        }
        else {
          return(as.numeric(as.character(x)))
        }
      }
    }

    #try to convert
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
#' @export
#' @examples
#' #add delta variables to iris between Sepal.Length and Sepal.Width
#' head(df_add_delta(iris, primary_var = 1, secondary_vars = 2))
#' #add delta variables to iris between Sepal.Length and all other numerical variables
#' head(df_add_delta(iris, primary_var = 1))
df_add_delta = function(df, primary_var, secondary_vars = NULL, prefix = "delta", sep = "_", subtract_from_primary = T, standardize = F) {
  #checks
  df = as.data.frame(df)
  primary_var
  if (class(df[[primary_var]]) != "numeric") cli::cli_abort("Primary var must be numeric!")

  #non-numeric
  non_num_vars = purrr::map_lgl(df, is.numeric) %>% `!` %>% names(.)[.]
  #find the non-numeric variable names

  #convert
  if (is.double(primary_var)) primary_var = as.integer(primary_var)

  #primary
  if (is.integer(primary_var)) {
    if (primary_var < 1) cli::cli_abort("Primary var indice must be a positive, whole number!")

    primary_var = colnames(df)[primary_var]
  }

  #secondary
  #if secondaries not given, use all other vars
  if(is.null(secondary_vars)) {
    secondary_vars = setdiff(colnames(df), primary_var) %>% setdiff(non_num_vars)
    cli::cli_inform("secondary_vars was not given. It was assumed that all other numerical variables were to be used.")
  }

  #convert
  if (is.double(secondary_vars)) secondary_vars = as.integer(secondary_vars)

  #if given integers
  if(is.integer(secondary_vars)) {
    #checks

    #neither all positive or all negative
    if (!(all(secondary_vars < 0) | all(secondary_vars > 0))) cli::cli_abort("When using indices for secondary vars, they must all be positive or all negative!")

    #outside of range
    if (any(secondary_vars > ncol(df))) cli::cli_abort("Secondary var indice outside range!")

    #all positive
    if (all(secondary_vars > 0)) {
      secondary_vars = colnames(df)[secondary_vars] #select vars using the indices

      #check if primary is among them
      if (primary_var %in% secondary_vars) cli::cli_abort("Primary var is among the secondary vars!")

      #check if any are non-numeric
      if (intersect(secondary_vars, non_num_vars) %>% length != 0) cli::cli_abort("Some secondary vars were non-numeric!")
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
    tmp_delta_name = stringr::str_c(prefix, sep, primary_var, sep, var)

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





#' Merge rows in data.frame.
#'
#' Find blocks of rows with a matching key or rowname, and merge them using a given function.
#'
#' In a variety of circumstances it is useful to merge several rows of data into a single row. For instance, if one dataset uses covers the same data but one uses a smaller unit than the other, then one may want to merge the smaller units so they correspond to the larger units. Alternative, if one has saved data for one unit under two different names by accident, one wants to merge these two (or more) rows without losing data.
#' @param data (data.frame or matrix) The data object.
#' @param key (character scalar) The name of the key variable, which is the variable to merge rows by. If given ".rownames", then it will use the rownames.
#' @param names (character vector) The rownames to merge.
#' @param new_name (character scalar) The new rownames to use. Defaults to the first member of the names parameter.
#' @param func (function) The function to use. Note that if you set numeric = FALSE, then the function must be able to handle non-numeric data.
#' @param numeric (logical scalar) Whether to apply the function only to the numeric columns.
#' ... Other parameters passed to func.
#' @export
#' @examples
#' #suppse you had a data.frame with data for multiple variables
#' #but accidentally, one observation was given two names, "C" and "D".
#' #and data has been dispersed among the rows
#' #we can move all the data into one row without data loss.
#' t = data.frame(X = c(1, 2, 3, NA), Y = c(1, 2, NA, 3));rownames(t) = LETTERS[1:4]
#' t
#' #here the real values for the C observation are both 3, but it has accidentally been called "D".
#' df_merge_rows(t, names = c("C", "D"), func = mean)
#' #suppose instead we have the names to match in a column, we can use the key parameter.
#' t = data.frame(large_unit = c("a", "a", "b", "b", "c"), value = 1:5)
#' t
#' df_merge_rows(t, "large_unit") #rows merged by sum by default
#' df_merge_rows(t, "large_unit", func = mean) #rows merged by mean
df_merge_rows = function(data, key = NULL, names = NULL, new_name = NULL, func = purrr::partial(sum, na.rm = T), numeric = T, ...) {

  #checks
  data
  df = as.data.frame(data); rm(data)
  assertthat::assert_that(is.function(func))

  if (is.null(names) & is.null(key)) cli::cli_abort("At least key or names parameters must be given!")
  if (!is.null(names) & !is.null(key)) cli::cli_abort("Both key and names were given. Only one of these can be given!")
  if (!is.null(names)) {
    if (is.null(new_name)) {
      new_name = names[1]
      cli::cli_inform("A new preferred rowname wasn't given. The first rowname was chosen for this purpose: `{new_name}`")
    }
  }


  #key version
  if (!is.null(key)) {
    #numeric columns
    if (numeric) {
      v_numeric = purrr::map_lgl(df, is.numeric) #detect numeric cols
    } else {
      v_numeric = 1:ncol(df) #use all
    }

    #do it
    df2 = plyr::ddply(df, key, function(row) {
      #skip if only 1 row
      if (nrow(row) == 1) return(row)

      #compute
      row_new = row[1, ] #copy content of the first row in the block
      row_new[v_numeric] = apply(row[v_numeric], 2, func, ...) #subset to numerics, use func
      row_new #save the new row
    })

    return(df2)
  }

  if (!is.null(names)) {
    #make vectors for matches
    v_key = plyr::mapvalues(rownames(df), names, to = rep(new_name, length(names)))

    #make new df with rownames column
    df2 = cbind(df, "rownames__" = v_key)

    #call self on the data with created key
    df2 = df_merge_rows(df2, key = "rownames__", func = func, numeric = numeric, ...)

    #restore rownames
    rownames(df2) = df2$rownames__

    #remove added column
    df2$rownames__ = NULL

    #return
    return(df2)
  }

  cli::cli_abort("Something went amiss! Debug this function!")
}



#built in functions have some problems


#' Transform data.frame to vector
#'
#' Appends each column after each other. A wrapper for unlist() that also removes the names.
#' @param data (data.frame) The data.frame.
#' @param fact_to_chr (log sclr) Convert factors to characters? (Default yes)
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
#' df_to_ldf(iris[c(1:5, 51:55, 101:105), ], "Species") #split iris by Species.
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
#' Combines a split data.frame back to the original. This wraps \code{\link{data.table}}'s \code{\link{rbindlist}} function.
#' @param list (list of data.frames) The data.frames.
#' @param add_by (log scalar) Whether to add a variable indicating the origin of each row.
#' @param by_name (chr scalar) What should the added variable by called? Only used when add_by = TRUE.
#' @param rownames_to_var (log scalar) Should rownames be saved in a variable? (Default no)
#' @param rownames_name (log scalar) The variable name to save rownames under. Only used if rownames_to_var = TRUE.
#' @export
#' @examples
#' #simple example: split iris by species, then recombine
#' all(ldf_to_df(df_to_ldf(iris, by = "Species")) == iris) #all identical
#'
#' #complex example: same as above but delete some columns
#' iris_set = iris[iris$Species == "setosa", -c(1, 5)] #create 3 lists with partly missing columns
#' iris_ver = iris[iris$Species == "versicolor", -c(2, 5)]
#' iris_vir = iris[iris$Species == "virginica", -c(3, 5)]
#' iris_list = list("setosa" = iris_set, "versicolor" = iris_ver, "virginica" = iris_vir) #a combined list
#' #then merge to one data.frame
#' ldf_to_df(iris_list) #note the missing data and column order
ldf_to_df = function(list, add_by = T, by_name = "group", rownames_to_var = F, rownames_name = "rownames", messages = T) {
  loadNamespace("data.table")

  #are there list names? if not, set them
  if (is.null(names(list))) names(list) = 1:length(list)

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

  #are some columns only found in some dfs?
  if (messages) {
    l_colnames = lapply(list, FUN = function(df) colnames(df)) #get names
    l_colnames = lapply(l_colnames, FUN = sort) #sort

    if (!all_elements_the_same(l_colnames)) {
      cli::cli_inform("Not all columns were found in each data.frame. This may indicate an error.")
    }
  }

  #combine
  data = data.table::rbindlist(list, fill = T) %>% as.data.frame()

  #add by?
  if (add_by) {
    nrows = purrr::map_int(list, nrow)
    by = mapply(x = list_names, each = nrows, FUN = function(x, each) {
      rep(x, each)
    }, SIMPLIFY = F) %>% unlist()

    data[by_name] = by
  }

  data
}


#' Reorder columns in a data.frame by name
#'
#' Reorder columns in a data.frame by name using a named vector.
#'
#' Copied and modified from http://stackoverflow.com/a/37009127/3980197.
#' @param data (data.frame) The data frame.
#' @param vars (named vector) The variables to reorder.
#' @export
#' @return The modified data frame.
#' @examples
#' #remove Species to front
#' head(df_reorder_columns(iris, c("Species" = 1)))
#' #check for identity
#' all(df_reorder_columns(iris, c("Species" = 1)) == iris[c(5, 1:4)])
#' #move multiple
#' head(df_reorder_columns(iris, c("Species" = 1, "Petal.Length" = 2)))
#' #throws error if not given a named vector
#' throws_error("df_reorder_columns(iris, 1)")
#' #or if names are not there
#' throws_error("df_reorder_columns(iris, c('abc' = 1))")
#' #throws warning if one tries to move the same multiple times as this is probably not intended
#' throws_error('df_reorder_columns(iris, c("Species" = 1, "Species" = 2))')
df_reorder_columns = function(data, vars){
  #checks
  if (!is.data.frame(data)) cli::cli_abort("data must be a data.frame!")
  if (!is_simple_vector(vars)) cli::cli_abort("vars must be a named vector!")
  if (is.null(names(vars))) cli::cli_abort("vars must be a named vector!")
  if (!all(names(vars) %in% names(data))) cli::cli_abort("Not all names were names of columns in the data.frame!")

  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars

  #more checks
  if (any(duplicated(var.nms)) | any(duplicated(var.pos))) cli::cli_abort("Some names/positions were identical! You cannot move the same column to multiple places!")

  ##sanity checks
  stopifnot( is.character(var.nms),
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0),
             all(var.pos <= var.nr) )

  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot(length(out.vec)==var.nr )

  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}



#' Add affix to column names.
#'
#' Add a prefix to column names on data.frame or matrix or similar.
#' @param data (an object) An object whose colnames should be changed
#' @param prefix (chr) A prefix to add.
#' @param suffix (chr) A suffix to add.
#' @param exclude (chr) Variables to exclude.
#' @export
#' @examples
#' df_add_affix(iris, prefix = "P_") |> head() #add P_ prefix
#' df_add_affix(iris, suffix = "_S") |> head() #add _S suffix
#' df_add_affix(iris, suffix = "_X", exclude = "Species") |> head() #add _X suffix but not for Species
df_add_affix = function(data, prefix = "", suffix = "", exclude = c()) {
  #rename columns not in exclude
  colnames(data)[!colnames(data) %in% exclude] = paste0(prefix, colnames(data)[!colnames(data) %in% exclude], suffix)
  #return
  data
}


#' Transpose data.frame
#'
#' Transpose a data.frame, returning a data.frame that also keeps the dimnames.
#' @param df (df) A data frame.
#' @return A transposed data frame.
#' @export
#' @examples
#' df_t(iris)
df_t = function(df) {
  df2 = t(df) %>% as.data.frame()

  #names
  rownames(df2) = colnames(df)
  colnames(df2) = rownames(df)

  df2
}


#' Gather by pattern
#'
#' Makes a data.frame tidyr by gathering columns that are variants of each other, such as for different years.
#' @param data (data.frame) A data.frame to tidy.
#' @param pattern (chr sclr) The regex pattern to group variables by.
#' @param varying_name (chr sclr) A name to use for the column of varying part of the name, e.g. the year. Defaults to ".varying".
#' @return A tidy data.frame.
#' @export
df_gather_by_pattern = function(data, pattern, key_col = ".varying", id_col = ".id", method = "pure_tidyr") {

  #convert class if needed
  data = as.data.frame(data)
  is_(key_col, class = "character", size = 1, error_on_false = T)
  is_(method, class = "character", size = 1, error_on_false = T)
  if (!method %in% c("pure_tidyr", "custom")) cli::cli_abort("The selected method does not exist!")
  #the pattern input will be checked by the sub-function

  #determine the groups of variables
  colnames_data = colnames(data)
  l_memberships = group_by_pattern(colnames_data, pattern = pattern)
  #remove the varying part
  colnames_remainder = stringr::str_replace(colnames_data, pattern = pattern, replacement = "")

  if (method == "custom") {
    #combine the groups using gather
    l_combined = mapply(members = l_memberships, name = names(l_memberships), FUN = function(members, name) {

      #gather members of this group
      d_gathered = tidyr::gather_(data, key_col = key_col, value_col = ".value", gather_cols = colnames_data[members]) %>%
        extract_last(margin_2 = 1:2) #extract last two columns

      #get the varying part
      colnames_varying = stringr::str_match(d_gathered[[key_col]], pattern = pattern)[, 2]

      #insert the better varying values
      d_gathered[[key_col]] = colnames_varying

      #insert id
      d_gathered[[id_col]] = rownames(data)

      #insert variable
      d_gathered$.var = name

      d_gathered
    }, SIMPLIFY = F)

    #bind by row
    d_long = ldf_to_df(l_combined, add_by = F)

    #spread
    d_wide = tidyr::spread_(d_long, key_col = ".var", value_col = ".value")

    #return
    return(d_wide)
  }

  if (method == "pure_tidyr") {
    #gather all values into one very long data.frame
    d_tmp_a = tidyr::gather_(data, key_col = ".varying", value_col = ".value", gather_cols = colnames_data)
    #add two missing columns
    d_tmp_a$.varying = stringr::str_replace(d_tmp_a$.varying, pattern = pattern, replacement = "|||\\1")
    d_tmp_a[[id_col]] = rownames(data)
    #separate the constant from the varying part of the variable names
    d_tmp_b = tidyr::separate_(d_tmp_a, col = ".varying", into = c(".variable", key_col), sep = "\\|\\|\\|")
    #finally, spread out the variables
    d_tmp_c = tidyr::spread_(d_tmp_b, key_col = ".variable", value_col = ".value")

    #return
    return(d_tmp_c)
  }

  cli::cli_abort("You somehow managed to break this function!")
}



#' Group by pattern
#'
#' Determines groups of strings using a regex pattern.
#' @param x (chr vctr) A character vector of values to group.
#' @param pattern (chr sclr) The regex pattern to group variables by.
#' @return A list of integer vectors. Each vector is the indices of the variables that belong to each group.
#' @export
group_by_pattern = function(x, pattern) {
  #check x input
  is_(x, class = "character", error_on_false = T)

  #check if pattern input is right size and type
  is_(pattern, class = "character", size = 1, error_on_false = T)
  #check if pattern input has a capturing group as required
  if (!(stringr::str_detect(pattern, pattern = "\\(") & stringr::str_detect(pattern, pattern = "\\)"))) cli::cli_abort("The given pattern did not have a capturing group!")

  ### determine the groups of variables
  #remove the varying part
  x_remainder = stringr::str_replace(x, pattern = pattern, replacement = "")
  #get the varying part
  x_varying = stringr::str_match(x, pattern = pattern)[, 2]

  #group memberships
  l_memberships = lapply(unique(x_remainder), FUN = function(x) {
    which(x_remainder == x)
  })
  names(l_memberships) = unique(x_remainder)

  #return list
  l_memberships
}


#legalize names in data frame, keep labels
#' Legalize variable names for data frame
#'
#' Data frames often bad illegal names, which cause problems in formulas. This function makes the names legal and returns the data frame. The original names are added as labels.
#' @param df (data frame)
#' @param func (function) A function to use to legalize the names.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' iris_bad = iris
#' names(iris_bad) = str_replace(names(iris_bad), "\\.", " ") #replace with spaces
#' names(iris_bad)
#' df_legalize_names(iris_bad) %>% names()
#' iris_bad = iris
#' names(iris_bad)[1] = "" #empty name
#' names(iris_bad)[2] = "" #another
#' names(iris_bad)[3] = NA #NA
#' names(iris_bad)
#' df_legalize_names(iris_bad) %>% names()
df_legalize_names = function(df, func = str_legalize) {
  #rename if empty
  names(df) = purrr::map_chr(names(df), function(name_) {
    if (name_ %in% c("", NA)) return("unnamed")
    name_
  })

  #loop and assign labels
  for (v in names(df)) {
    attr(df[[v]], which = "label") = v
  }

  #set clean names
  names(df) = names(df) %>%
    str_uniquify() %>%
    func()

  df
}




#' Merge columns
#'
#' Combines columns by filling in missing values with values from other columns in a designated order.
#' @param df (df) Data frame.
#' @param cols (chr) Variable names to merge.
#' @return A vector.
#' @export
#' @examples
#' tibble(
#' a = c(1, NA, NA),
#' b = c(-1, 2, NA),
#' c = c(-5, -9, 3)
#' ) %>% df_merge_cols(letters[1:3])
df_merge_cols = function(df, cols) {
  df
  cols

  #1 col? use that
  if (length(cols) == 1) return(df[[cols]])

  #0 col error
  if (length(cols) == 0) cli::cli_abort('cols must be a length >=1 chr vector')

  #make vector, begin with last col
  y = df[[cols[1]]]

  #fill in in reverse order
  for (col_ in cols[-1]) {
    #which are NA?
    y_NA = is.na(y)

    #fill in the NAs
    y[y_NA] = df[[col_]][y_NA]
  }

  y
}





#' Remove list columns
#'
#' @param x Data frame
#'
#' @return Data frame
#' @export
#'
#' @examples
#' iris$list = list(1)
#' iris$df = list(iris)
#' map(iris, typeof)
#' map(df_no_list_cols(iris), typeof)
df_no_list_cols = function(x) {
  x[!purrr::map_lgl(x, is.list)]
}






#' Make variable table
#'
#' @param x Data frame
#'
#' @return Data frame
#' @export
#'
#' @examples
#' df_var_table(iris)
df_var_table = function(x) {
  tibble(
    #name
    var_name = names(x),

    #label if given
    label = map_chr(x, function(x) {
      y = attr(x, "label")
      if (is.null(y)) return(NA_character_)
      if (length(y) > 1) return(str_glue("{y}={names(y)}") %>% str_c(collapse = ", "))
      y[1]
    }),

    #sample size
    n = miss_by_var(x, reverse = T),
    miss_prop = miss_by_var(x, prop = T),

    #type
    type = purrr::map_chr(x, typeof),
    classes = purrr::map_chr(x, ~class(.) %>% str_c(collapse = ", "))
  )
}


#winsorize function

#' Winsorsise data
#'
#' @param x Data frame
#' @param variables Names of variables to winsorsise. If not given, defaults to all numeric variables.
#' @param z Standard z score to winsorise to (mean+sd)
#' @param rz Robust z score to winsorise to (median+mad)
#' @param centile Centile to winsorsise to
#'
#' @return Data frame with the same variables as input.
#' @export
#'
#' @examples
#' iris[1, 1] = 100
#' df_winsorise(iris, z = 2)
#' df_winsorise(iris, rz = 2)
#' df_winsorise(iris, centile = .99)
df_winsorise = function(x, variables = NULL, z = NULL, rz = NULL, centile = NULL) {
  #variables default to all numeric
  if (is.null(variables)) variables = names(x)[purrr::map_lgl(x, is.numeric)]

  #exactly 1 method
  if (sum(c(!is.null(z), !is.null(rz), !is.null(centile))) != 1) cli::cli_abort("You must supply exactly one of `z`, `rz`, `centile`")

  #loop
  furrr::future_map(variables, function(v) {
    #z score based, nonrobust
    if (!is.null(z) | !is.null(rz)) {
      #compute descriptives
      v_desc = describe2(x[, v])

      #normal
      if (!is.null(z)) {
        #limits
        v_upper = v_desc$mean + v_desc$sd * z
        v_lower = v_desc$mean - v_desc$sd * z

        y = x[[v]] %>% kirkegaard::winsorise(upper = v_upper, lower = v_lower)
      }

      #robust
      if (!is.null(rz)) {
        #limits
        v_upper = v_desc$median + v_desc$mad * rz
        v_lower = v_desc$median - v_desc$mad * rz

        y = x[[v]] %>% kirkegaard::winsorise(upper = v_upper, lower = v_lower)
      }

    }

    #centile
    if (!is.null(centile)) {
      #limits
      v_centiles = x[[v]] %>% quantile(probs = c(centile, 1 - centile), na.rm = T)
      v_upper = v_centiles[1]
      v_lower = v_centiles[2]

      y = x[[v]] %>% kirkegaard::winsorise(upper = v_upper, lower = v_lower)
    }

    tibble(
      y = y
    ) %>% set_colnames(v)

  }) %>% bind_cols() %>% return()
}


#' Smart join for data frames
#'
#' @param data1 Main data frame
#' @param data2 Another data frame to update with
#' @param id_col Column to use for ID, must be unique.
#' @param update Updating method. "missing" = replace NA values in data1 with non-NA values from data2. "values" = replace NA and non-NA values in data1 with non-NA values from data2. "all" replace NA and non-NA values in data1 with values (including NA) from data2.
#'
#' @returns A data frame
#' @export
#'
#' @examples
#' data1 <- data.frame(
#'   id = 1:4,
#'   value = c(1, NA, 3, 4)
#' )
#'
#' data2 <- data.frame(
#'   id = 2:4,
#'   value = c(20, 30, NA)
#' )
#'
#' # Fill only missing values in data1
#' smart_join(data1, data2, id_col = "id", update = "missing")
#'
#' # Overwrite non-missing values where data2 has non-NA
#' smart_join(data1, data2, id_col = "id", update = "values")
#'
#' # Fully replace values in data1 with those from data2 (including NA)
#' smart_join(data1, data2, id_col = "id", update = "all")
smart_join <- function(data1, data2, id_col, update = c("missing", "values", "all")) {
  update <- match.arg(update)

  # Capture id_col as symbol; supports bare name or string
  id_sym  <- rlang::ensym(id_col)
  id_name <- rlang::as_name(id_sym)

  # Special case: rownames as key
  if (identical(id_name, ".rownames")) {
    if (is.null(rownames(data1)) || any(rownames(data1) == "")) {
      cli::cli_abort("data1 does not have valid rownames for id_col = '.rownames'.")
    }
    if (is.null(rownames(data2)) || any(rownames(data2) == "")) {
      cli::cli_abort("data2 does not have valid rownames for id_col = '.rownames'.")
    }

    data1 <- tibble::rownames_to_column(as.data.frame(data1), var = ".rownames")
    data2 <- tibble::rownames_to_column(as.data.frame(data2), var = ".rownames")
    id_name <- ".rownames"
  } else {
    if (!id_name %in% colnames(data1)) {
      cli::cli_abort("id_col '{id_name}' not found in data1.")
    }
    if (!id_name %in% colnames(data2)) {
      cli::cli_abort("id_col '{id_name}' not found in data2.")
    }
  }

  # Enforce uniqueness of IDs in both data sets
  if (anyDuplicated(data1[[id_name]]) > 0) {
    cli::cli_abort("id_col '{id_name}' has duplicate values in data1; smart_join() requires unique IDs in both data1 and data2.")
  }
  if (anyDuplicated(data2[[id_name]]) > 0) {
    cli::cli_abort("id_col '{id_name}' has duplicate values in data2; smart_join() requires unique IDs in both data1 and data2.")
  }

  # Keep data2 IDs around so we know which joined rows actually came from data2
  data2_ids <- data2[[id_name]]

  # --- sf handling: geometry should always come from data1 (left) ----
  if (inherits(data1, "sf") && inherits(data2, "sf")) {
    # sf::full_join.sf() forbids y being sf; drop geometry from data2
    data2 <- sf::st_drop_geometry(data2)
  } else if (!inherits(data1, "sf") && inherits(data2, "sf")) {
    # If only right side is sf, also drop its geometry
    cli::cli_warn("Dropping geometry from data2; geometry must come from data1.")
    data2 <- sf::st_drop_geometry(data2)
  }

  # Shared non-ID columns (after possible geometry drop from data2)
  common_cols <- intersect(
    setdiff(names(data1), id_name),
    setdiff(names(data2), id_name)
  )

  # If data1 is sf, don't try to merge its geometry column
  geom_col <- attr(data1, "sf_column", exact = TRUE)
  sf_geom_cols <- intersect(common_cols, geom_col)
  common_to_merge <- setdiff(common_cols, sf_geom_cols)

  # Full join: keep all rows and columns
  joined <- dplyr::full_join(
    data1,
    data2,
    by = setNames(id_name, id_name),
    suffix = c(".data1", ".data2")
  )

  # For each joined row, did this ID exist in data2?
  has_data2_row <- joined[[id_name]] %in% data2_ids

  fillings <- list()

  # For shared, non-geometry columns: update according to 'update' mode
  for (col in common_to_merge) {
    col1 <- paste0(col, ".data1")
    col2 <- paste0(col, ".data2")

    has1 <- col1 %in% names(joined)
    has2 <- col2 %in% names(joined)

    if (has1 && has2) {
      x_vals <- joined[[col1]]  # from data1
      y_vals <- joined[[col2]]  # from data2

      # Decide where we will actually take values from data2
      mask <- switch(
        update,
        "missing" = is.na(x_vals) & !is.na(y_vals) & has_data2_row,
        "values"  = !is.na(y_vals) & has_data2_row,
        "all"     = has_data2_row
      )

      will_use_data2 <- any(mask, na.rm = TRUE)

      if (will_use_data2) {
        # --- type compatibility check ----------------------------
        is_factor_both  <- is.factor(x_vals) && is.factor(y_vals)
        is_numeric_both <- is.numeric(x_vals) && is.numeric(y_vals)
        is_char_both    <- is.character(x_vals) && is.character(y_vals)
        is_logical_both <- is.logical(x_vals) && is.logical(y_vals)
        is_date_both    <- inherits(x_vals, "Date")   && inherits(y_vals, "Date")
        is_posix_both   <- inherits(x_vals, "POSIXt") && inherits(y_vals, "POSIXt")

        if (! (is_factor_both || is_numeric_both || is_char_both ||
               is_logical_both || is_date_both || is_posix_both)) {
          cli::cli_abort("Column '{col}' has incompatible types between data1 ({paste(class(x_vals), collapse = '/')}) and data2 ({paste(class(y_vals), collapse = '/')}); cannot safely update values.")
        }

        common_type <- vctrs::vec_ptype_common(x_vals, y_vals)
        x_cast <- vctrs::vec_cast(x_vals, common_type)
        y_cast <- vctrs::vec_cast(y_vals, common_type)

        res_col <- x_cast
        res_col[mask] <- y_cast[mask]

        joined[[col]] <- res_col
        fillings[[col]] <- sum(mask, na.rm = TRUE)
      } else {
        # No actual use of data2 values → keep data1 as-is
        joined[[col]] <- x_vals
      }

      joined[[col1]] <- NULL
      joined[[col2]] <- NULL

    } else if (has1 && !has2) {
      # Only data1 version present -> rename
      joined[[col]] <- joined[[col1]]
      joined[[col1]] <- NULL

    } else if (!has1 && has2) {
      # Only data2 version present -> rename
      joined[[col]] <- joined[[col2]]
      joined[[col2]] <- NULL
    }
  }

  # Only message if something was actually updated
  if (length(fillings) > 0) {
    mode_text <- switch(
      update,
      "missing" = "Filled missing values",
      "values"  = "Updated non-missing values",
      "all"     = "Replaced values"
    )

    detail_lines <- paste(
      sprintf(" - %s: %d value(s)", names(fillings), unlist(fillings)),
      collapse = "\n"
    )
    cli::cli_inform("{mode_text} in {length(fillings)} shared variable(s) from data2 into data1 (update = '{update}'):\n{detail_lines}")
  }

  # --- NEW: restore rownames when id_col = ".rownames" -----------------
  if (identical(id_name, ".rownames") && ".rownames" %in% names(joined)) {
    rn <- joined$.rownames

    # Only restore if they are unique and non-empty
    if (!anyDuplicated(rn) && all(!is.na(rn)) && all(rn != "")) {
      joined_df <- as.data.frame(joined)
      rownames(joined_df) <- as.character(rn)
      joined_df$.rownames <- NULL
      return(joined_df)
    }
  }

  joined
}
