#' Standardize data.frame
#'
#' Standardize all variables in a data.frame. Can use weighted standardization. Wraps \code{\link{standardize}}.
#' @param df (data frame) The data frame.
#' @param exclude (chr vector) Names of variables to not standardize.
#' @param messages (lgl scalar) Whether to output messages.
#' @param exclude_factors (lgl scalar) Whether to exclude factors.
#' @param w (num vector) Weights to use, if any.
#' @export
#' @return Returns a standardized data.frame, i.e. one where every variable has mean 0 and sd 1.
#' @examples
#' head(iris) #not standardized
#' head(df_standardize(iris)) #standardized
#' head(df_standardize(iris, exclude_factors = F)) #also standardize factors (may be nonsensical)
#' head(df_standardize(iris, w = runif(150))) #standardized with weights
df_standardize = function(df, exclude = NULL, messages = T, exclude_factors = T, w = NULL) {
  df

  #weights
  if (is.null(w)) w = rep(1, nrow(df))

  for (col_idx in 1:ncol(df)) {

    #skip if in exclusion vector
    if (colnames(df)[col_idx] %in% exclude) {
      next
    }

    #logical
    if (is.logical(df[[col_idx]])) {
      if (messages) message("Skipped " + colnames(df)[col_idx] + " because it is a logical.")
      next
    }

    #factor
    if (is.factor(df[[col_idx]])) {
      if (exclude_factors) {
        if (messages) message("Skipped " + colnames(df)[col_idx] + " because it is a factor.")
        next
      } else {
        df[[col_idx]] = standardize(df[[col_idx]] %>% as.numeric(), w = w)
        next
      }
    }

    #character
    if (class(unlist(df[col_idx])) == "character") {
      if (messages) {
        if (messages) message("Skipped " + colnames(df)[col_idx] + " because it is a character.")
      }
      next
    }

    #otherwise standardize
    df[[col_idx]] = standardize(df[[col_idx]], w = w)
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
      if (is.numeric(x)) round(x, digits = digits) else x
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
  num_cols = purrr::map_lgl(df, is.numeric) %>% which

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


#' Convert a data.frame to a numeric matrix, including factors.
#'
#' Returns a numeric matrix. Ordered factors are converted to numbers, while non-ordered factors are split into dummy variables using the first level as the the reference.
#'
#' Factors with only two levels are kept as they are.
#' @param df (data.frame) A data.frame with variables.
#' @param skip_chr (lgl scalar) Whether to skip character columns (default). If false, they are converted to non-ordered factors.
#' @export
#' @examples
#' head(as_num_matrix(iris)) #Convert iris to purely numerics. Two variables are created because the original had 3 levels.
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
      # if only 2 levels
      if (length(levels(df[[i]])) == 2) {
        #keep as it is
        mat = cbind(mat, df[[i]])

        #save name
        new_colnames = c(new_colnames, colnames(df)[i])

        next
      }

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
    stop("This parameter is depreciated. Use one of the two new ones.")
  }

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
  if (class(df[[primary_var]]) != "numeric") stop("Primary var must be numeric!")

  #non-numeric
  non_num_vars = purrr::map_lgl(df, is.numeric) %>% `!` %>% names(.)[.]
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
  if(is.null(secondary_vars)) {
    secondary_vars = setdiff(colnames(df), primary_var) %>% setdiff(non_num_vars)
    message("secondary_vars was not given. It was assumed that all other numerical variables were to be used.")
  }

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


#' Calculate summary statistics by rows in a data.frame.
#'
#' Calculate mean/median/sd/etc values by row. Can be given multiple data.frames, matrices or vectors which are coerced into one data.frame. Can standardize data before calculating. Ignores missing data by default.
#' @param ... (data.frames, matrices, vectors) The variables to use. They will be coerced to a single data.frame.
#' @param standardize (boolean) Whether to standardize the data before calculating. Defaults to F.
#' @param func (function) Which base function to call. Can be mean, median, sd, var and any other suitable function. Default to mean.
#' @param pattern (string) A pattern to use for finding the columns names.
#' @param ignore_NA (boolean) Whether to ignore missing data. Defaults to T.
#' @param progress (chr scalar) Progress parameter passed to aaply. Default is text.
#' @export
#' @examples
#' df_rowFunc(iris[-5]) #get means by row
#' all(df_rowFunc(iris[-5]) == rowMeans(iris[-5])) #equal to rowMeans
#' df_rowFunc(iris[-5], func = median) #rowMedians
df_rowFunc = function(..., standardize = F, func = mean, ignore_NA = T, progress = "text", pattern = NULL) {
  if (!is.null(pattern)) stop("pattern is depreciated. Use df_subset_by_pattern")

  #make df
  tmp_df = data.frame(...)

  #check for numericness
  if(!all(purrr::map_chr(tmp_df, class) %in% c("numeric", "integer"))) stop("Some variables were not numeric!")

  #standardize?
  if (standardize) tmp_df = df_standardize(tmp_df)

  #get results
  tryCatch({ #try to pass na.rm=T
    results = plyr::aaply(tmp_df, .margins = 1, .progress = progress, .expand = F,.fun = function(x) {
      get("func")(x %>% unlist, na.rm = ignore_NA)
    })},
    error = function(e) {
      results <<- plyr::aaply(tmp_df, .margins = 1, .expand = F, .progress = progress, .fun = function(x) {
        get("func")(x %>% unlist)
      })
    }
  )

  return(results)

}


#sort a df according to a variable
#just a minor edit of the function in reshape package. Preseres rownames.

#' Sort a data.frame by one or more variables. Can sort by decreasing or increasing order
#'
#' Sort a data.frame by one or more variables. Can sort by decreasing or increasing order. Improvement of the reshape::order because it preserves rownames and can sort by decreasing order as well.
#' @param df (df) A data.frame.
#' @param vars (str/int) variables to use for sorting.
#' @param decreasing Whether to use decreasing order. Default=F.
#' @export
#' @examples
#' head(df_sort(iris, 1)) #sort by the first variable, increasing order
#' head(df_sort(iris, 1, decreasing = T)) #sort by the first variable, decreasing order
#' head(df_sort(iris, "Species")) #sort by Species variable
df_sort = function (df, vars = names(df), decreasing = F){
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
#' @param weights (num vector) A vector of values to use for weights. If none given, all cases will be given equal weights.
#' @export
#' @examples
#' #generate some data
#' df = data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5))
#' weightsvar = runif(5)
#' #residualize the data.frame
#' df_residualize(df, resid.vars = "c") #without weights
#' df_residualize(df, resid.vars = "c", weights = weightsvar) #with weights
df_residualize = function(data, resid.vars, suffix = "", exclude.resid.vars = T, return.resid.vars = T, print.models = F, exclude_vars = NULL, weights = NA) {


  #save rownames
  v_rownames = rownames(data)

  #check names
  if (all(make.names(colnames(data)) != colnames(data))) stop("This function requires the data to have syntactically valid names. See ?make.names")

  #weights
  if (length(weights) == 1) {
    if (is.na(weights)) {
      wtds = rep(1, nrow(data))
    }
  } else {
    wtds = weights
  }

  #make exclusion vector
  v_excluded = exclude_vars
  if (exclude.resid.vars) {
    v_excluded = c(v_excluded, resid.vars)
  }

  # #the residuals function
  # lm_f = function(x) {
  #   x = residuals(lm(data = data, formula = update(x ~ 0, paste0("~", resid.vars)), na.action = na.exclude, weights = wtds))
  # }

  #calculate residuals
  resid = data.frame(matrix(nrow = nrow(data), ncol = ncol(data))) #make empty df same size as data
  colnames(resid) = colnames(data) #get colnames

  for (colname in colnames(data)) { #loop over colnames
    if (!colname %in% v_excluded) { #if colname isn't excluded
      form = stringr::str_c(colname, " ~ ", paste0(resid.vars)) %>% as.formula() #make formula
      if (print.models) message(form) #message if desired

      #get resids
      resid[, colname] = residuals(lm(data = data, formula = form, na.action = na.exclude, weights = wtds))
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

  if (is.null(names) & is.null(key)) stop("At least key or names parameters must be given!")
  if (!is.null(names) & !is.null(key)) stop("Both key and names were given. Only one of these can be given!")
  if (!is.null(names)) {
    if (is.null(new_name)) {
      new_name = names[1]
      message(sprintf("A new preferred rowname wasn't given. The first rowname was chosen for this purpose: `%s`", new_name))
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

  stop("Something went amiss! Debug this function!")
}


# split unsplit functions -----------------------------------------------------------------
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
      message("Not all columns were found in each data.frame. This may indicate an error.")
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
#' head(df_add_id(iris, "A")) #Add an ID column named "ID" filled with "A"
df_add_id = function(data, id, id_var = "ID") {
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
#' #rename one variable
#' names(df_rename(iris, "Sepal.Length", "Sepal_Length"))
#' #rename multiple variables
#' names(df_rename(iris, c("Sepal.Length", "Species"), c("SEPAL_LENGTH", "SPECIES")))
#' #randomly rename iris
#' df_rename(iris, current_names = names(iris), new_names = rev(names(iris)[sample(1:5)])) %>% names
#' #warning on non-existent names
#' head(df_rename(iris, "bleh", "blah"))
df_rename = function(data, current_names, new_names) {

  #check input
  is_(data, class="data.frame", error_on_false = T)
  is_(current_names, class="character", error_on_false = T)
  is_(new_names, class="character", error_on_false = T)

  #check that lengths match
  if (length(current_names) != length(new_names)) {
    stop("The vectors of names must be equally long!")
  }

  #check not too long
  if (any(duplicated(current_names))) stop("There were duplicate names in current_names!")
  if (any(duplicated(new_names))) stop("There were duplicate names in new_names!")

  #check if variables exist
  purrr::walk(current_names, function(x) {
    if (!x %in% names(data)) warning(sprintf("Variable %s was not in the data.", x), call. = F)
  })

  #main loop
  old_names = names(data)
  for (i in seq_along(current_names)) {
    cur = current_names[i]
    new = new_names[i]

    #skip if not present
    if (!cur %in% names(data)) next

    #position
    pos = which(old_names == cur)

    #rename
    names(data)[pos] = new
  }

  #return
  data
}




#' Remove variables from a data.frame
#'
#' Remove variables from a data.frame by name, position or logical.
#' @param data (data.frame) A data.frame.
#' @param vars (vector) The variables to remove. Can be logical, character or integer.
#' @export
#' @examples
#' #remove two variables by name
#' #does not work in base r by expected syntax:
#' #iris[-c("Sepal.Length", "Species")]
#' names(df_remove(iris, c("Sepal.Length", "Species")))
df_remove = function(data, vars) {
  #check input
  is_(data, class = "data.frame", error_on_false = T)
  is_(vars, class = c("character", "logical", "numeric"), error_on_false = T)

  #convert vars to names
  if (is.numeric(vars)) vars = names(data)[vars]
  if (is.logical(vars)) vars = names(data)[vars]

  #loop over and remove
  for (var in vars) {
    #not there or already removed?
    if (is.null(data[[var]])) warning(sprintf("Variable %s was either removed twice or was not there to begin with.", var))
    #remove
    data[[var]] = NULL
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
  if (!is.data.frame(data)) stop("data must be a data.frame!")
  if (!is_simple_vector(vars)) stop("vars must be a named vector!")
  if (is.null(names(vars))) stop("vars must be a named vector!")
  if (!all(names(vars) %in% names(data))) stop("Not all names were names of columns in the data.frame!")

  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars

  #more checks
  if (any(duplicated(var.nms)) | any(duplicated(var.pos))) stop("Some names/positions were identical! You cannot move the same column to multiple places!")

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
#' @param prefix (str) A prefix to add.
#' @param suffix (str) A suffix to add.
#' @export
#' @examples
#' test_iris = iris[1:10, ] #small test dataset
#' df_add_affix(test_iris, prefix = "P_") #ad P_ prefix
#' df_add_affix(test_iris, suffix = "_S") #ad _S suffix
df_add_affix = function(data, prefix = "", suffix = "") {
  #rename
  colnames(data) = paste0(prefix, colnames(data), suffix)
  #return
  data
}


#' Apply function to columns of a data.frame
#'
#' Apply functions to columns of a data.frame. This can be done selectively using colnames, logical indicates, integer indices or regex patterns. One can also pass extra arguments to the function. By default, the unselected columns are kept.
#' @param data (an object) An object whose colnames should be changed
#' @param func (function) A function to apply.
#' @param indices (chr, num, log vector) Either a character, numeric or logical vector used to select columns.
#' @param pattern (chr sclr) A regex pattern used to match column names.
#' @param pattern_inverse (chr sclr) Whether to invert the match via regex pattern. Default=F.
#' @param keep_unselected (log sclr) Whether to keep the unselected columns. Default=T.
#' @param ... Other parameters passed to func.
#' @export
#' @examples
#' #notice original numbers
#' head(iris)
#' multiply_by_zero = function(x) return(x*0) #auxiliary function
#' #using regex
#' head(df_colFunc(iris, func = multiply_by_zero, pattern = "Length"))
#' #using inverse regex
#' head(df_colFunc(iris, func = multiply_by_zero, pattern = "Species", pattern_inverse = T))
#' #using integer indices
#' head(df_colFunc(iris, func = multiply_by_zero, indices = 2:3))
#' #using logical indices
#' head(df_colFunc(iris, func = multiply_by_zero, indices = c(T, F, T, F, F)))
#' #using characters
#' head(df_colFunc(iris, func = multiply_by_zero, indices = c("Sepal.Length", "Petal.Width")))
#' #removing unselected columns
#' head(df_colFunc(iris, func = multiply_by_zero, pattern = "Length", keep_unselected = F))
#' #select all by not providing any selector
#' str(df_colFunc(iris, func = as.character)) #all have been changed to chr
df_colFunc = function(data, func, indices = NULL, pattern = NULL, pattern_inverse = F, keep_unselected = T, ...) {

  #force
  data
  func

  #type
  data = as.data.frame(data)

  #other input
  if (!is.null(pattern) & !is.null(indices)) stop("Only one of indices or pattern can be given!")
  if (is.null(pattern) & is.null(indices)) indices = names(data)
  if (!is.null(indices)) is_(indices, class = c("character", "logical", "numeric", "integer"), error_on_false = T)

  #which cols?
  if (!is.null(pattern)) {
    #find names by regex

    indices = str_detect2(names(data), pattern = pattern, value = T)
    #inverse if desired
    if (pattern_inverse) indices = setdiff(names(data), indices)
  }

  if (!is.null(indices)) {
    #chr indices
    if (is.character(indices)) {
      if (!all(indices %in% names(data))) {
        v_missing_names = setdiff(indices, names(data)) %>% stringr::str_c(collapse = ", ")
        stop("Some colnames did not exist in the data.frame!: " + v_missing_names)
      }
    }
    #logical indices
    if (is.logical(indices)) {
      #check length
      if (length(indices) != length(names(data))) stop("The length of the logical indices did not match the data.frame!")
      indices = names(data)[indices] #convert to chr indices
    }
    #integer indices
    if (is.numeric(indices)) {
      #check if are integers
      if (!all(is_whole_number(indices))) stop("Numeric indices must be integers or converible to integers without loss!")
      #check if all exist
      if (!all(indices %in% seq_along(data))) stop("Numeric indices must not be larger than the data.frame!")
      indices = names(data)[indices] #convert to chr indices
    }
  }

  #loop over variables and apply func
  for (col in indices) {
    data[[col]] = func(data[[col]], ...)
  }

  #keep unselected?
  if (!keep_unselected) {
    data = df_remove(data, setdiff(names(data), indices))
  }

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
  if (!method %in% c("pure_tidyr", "custom")) stop("The selected method does not exist!")
  #the pattern input will be checked by the sub-function

  #determine the groups of variables
  colnames_data = colnames(data)
  l_memberships = group_by_pattern(colnames_data, pattern = pattern)
  #remove the varying part
  colnames_remainder = stringr::str_replace(colnames_data, pattern = pattern, replacement = "")

  if (method == "custom") {
    #combine the groups using gather
    l_combined = mapply(members = l_memberships, name = names(l_memberships), FUN = function(members, name) {
      # browser()
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
    # browser()
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

  stop("You somehow managed to break this function!")
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
  if (!(stringr::str_detect(pattern, pattern = "\\(") & stringr::str_detect(pattern, pattern = "\\)"))) stop("The given pattern did not have a capturing group!")

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
#' df_subset_by_pattern(iris, "Length") # length columns
#' df_subset_by_pattern(iris, "Length", T) # non-length columns
df_subset_by_pattern = function(data, pattern, inverse = FALSE) {
  if (!inverse) return(data[, stringr::str_detect(colnames(data), as.character(pattern)), drop = FALSE])
  if (inverse) return(data[, !stringr::str_detect(colnames(data), as.character(pattern)), drop = FALSE])
}



#' Remove NA columns
#'
#' Detect variables with no data in a data.frame and remove them. Can be given a vector of names or indices of columns to keep no matter what.
#' @param data (data.frame) Data to subset.
#' @param keep (num or chr vector) A vector of names of columns to keep no matter what. Can also be numeric indices which are then replaced with the colnames.
#' @return Returns the subset of the object.
#' @export
#' @examples
#' df_remove_NA_vars(data.frame(a = NA, b = 1))
#' df_remove_NA_vars(data.frame(a = NA, b = 1), keep = "a")
#' df_remove_NA_vars(data.frame(a = NA, b = 1), keep = 1)
df_remove_NA_vars = function(data, keep = "") {

  #numeric
  if (is.numeric(keep)) keep = colnames(data)[keep]

  #which to remove
  v_remove = purrr::map_lgl(data, function(x) {
    all(is.na(x))
  })

  #subset
  data[!v_remove | colnames(data) %in% keep]
}


#' Subset a data.frame flexibilly
#'
#' Subsets a data.frame by a vector of variables given as names. Only subsets the ones found in the data.
#'
#' Subsets the intersection of vars and the columns in the data. Throws a warning if no overlap. Subsets using drop=F.
#' @param data (data.frame) Data to subset.
#' @param vars (chr vectr) Variables to subset.
#' @return Returns a data.frame that is a subset of the original.
#' @export
#' @examples
#'df_flexsubset(iris, c("Species")) %>% str
#'df_flexsubset(iris, c("Species", "Sepal.Length")) %>% str
#'df_flexsubset(iris, c("Species", "test")) %>% str
#'df_flexsubset(iris, c("test")) %>% str
df_flexsubset = function(data, vars, messages = T) {
  #check input
  is_(data, class = "data.frame", error_on_false = T)
  is_(vars, class = "character", error_on_false = T)
  is_(messages, class = "logical", size = 1, error_on_false = T)

  #determine overlap
  # vars_overlap = intersect(names(data), vars) #this changes the order
  vars_overlap = purrr::map_chr(vars, function(x) {
    if (x %in% names(data)) return(x)
    NA
  }) %>% na.omit %>% as.vector
  # vars_nonoverlap = setdiff(vars, names(data))
  vars_nonoverlap = purrr::map_chr(vars, function(x) {
    if (!x %in% names(data)) return(x)
    NA
  }) %>% na.omit %>% as.vector

  #warning on 0
  if (length(vars_overlap) == 0) warning("There was no overlap in columns! Returning a 0-column data.frame. This may be an error.")

  #messages
  if (messages & length(vars_nonoverlap) > 0) message("The following variables were not found: " + stringr::str_c(vars_nonoverlap, collapse = ", "))

  #return
  data[, vars_overlap, drop = F]
}

# df_merge_cols -----------------------------------------------

#' Merge columns
#'
#' Combines columns by filling in missing values with values from other columns in a designated order.
#' @param df (df) Data frame.
#' @param cols (chr) Variable names to merge.
#' @return A vector.
#' @export
#' @examples
#' data_frame(
#' a = c(1, NA, NA),
#' b = c(-1, 2, NA),
#' c = c(-5, -9, 3)
#' ) %>% df_merge_cols(letters[1:3])
df_merge_cols = function(df, cols) {
  df
  cols

  #1 col? use that
  if (length(cols) == 1) return(df[[cols]])

  #0 col? error
  if (length(cols) == 0) stop("cols must be a length ≥1 chr vector")

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



# df_fct_split -----------------------------------------------
#an improvement as model.matrix

#' Split factors into multiple columns
#'
#' Split factors into multiple columns
#'
#' This function is a more user-friendly version of `model.matrix`.
#'
#' For the prefix, one can use `%v` to refer to the variable name.
#' @param df (df) A aata frame.
#' @param fcts (chr) Names of columns to split up.
#' @param prefix (chr) A prefix to add.
#' @param warn_unused_levels (lgl) Whether to warn the user when some levels have no cases.
#' @param type (chr) Which type of column should be created.
#' @param rm_old (lgl) Whether to remove the old columns after the splitting.
#' @return A modified data frame.
#' @export
#' @examples
#' df_fct_split(iris, "Species")
df_fct_split = function(df, fcts, prefix = "", warn_unused_levels = T, type = "l", rm_old = F) {
  #input
  df
  fcts
  assertthat::assert_that(is_scalar_character(type))
  if (is.null(prefix)) prefix = ""

  #loop over fcts
  for (fct in fcts) {
    #does not exist
    if (!fct %in% colnames(df)) stop(sprintf("Variable was not found: `%s`", fct))

    #not a factor
    if (!is.factor(df[[fct]])) stop(sprintf("Variable was not a factor: `%s`", fct))

    #get levels
    fct_levels = table2(df[[fct]], include_NA = F)

    #any unused?
    if (any(fct_levels[["Count"]] == 0)) {
      unused_levels = fct_levels %>% filter(Count == 0) %>% `[[`("Group")
      warning(sprintf("There were unused factor levels in factor `%s`: `%s`", fct, str_c(unused_levels, collapse = ", ")))
    }

    #loop over levels
    for (lvl in fct_levels$Group) {
      #add new dichotomous categorical variable
      #make prefix
      prefix_this = str_replace_all(prefix, "%v", fct)
      new_colname = prefix_this + lvl

      #error if duplicate
      if (new_colname %in% colnames(df)) stop(sprintf("Duplicate colname detected: %s", new_colname))

      #switch
      switch(type,
             "l" = {df[[new_colname]] = (df[[fct]] == lvl)},
             "f" = {df[[new_colname]] = (df[[fct]] == lvl) %>% as.factor},
             "n" = {df[[new_colname]] = (df[[fct]] == lvl) %>% as.numeric},
             "i" = {df[[new_colname]] = (df[[fct]] == lvl) %>% as.integer}
      )
    }

    #remove old?
    if (rm_old) {
      df[[fct]] = NULL
    }

  }


  df
}

