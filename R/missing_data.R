## FUNCTIONS FOR DEALING WITH MISSING DATA


#' Count missing data
#'
#' A simple wrapper for \code{\link{is.na}} and \code{\link{sum}}.
#' @param x (any suitable object) An object for which to count NAs.
#' @param reverse (logical scalar) Whether to count non-NAs instead (default false).
#' @return An integer.
#' @export
#' @examples
#' m = matrix(c(NA, 1:3, NA, 4:6, NA), nrow = 3)
#' miss_count(m)
#' miss_count(m, reverse = T)
#' miss_count(m, prop = T)
#' miss_count(m, reverse = T, prop = T)
miss_count = function(x, reverse = F, prop = F) {

  if (!reverse) {
    y = sum(is.na(x))
  } else {
   y = sum(!is.na(x))
  }

  #prop
  if (prop) return(y / length(x))

  y
}


#' Missing datapoint counter, case-level
#'
#' Counts the number of missing datapoints per case
#' @param x (data frame) The data.
#' @param reverse (lgl) Count non-NA instead.
#' @export
#' @examples
#' miss_by_case(iris)
#' miss_by_case(miss_add_random(iris))
#' miss_by_case(miss_add_random(iris), reverse = T)
#' miss_by_case(miss_add_random(iris), prop = T)
miss_by_case = function(x, reverse = F, prop = F){
  #miss status
  x_miss = is.na(x)

  #reverse
  if (reverse) x_miss = !x_miss

  #count missing data by row
  y = apply(x_miss, 1, sum)

  #if desired, convert to fraction
  if (prop) return(y / ncol(x))

  y
}


#' Missing datapoint counter, variable-level
#'
#' Counts the number of missing datapoints per variable
#' @param x a matrix or data.frame
#' @param reverse (lgl) Count non-NA instead.
#' @export
#' @examples
#' miss_by_var(iris)
#' miss_by_var(miss_add_random(iris))
#' miss_by_var(miss_add_random(iris), reverse = T)
#' miss_by_var(miss_add_random(iris), prop = T)
miss_by_var = function(x, reverse = F, prop = F){
  #miss status
  x_miss = is.na(x)

  #reverse
  if (reverse) x_miss = !x_miss

  #count missing data by column
  y = apply(x_miss, 2, sum)

  #if desired, convert to fraction
  if (prop) return(y / nrow(x))

  y
}




#' Missing data barplot with ggplot2.
#'
#' Returns a ggplot2 object of the missing data.
#' @param data (data.frame) Data.frame.
#' @param percent (lgl scalar) Whether to use percent or raw counts.
#' @param case (lgl scalar) Whether to plot missingness for cases or variables.
#' @param reverse (lgl sclr) Whether to count datapoints instead of missing datapoints.
#' @export
#' @examples
#' test_data = miss_add_random(iris)
#' miss_plot(test_data)
#' miss_plot(test_data, percent = F) #raw count
#' miss_plot(test_data, case = F) #variables
#' miss_plot(test_data, case = F, percent = F) #variables, raw
#' miss_plot(test_data, reverse = T) #reverse
miss_plot = function(data, percent=T, case=T, reverse=F) {
  #cases or vars?
  if (case) {
    m = miss_by_case(data, reverse = reverse)
    d = table(m) %>% as.data.frame()
    names(d) = c("number_miss", "count")
  } else {
    m = miss_by_var(data, reverse = reverse)
    d = data.frame(number_miss = m, var = names(m))
  }

  #summarize
  max.miss = max(m)
  min.miss = min(m)

  #plot
  if (case) {
    #percent?
    if (percent) d$count %<>% divide_by(nrow(data))

    g = ggplot2::ggplot(d, aes(number_miss, count)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(breaks = min.miss:max.miss) #always keep entire range


    #percent
    if (percent) g = g + scale_y_continuous(labels = scales::percent, name = "Percent")
    if (!percent) g = g + ylab("Count")

    #xlab
    if (!reverse) {
      g = g + xlab("Number of missing values")
    } else {
      g = g + xlab("Number of datapoints")
    }

  } else {
    #percent?
    if (percent) d$number_miss %<>% divide_by(nrow(data))

    #reorder factor by missing
    d$var %<>% forcats::fct_reorder(-d$number_miss)

    #plot
    g = ggplot2::ggplot(d, aes(var, number_miss)) +
      geom_bar(stat = "identity") +
      xlab("Variable")

    #percent
    if (percent) {
      g = g + scale_y_continuous(name = "Percent missing values", labels = scales::percent)
    } else {
      g = g + scale_y_continuous(name = "Number of missing values")
    }

  }



  #return
  g + theme_bw()
}


#' Wrapper for matrixplot()
#'
#' Wrapper for matrixplot() from VIM, but automatically substrings the variable names to 8 characters.
#' @param df a data.frame.
#' @export
#' @examples
#' matrixplot2(miss_add_random(iris))
matrixplot2 = function(df) {
  	return(VIM::matrixplot(df, labels=substr(colnames(df), 1, 8)))
}


#' Missing data matrix
#'
#' Create the missing data matrix. Not so useful in itself, but useful for other functions.
#' @param data (data.frame/matrix) The data.
#' @return A matrix where missing/present values are coded as 1/0.
#' @export
#' @examples
#' miss_matrix(miss_add_random(iris))
miss_matrix = function(data) {
  m_d = data %>% as.matrix %>% is.na() %>% as.numeric() %>% matrix(ncol = ncol(data))
  copy_names(x = data, y = m_d)

  m_d
}

#' How much data is missing?
#'
#' Calculates the proportions of missing data by case, variable and cell.
#' @param data (data.frame/matrix) The data.
#' @return A length 3 vector with the proportions.
#' @export
#' @examples
#' miss_amount(miss_add_random(iris))
miss_amount = function(data) {
  #fast shortcut
  if (!anyNA(data)) {
    c(
      "cases with missing data" = 0,
      "vars with missing data" = 0,
      "cells with missing data" = 0
      )
  }

  #calculate amount of missing
  by_case = miss_by_case(data) %>% percent_cutoff(cutoffs = 1)
  by_var = miss_by_var(data) %>% percent_cutoff(cutoffs = 1)
  overall = miss_count(data) / total_cells(data)

  c("cases with missing data" = as.vector(by_case),
    "vars with missing data" = as.vector(by_var),
    "cells with missing data" = overall)
}


#' Calculates the patterns of missing data
#'
#' Calculates the patterns of missing data. This is the missing data matrix multiplied row-wise by a vector of the 2 to the 1 to n power, where n is the number of variables. This results in a unique id for each way the data can be missing.
#' @param data (data.frame/matrix) The data.
#' @return A vector of patterns the same length as the number of rows in data.
#' @export
#' @examples
#' miss_pattern(miss_add_random(iris))
miss_pattern = function(data) {
  #create missing matrix
  m_d = miss_matrix(data)

  #multiply by 2^i, row wise
  (t(m_d) * (purrr::map_dbl(1:ncol(m_d), function(i) 2^i))) %>% #transpose, then multiply by 2^i
    t() %>% #transpose back
    apply(MARGIN = 1, FUN = sum) #get sum by rows
}


#' Calculate the complexity of missing data
#'
#' Calculates the complexity of the patterns of missing data in the dataset.
#'
#' This method was proposed by McKnight et al (2007) [Missing Data: A Gentle Introduction].
#' @param data (data.frame/matrix) The data.
#' @return A length 3 vector with the proportions.
#' @export
#' @examples
#' miss_complexity(miss_add_random(iris))
miss_complexity = function(data) {
  #number of unique patterns
  v_uniq = miss_pattern(data) %>% table() %>% unique() %>% length()

  v_uniq / nrow(data)
}


#' Missing data dependencies
#'
#' Calculates Cohen's d or equivalent for every variable pair. Larger values mean that cases with missing data in one variable differ from other cases in the other variable, thus that the data are not missing at random (MCAR).
#'
#' This method was proposed by McKnight et al (2007) [Missing Data: A Gentle Introduction].
#' @param data (df/mat) Data.
#' @param robust (lgl scalar) Whether to use robust measures (default false). If true, will use median/mad instead of mean/sd to calculate the standardized mean differences.
#' @return A data frame of size n x n where n is the number of variables in data. The rows are the gropuing variable (missing vs. not-missing) and the columns are the outcome variables.
#' @export
#' @examples
#' miss_analyze(miss_add_random(iris))
miss_analyze = function(data, robust = F) {
  #data
  data = as.data.frame(data)

  #miss matrix
  m_d = miss_matrix(data)

  #for each variable, analyze relationship each other variable
  d_NA_diffs = plyr::ldply(.data = colnames(data), function(var) {
    #for each variable
    purrr::map_dbl(colnames(data), function(var2) {
      #if same
      if (var == var2) return(NA)

      #if no missing data
      if (all(m_d[, var] == 0)) return(NA)

      #if factor, use cramer's V
      if (data[[var2]] %>% is.factor) {

        return(silence(lsr::cramersV(m_d[, var], data[[var2]]) %>%
                         compute.es::res(r = ., n = 100, verbose = F)) %>%
                 magrittr::extract("d") %>%
                 unlist() %>%
                 as.vector()) #silence to avoid chi sq warnings
      }

      #if numeric, use standardized mean difference
      if (!robust) { #parametric measures
        d_val = SMD_matrix(x = data[[var2]], group = m_d[, var])[1, 2]
      } else { #robust measures
        d_val = SMD_matrix(x = data[[var2]], group = m_d[, var], central_tendency = median, dispersion = mad)[1, 2]
      }

      d_val
    })
  })

  #names
  rownames(d_NA_diffs) = colnames(d_NA_diffs)

  d_NA_diffs
}


#func from https://trinkerrstuff.wordpress.com/2012/05/02/function-to-generate-a-random-data-set/

#' Insert random NAs into a data.frame.
#'
#' Inserts missing data into a data.frame at random, thus creating data that are Missing Completely At Random (MCAR). THis isn't how data usually are missing in the real world, but may be sufficient for some situations.
#' @param df (data.frame) A data.frame.
#' @param prop (num vector) The proportion of NAs to add.
#' @return A data.frame.
#' @export
#' @examples
#' df = data.frame(c(1:10), letters[1:10]) #example data
#' miss_add_random(df) #add 10% random NAs
#' miss_amount(df) #verify that 10% really is missing
miss_add_random =  function(df, prop = .1) {
  #if nothing, skip
  if (prop == 0) return(df)

  #add missing
  n = nrow(df)
  m = ncol(df)
  num.to.na = ceiling(prop*n*m)
  id = sample(0:(m*n-1), num.to.na, replace = FALSE)
  rows = id %/% m + 1
  cols = id %% m + 1

  #walk and insert
  purrr::walk(seq(num.to.na), function(x) {
    df[rows[x], cols[x]] <<- NA
  } )

  return(df)
}


#' Impute data using VIM::irmi
#'
#' Useful wrapper for VIM's irmi function. Can skip cases without changing case order depending on the number of missing values
#'
#' @param data (data.frame) A data.frame.
#' @param max_na (num scalar) The maximum number of missing datapoints per case.
#' @param method_args Arguments to forward to the imputation call.
#' @param leave_out Names of variables to leave out of the imputation such as ids.
#' @param method rf (missForest) or irmi (VIM)
#'
#' @return A data frame with missing data imputed for the desired cases.
#' @export
#' @examples
#' df = miss_add_random(iris[-5]) #example data, remove data at random from iris num data
#' miss_impute(df) #impute missing
#' miss_impute(df, method = "irmi") #with irmi
#' #preserves rownames for ease of use
#' df = data.frame(a = rnorm(5), b = rnorm(5), c = c(1, NA, NA, 1, 4)) %>% set_rownames(letters[1:5])
#' miss_impute(df)
miss_impute = function(data, max_na = floor(ncol(data)/2), method = "irmi", method_args = NULL, leave_out = c()) {

  #tibbles do not work here
  was_tibble = is_tibble(data)
  data = as.data.frame(data)

  #save rownames
  .rownames = rownames(data)

  #exclude variables?
  if (length(leave_out) > 0) {
    orig_var_order = names(data)
    left_out_data = data %>% select(!!leave_out)
    data = data %>% select(-!!leave_out)
  }

  #convert ordered with <3 levels to factors
  needs_conversion = purrr::map_lgl(data, ~is.ordered(.) && nlevels(.) < 3)
  if (any(needs_conversion)) {
    message("Some ordered variables had fewer than 3 levels and must be converted to ordinary factors for imputation to work: " + stringr::str_c(names(data)[needs_conversion], collapse = ", "))

    #convert
    data[] = lapply(data, function(v) {
      if (is.ordered(v) && nlevels(v) < 3) {
        return(factor(v, ordered = F))
      }

      v
    })
  }

  #characters are not good, throw a warning
  chr_vars = purrr::map_lgl(data, is.character)
  if (any(chr_vars)) {
    warning(str_glue("There are character type variables. You probably want to convert these to factors or exclude"), call. = F)
  }

  #exclude cases?
  case_na = miss_by_case(data)
  exclusion = any(case_na > max_na)
  if (exclusion) {
    #cases excluded
    cases_excl = case_na > max_na

    #add id
    case_ids = (1:nrow(data)) %>% as.character
    # data$.tmpid = case_ids
    # rownames(data) = case_ids

    #save excluded cases
    data_excl = data[cases_excl, ]

    #non-excluded cases
    data = data[!cases_excl, ]
  }

  #impute
  if (method == "rf") {
    method_args = c(list(xmis = data), method_args)
    data = do.call(missForest::missForest, args = method_args)$ximp
  } else if (method == "irmi") {
    method_args = c(list(x = data, imp_var = F, noise = F), method_args)
    data = do.call(VIM::irmi, args = method_args)
    # VIM::irmi(data, noise = noise, imp_var = F)
  } else {
    stop(str_glue("`method` not recognized {method}, must be either 'rf' or 'irmi'"), call. = F)
  }


  #add back
  if (exclusion) {
    #rbind
    data = rbind(data, data_excl)

    #sort by ids
    data$.tmpid = as.numeric(c(case_ids[!cases_excl], case_ids[cases_excl]))
    data = dplyr::arrange(data, .tmpid)

    #remove ids
    data$.tmpid = NULL
  }

  #exclude variables?
  if (length(leave_out) > 0) {
    #add back in
    data = bind_cols(data, left_out_data)
    #reorder
    data = data[orig_var_order]
  }

  #set original rownames if not tibble
  rownames(data) = .rownames

  #set to tibble if it was before
  if (was_tibble) data = as_tibble(data)

  #return
  data
}


#' Filter data by missing values per row.
#'
#' Counts the number of missing values per row and then keeps rows that have at most a chosen number of missing values.
#' @param data (data frame) The data.
#' @param missing (num) The maximum number of missing values in cases. Defaults to 0 (keep only cases with no missing values). If this is a proportion between 0-1, then it is used as such.
#' @param reverse (lgl) Filter based on non-NA data instead.
#' @export
#' @examples
#' set.seed(1)
#' df = data.frame(ints = 1:10, letters = letters[1:10], unif = runif(10), norm = rnorm(10))
#' df = miss_add_random(df, prop = .25) %>% mutate(nomiss = T, allmiss = NA)
#' df
#' #filter cases/rows
#' miss_filter(df) #allow no missing values
#' miss_filter(df, missing = 1) #allow up to 1 value missing
#' miss_filter(df, missing = 0.5) #allow up to a half missing
#' #only use certain variables for counting
#' miss_filter(df, vars = c("ints", "letters"))
#' #filter columns/vars
#' miss_filter(df, missing = 0, by_case = F)
#' miss_filter(df, missing = 0.5, by_case = F)
miss_filter = function(data, missing = 0, reverse = F, by_case = T, vars = NULL) {
  #initial
  assertthat::assert_that(is.numeric(missing) && length(missing) == 1)
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is_logical(reverse, scalar = T))

  #which variables to include in the filtering?
  if (is.null(vars)) {
    vars = names(data)
  } else {
    if (!by_case) {
      warning("`vars` argument will not affect results when `by_case` = FALSE", call. = F)
    }
  }
  assert_that(all(vars %in% names(data)), msg = str_glue("Some `vars` were not in `data`: {str_c(setdiff(vars, names(data), collapse = ', ')}"))

  #subset data to the variables we use for counting
  y = data[, vars, drop = F]

  #by row or by column?
  if (by_case) {
    #if proportion
    if (!is_whole_number(missing)) {
      if (reverse) missing = 1 - missing
      missing = floor(missing * length(vars))
    }

    #compute missing values by case
    y_miss = miss_by_case(y)

    #filter
    if (!reverse) return(data[y_miss <= missing, ])

    return(data[y_miss >= missing, ])
  } else { #variable subsetting
    #if proportion
    if (!is_whole_number(missing)) {
      if (reverse) missing = 1 - missing
      missing = floor(missing * nrow(data))
    }

    #compute missing values by column
    y_miss = miss_by_var(data)

    #return the data subset based on the missing values
    if (!reverse) return(data[, y_miss <= missing, drop = F])

    #do the same, but with reverse = T
    return(data[, y_miss >= missing, drop = F])
  }

}




#' Calculate proportion missing data by group variables
#'
#' @param data Data frame
#' @param grouping_vars Names of grouping variables
#' @param vars Names of data variables
#'
#' @return A data frame
#' @export
miss_by_group = function(data, grouping_vars, vars = NULL) {
  # browser()
  data
  grouping_vars
  #vars in data
  assertthat::assert_that(all(vars %in% names(data)), msg = "Some `vars` were not in `data`.")
  assertthat::assert_that(all(grouping_vars %in% names(data)), msg = "Some `grouping_vars` were not in `data`.")

  #if no data vars given, assume everything else than grouping
  if (is.null(vars)) {
    vars = setdiff(names(data), grouping_vars)
  } else {
    assertthat::assert_that(all(grouping_vars %in% names(data)))
  }

  data[c(grouping_vars, vars)] %>%
    plyr::ddply(grouping_vars, function(d) {
      y = tibble(
        n = nrow(d)
      )

      #loop and add
      for (v in vars) {
        y[[v]] = mean(!is.na(d[[v]]))
      }

      y
    })
}



#fill in missing values based on other variables/vectors

#' Fill in missing values based on other variables/vectors
#'
#' Handy function to imputing missing values based on values from other sources instead of statistical methods.
#'
#' @param ... Can be a data frame, list of vectors, or just vectors. Vectors must have same length.
#'
#' @return A vector with imputed values
#' @export
#'
#' @examples
#' list(c(1, NA, NA), c(9, 2, NA), c(9, 9, 3)) %>% miss_fill()
miss_fill = function(...) {

  x = NULL
  #list
  lst = list(...)

  #was already a list non-data frame?
  if (length(lst) == 1 && is.list(lst[[1]]) & !is.data.frame(lst[[1]])) lst = lst[[1]]

  #is a data frame?
  if (length(lst) == 1 && is.data.frame(lst[[1]])) {
    x = lst[[1]]
  }

  #is it vectors?
  if (is.null(x) && all(purrr::map_lgl(lst, is.atomic))) {
    if (all_the_same(purrr::map_int(lst, length))) {
      x = data.frame(lst, stringsAsFactors = F)
    } else {
      stop("When inputting vectors, all the vectors must be the same length, but they were not", call. = F)
    }
  }

  #something else entirely?
  if (is.null(x)) stop("Bad input, check your input", call. = F)

  #insert by loop
  for (v in seq_along(x)) {
    #skip 1st
    if (v == 1) next

    #insert missings
    replaceable = is.na(x[[1]]) & !is.na(x[[v]])
    x[[1]][replaceable] = x[[v]][replaceable]
  }

  x[[1]]
}


#' Combine duplicate variables from a join back into single variables
#'
#' Looks for variable pairs with names ending in ".x" and ".y" and combines them into a single variable. This is done so that no new missing data is introduced. Will not work for second order duplicates from joins, e.g. var.x.x.
#'
#' @param x A data frame
#' @param vars A character vector of variable names to consider. If NULL, all variables are considered.
#' @param priority Which variable to prioritize when combining. Default is "x".
#'
#' @return A data frame
#' @export
#'
#' @examples
#' d1 = tibble(
#' id = 1:3,
#' y = c(1, 2, 3),
#' x = c(1, NA, NA)
#' )
#'
#' d2 = tibble(
#' id = 1:3,
#' x = c(NA, 2, NA),
#' z = c(1, 2, 3)
#' )
#'
#' d3 = tibble(
#' id = 1:3,
#' x = c(NA, NA, 3),
#' a = letters[1:3]
#' )
#'
#' d1 %>%
#'   left_join(d2, by = "id") %>%
#'   miss_combine_duplicate_vars() %>%
#'   left_join(d3, by = "id") %>%
#'   miss_combine_duplicate_vars()
miss_combine_duplicate_vars = function(x, vars = NULL, priority = "x") {
  #which are duplicated from a join?
  if (is.null(vars)) {
    vars = names(x)
  }

  #error if there are illegal, duplicate variable names present
  if (any(duplicated(vars))) {
    stop("There are duplicate variable names present in the data. This function cannot handle this.", call. = F)
  }

  #duplicates
  dups_table = vars %>%
    str_subset("\\.[xy]$") %>%
    str_replace_all("\\.[xy]$", "") %>%
    table2(include_NA = F)

  #loop
  for (i_dup in dups_table$Group) {

    #check if both are present
    i_dup_x = str_c(i_dup, ".x")
    i_dup_y = str_c(i_dup, ".y")

    i_dup_x_exists = i_dup_x %in% vars
    i_dup_y_exists = i_dup_y %in% vars

    #which to use as the base
    if (priority == "x") {
      x[[i_dup]] = miss_fill(x[[i_dup_x]], x[[i_dup_y]])
    } else {
      x[[i_dup]] = miss_fill(x[[i_dup_y]], x[[i_dup_x]])
    }

    #remove the dups
    x[[str_c(i_dup, ".x")]] = NULL
    x[[str_c(i_dup, ".y")]] = NULL
  }

  x
}


#avoid zoo dependency

#' Last observation carried forward
#'
#' @param x A vector
#' @param reverse Whether to do it in reverse
#'
#' @return A vector
#' @export
#'
#' @examples
#' c(NA, 1, NA, 2, NA) %>% miss_locf()
#' c(NA, 1, NA, 2, NA) %>% miss_locf(reverse = T)
#' c(NA, 1, NA, 2, NA, NA, NA) %>% miss_locf()
miss_locf = function(x, reverse = F) {
  #reverse?
  if (reverse) x = rev(x)

  #recode NA
  #these are kept distinct by rle() by default for same reason ???
  x_class = class(x)
  x[is.na(x)] = "___tmp"

  #run level encoding
  x_rle = rle(x)

  #swap values for NAs
  which_na = which(x_rle$values == "___tmp")

  #skip 1st
  which_na = setdiff(which_na, 1)

  #replace values
  x_rle$values[which_na] = x_rle$values[which_na - 1]

  #back to normal
  y = inverse.rle(x_rle)

  #NA recode
  y[y == "___tmp"] = NA

  #fix type/class
  if (x_class[1] == "logical") y = as.logical(y)
  if (x_class[1] == "integer") y = as.integer(y)
  if (x_class[1] == "numeric") y = as.double(y)
  if (x_class[1] == "factor") y = factor(y, levels = levels(x))
  if (x_class[1] == "ordered") y = ordered(y, levels = levels(x))

  #reverse?
  if (reverse) y = rev(y)

  y
}

