## FUNCTIONS FOR DEALING WITH MISSING DATA


#' Count missing data
#'
#' A simple wrapper for \code{\link{is.na}} and \code{\link{sum}}.
#' @param x (any suitable object) An object for which to count NAs.
#' @param reverse (logical scalar) Whether to count non-NAs instead (default false).
#' @return An integer.
#' @export
#' @examples
#' m = matrix(c(NA, 1:3, NA, 4:6, NA), nrow=3)
#' count_NA(m)
count_NA = function(x, reverse = F) {
  if (reverse) return(sum(!is.na(x)))
  sum(is.na(x))
}


#' Missing datapoint counter, case-level
#'
#' Counts the number of missing datapoints per case
#' @param x (data frame) The data.
#' @param reverse (lgl) Count non-NA instead.
#' @export
#' @examples
#' miss_by_case(miss_add_random(iris))
#' miss_by_case(iris)
#' miss_by_case(iris, reverse = T)
miss_by_case = function(x, reverse = F){
  apply(x, 1, count_NA, reverse = reverse)
}


#' Missing datapoint counter, variable-level
#'
#' Counts the number of missing datapoints per variable
#' @param x a matrix or data.frame
#' @param reverse (lgl) Count non-NA instead.
#' @export
#' @examples
#' miss_by_var(miss_add_random(iris))
#' miss_by_var(iris)
#' miss_by_var(iris, reverse = T)
miss_by_var = function(x, reverse = F){
  apply(x, 2, count_NA, reverse = reverse)
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
miss_amount = function(data, round = 2) {
  #calculate amount of missing
  by_case = miss_by_case(data) %>% percent_cutoff(cutoffs = 1)
  by_var = miss_by_var(data) %>% percent_cutoff(cutoffs = 1)
  overall = count_NA(data) / total_cells(data)

  c("cases with missing data" = as.vector(by_case),
    "vars with missing data" = as.vector(by_var),
    "cells with missing data" = overall) %>% round(round)
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
  (t(m_d) * (sapply(1:ncol(m_d), function(i) 2^i))) %>% #transpose, then multiply by 2^i
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
  d_NA_diffs = plyr::ldply(.data = colnames(data), .fun = function(var) {
    #for each variable
    sapply(colnames(data), FUN = function(var2) {
      #if same
      if (var == var2) return(NA)

      #if no missing data
      if (all(m_d[, var] == 0)) return(NA)

      #if factor, use cramer's V
      if (data[[var2]] %>% is.factor()) {

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
miss_add_random =  function(df, prop = .1){
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


#' Impute data using VIM::irmi
#'
#' Useful wrapper for VIM's irmi function. Can skip cases without changing case order depending on the number of missing values
#' @param data (data.frame) A data.frame.
#' @param max_na (num scalar) The maximum number of missing datapoints per case.
#' @return A data.frame with missing data imputed for the desired cases.
#' @export
#' @examples
#' df = miss_add_random(iris[-5]) #example data, remove data at random from iris num data
#' miss_impute(df) #impute missing
miss_impute = function(data, max_na = floor(ncol(data)/2), noise = F) {
  #tibbles do not work here
  data = as.data.frame(data)
  #exclude?
  case_na = miss_by_case(data)
  exclusion = any(case_na > max_na)
  if (exclusion) {
    #cases excluded
    cases_excl = case_na > max_na

    #add id
    case_ids = (1:nrow(data)) %>% as.character()
    # data$.tmpid = case_ids
    # rownames(data) = case_ids

    #save excluded cases
    data_excl = data[cases_excl, ]

    #non-excluded cases
    data = data[!cases_excl, ]
  }

  #impute
  data = VIM::irmi(data, noise = noise)

  #add back
  if (exclusion) {
    #rbind
    data = rbind(data, data_excl)

    #sort by ids
    data$.tmpid = as.numeric(c(case_ids[!cases_excl], case_ids[cases_excl]))
    data = df_sort(data, vars = ".tmpid", decreasing = F)

    #remove ids
    data$.tmpid = NULL
  }

  #return
  data
}


#' Filter data by missing values per row.
#'
#' Counts the number of missing values per row and then keeps rows that have at most a chosen number of missing values.
#' @param data (data frame) The data.
#' @param missing (num) The maximum number of missing values in cases. Defaults to 0 (keep only cases with no missing values).
#' @param reverse (lgl) Filter based on non-NA data instead.
#' @export
#' @examples
#' df = data.frame(1:10, letters[1:10])
#' df = miss_add_random(df)
#' miss_filter(df)
miss_filter = function(data, missing = 0, reverse = F) {
  #initial
  assertthat::assert_that(is_whole_number(missing, scalar = T))
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is_logical(reverse, scalar = T))

  #filter
  if (!reverse) return(data[miss_by_case(data) <= missing, ])
  data[miss_by_case(data, reverse = T) >= missing, ]
}
