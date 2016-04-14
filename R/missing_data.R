## FUNCTIONS FOR DEALING WITH MISSING DATA

#' Missing datapoint counter, case-level
#'
#' Counts the number of missing datapoints per case
#' @param x (data.frame/matrix) The data.
#' @export miss_case miss_by_case
#' @aliases miss_case
#' @examples
#' miss_by_case(df_addNA(iris))
miss_by_case = function(x){
  y = apply(x, 1, is.na)
  y = apply(y, 2, sum)
  return(y)
}

miss_case = miss_by_case

#' Missing datapoint counter, variable-level
#'
#' Counts the number of missing datapoints per variable
#' @param x a matrix or data.frame
#' @export miss_by_var miss_table
#' @aliases miss_table
#' @examples
#' miss_by_var(df_addNA(iris))
miss_by_var = function(x){
  y = apply(x, 2, is.na)
  y = apply(y, 2, sum)
  return(y)
}

miss_table = miss_by_var

#' Count missing data
#'
#' A simple wrapper for is.na() and sum(). Returns an integer.
#' @param x (any suitable object) An object for which to count NAs.
#' @param reverse (logical scalar) Whether to count non-NAs instead (default false).
#' @export
#' @examples
#' m = matrix(c(NA, 1:3, NA, 4:6, NA), nrow=3)
#' count_NA(m)
count_NA = function(x, reverse = F) {
  if (reverse) return(sum(!is.na(x)))
  sum(is.na(x))
}

#' Missing data histogram with ggplot2.
#'
#' Returns a ggplot2 histogram plot.
#' @param df a data.frame.
#' @param percent whether to use percent or not. Defaults to true.
#' @export miss_plot plot_miss
#' @aliases plot_miss
#' @examples
#' miss_plot(df_addNA(iris))
miss_plot = function(df, percent=T, case=T) {
  library(ggplot2)
  if (case) {
    m = miss_by_case(df)
  } else {
    m = miss_by_var(df)
  }

  d = data.frame(number.of.NA = m)
  max.miss = max(m)
  min.miss = min(m)

  if (percent) {
    d$percent = (d$number.of.NA/sum(d$number.of.NA))*100
    g = ggplot(data = d, aes(x = factor(number.of.NA))) +
      geom_bar(aes(y = ((..count..)/sum(..count..))*100)) +
      scale_y_continuous('percent') +
      xlab("Number of NAs") +
      scale_x_discrete(breaks=min.miss:max.miss)
    return(g)
  }
  else {
    g = ggplot(data = d, aes(x = factor(number.of.NA))) +
      geom_histogram() +
      xlab("Number of NAs") +
      scale_x_discrete(breaks=min.miss:max.miss)
    return(g)
  }
}

#old name
plot_miss = miss_plot

#' Wrapper for matrixplot()
#'
#' Wrapper for matrixplot() from VIM, but automatically substrings the variable names to 8 characters.
#' @param df a data.frame.
#' @export
#' @examples
#' matrixplot2(df_addNA(iris))
matrixplot2 = function(df) {
	library(VIM) #load VIM if not already loaded
  	return(matrixplot(df, labels=substr(colnames(df), 1, 8)))
}


#' Missing data matrix
#'
#' Create the missing data matrix. Not so useful in itself, but useful for other functions.
#' @param data (data.frame/matrix) The data.
#' @return A matrix where missing/present values are coded as 1/0.
#' @export
#' @examples
#' miss_matrix(df_addNA(iris))
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
#' miss_amount(df_addNA(iris))
miss_amount = function(data, round = 2) {
  #calculate amount of missing
  by_case = miss_case(data) %>% percent_cutoff(cutoffs = 1)
  by_var = miss_table(data) %>% percent_cutoff(cutoffs = 1)
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
#' miss_pattern(df_addNA(iris))
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
#' miss_complexity(df_addNA(iris))
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
#' @param data (data.frame/matrix) The data.
#' @param robust (log scalar) Whether to use robust measures (default false). If true, will use median/mad instead of mean/sd to calculate the standardized mean differences.
#' @return A data.frame of size n x n where n is the number of variables in data. The rows are the gropuing variable (missing vs. not-missing) and the columns are the outcome variables.
#' @export
#' @examples
#' miss_complexity(df_addNA(iris))
miss_analyze = function(data, robust = F) {
  library(plyr); library(lsr); library(compute.es)

  #data
  data = as.data.frame(data)

  #miss matrix
  m_d = miss_matrix(data)

  #for each variable, analyze relationship each other variable
  d_NA_diffs = ldply(.data = colnames(data), .fun = function(var) {
    #for each variable
    sapply(colnames(data), FUN = function(var2) {
      #if same
      if (var == var2) return(NA)

      #if no missing data
      if (all(m_d[, var] == 0)) return(NA)

      #if factor, use cramer's V
      if (data[[var2]] %>% is.factor()) {

        return(silence(lsr::cramersV(m_d[, var], data[[var2]]) %>% res(r = ., n = 100, verbose = F)) %>% extract("d") %>% unlist() %>% as.vector()) #silence to avoid chi sq warnings
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

