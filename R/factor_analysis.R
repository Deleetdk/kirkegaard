#' Find residuals on case-level basis for all indicators in a factor analysis.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals.
#' @param data (data.frame) The data.
#' @param standardize (log scalar) Whether to standardize the residuals (default true). If not done, they may not have the same standard deviation.
#' @param ... (arguments to fa) Further arguments to \code{\link{fa}}.
#' @export
#' @examples
#' FA_residuals(iris[-5])
FA_residuals = function(data, standardize = T, ...) {
  library(stringr)
  library(psych)

  #initial
  data = std_df(data) #standardize
  fa = fa(data, ...) #factor analyze
  factor.scores = as.numeric(fa$scores) #get scores
  data2 = data; data2$factor.scores = factor.scores #insert scores into copy
  resids.table = data.frame(matrix(nrow=nrow(data), ncol=ncol(data))) #make df for resids
  colnames(resids.table) = colnames(data) #set names
  rownames(resids.table) = rownames(data) #

  #for each indicator
  for (indicator in colnames(data)) {
    formula = str_c(indicator, " ~ factor.scores") #the regression formula as string
    model = lm(formula, data2, na.action = "na.exclude") #regress
    resids = residuals(model) #extract residuals for this indicator
    if (standardize) {
      resids.table[, indicator] = standardize(resids)
    } else {
      resids.table[, indicator] = resids
    }
  }

  return(resids.table) #return resids
}

#' Calculates the mean absolute residual at the case-level.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals. Then calculates the mean absolute value of these by case.
#' @param data a data.frame.
#' @param sort whether to sort the results or not. Defaults to true.
#' @keywords psychometrics, psychology, latent variable, factor analysis, residuals
#' @export
#' @examples
#' FA_MAR(iris[-5])
FA_MAR = function(data, sort = T, ...) {
  resids = FA_residuals(data, ...)

  #mean absolute residuals
  mean.abs.resid = apply(resids, 1, function(x) {
    return(mean(abs(x)))
  }
  )

  #sort?
  if (sort) {
    mean.abs.resid = sort(mean.abs.resid, decreasing=T)
  }

  #return
  return(data.frame(MAR = mean.abs.resid))
}


## For finding problematic cases in FA
# Runs FA on a dataset without a case, for each case. Returns var% for subset analyses and difference to all cases.
#'
#' Returns a numerical vector of the change in the size of the first factor by removing that case.
#' @param data A data.frame.
#' @param sort Whether to sort the results or not. Defaults to true.
#' @param include_full_sample Whether to include the 'case' with the full sample. Defaults to true.
#' @keywords psychometrics, psychology, latent variable, factor analysis
#' @export
#' @examples
#' FA_CFS(iris[-5])
FA_CFS = function(data, sort = T, include_full_sample = T) {
  #initial
  prop.vars = as.data.frame(matrix(nrow=nrow(data)+1, ncol=2)) #for results
  colnames(prop.vars) = c("Prop.var%", "IPV")

  #all cases
  fa = fa(data) #factor analyze
  prop.var = mean(fa$communality) #the proportion of variance accounted for
  prop.vars[nrow(prop.vars),] = c(prop.var, 0) #insert
  rownames(prop.vars)[[nrow(prop.vars)]] = "All cases"

  #for each case
  for (case in 1:nrow(data)) {
    data2 = data[-case,] #get subset without that case
    tryCatch({
      fa = fa(data2) #factor analyze
      prop.var = round(mean(fa$communality),3) #the proportion of variance accounted for
      prop.var.delta = prop.var - prop.vars["All cases",1] #difference to row "all cases"
      prop.vars[case,] = c(prop.var, prop.var.delta) #insert
    },
    error = function(e) {
      prop.vars[case,] = c(NA, NA) #insert NAs
    }
    )

    rownames(prop.vars)[case] = rownames(data)[case] #set rowname
  }

  #sort?
  if (sort) {
    prop.vars = prop.vars[order(prop.vars[,2], decreasing=T),]
  }

  #full sample?
  if(!include_full_sample) {
    prop.vars = prop.vars[1:nrow(data), ]
  }

  return(prop.vars)
}

#' Factor analyze with all methods
#'
#'
#' Runs factor analysis on a dataset with all 30 possible combinations of extraction and scoring methods. Returns a list with all scores and loadings for further use.
#' @param DF A data.frame to extract factors from.
#' @param ... Parameters to fa().
#' @param skip_methods A character vector of methods to skip. Defaults to none.
#' @export
#' @examples
#' FA_all_methods(iris[-5])
FA_all_methods = function(DF, ..., skip_methods = "", messages = T) {
  #libs
  library(stringr)
  library(psych)

  #settings
  score.methods = c("regression", "Thurstone", "tenBerge", "Anderson", "Bartlett")
  extraction.methods = c("minres", "wls", "gls", "pa", "ml", "minchi")
  #all combitations of above: choose 1 of 5, choose 1 of 6
  perms = as.matrix(expand.grid(1:length(score.methods), 1:length(extraction.methods)))

  scores = data.frame(matrix(nrow = nrow(DF), ncol = 0))
  loadings = data.frame(matrix(nrow = ncol(DF), ncol = 0))
  #main loop
  for (row in 1:nrow(perms)) {
    #combination name
    score_meth_num = perms[row, 1]
    score_meth_name = score.methods[score_meth_num]
    extract_meth_num = perms[row, 2]
    extract_meth_name = extraction.methods[extract_meth_num]
    name = str_c(score_meth_name, "_", extract_meth_name)
    suppressor({message(str_c(row, " out of ", nrow(perms), " - ", name))}, messages = messages)

    #skip methods
    if (!skip_methods == "") {
      if (any(str_detect(name, skip_methods))) {
        suppressor({message(str_c("Skipping method: ", name))}, messages = messages)
        next
      }
    }

    #analyze
    err = tryCatch({
      suppressor({.fa = fa(DF, fm = extract_meth_name, scores = score_meth_name)}, messages = messages)
    },
    error = function(e) {
      scores[, name] = NA
      loadings[, name] = NA
      return("error")
    }
    )
    #skip on error
    if (is.character(err)) {
      suppressor({message(str_c("Skipping ", name, " due to extraction error"))}, messages = messages)

      next
    }

    #skip on Heywood case
    if (any(as.vector(.fa$loadings) > 1 | as.vector(.fa$loadings) < -1)) {
      suppressor({message(str_c("Heywood case found for ", name))}, messages = messages)
      next
    }

    #save
    suppressor({message(str_c("Saving results from ", name))}, messages = messages)
    scores[, name] = as.vector(.fa$scores)
    loadings[, extract_meth_name] = as.vector(.fa$loadings)
  }

  return(list(scores = scores,
              loadings = loadings))
}


#' Calculate mixedness metrics
#'
#' Returns 4 metrics that attempt to identify cases that are structural outliers/mixed in theor structure.
#'
#' @details
#' MAR, mean absolute residuals. Measures the how well indicator scores can be prediced from the factor scores.
#'
#' CFS, change in factor size. Measures how much the factor size changes with direction.
#'
#' ACFS, absolute change in factor size. Measures how much the factor size changes without direction.
#'
#' MeanALC, mean absolute loading change. Measures how much loadings are affected in general.
#'
#' MaxALC, max absolute loading change. Measures the maximal loading change.
#' @param df A data.frame to calculate mixedness metrics for.
#' @param ... Parameters to \code{\link{FA_MAR}}. These get passed on to \code{\link{fa}}.
#' @export
#' @examples
#' FA_mixedness(iris[-5])
FA_mixedness = function(df, ...){
  library(psych)
  library(plyr)
  library(stringr)

  #check if colnames contain illegal characters
  if (any(str_detect(colnames(df), " "))) stop("Colnames contain spaces. Remove and try again.")
  if (any(str_detect(colnames(df), "&"))) stop("Colnames contain ambersands. Remove and try again.")

  #for results
  return_df = data.frame(matrix(nrow=nrow(df), ncol=0))

  #mean abs. resids
  return_df$MAR = FA_MAR(df, sort = F, ...) %>% unlist %>% as.vector

  #all cases
  fa = fa(df, ...) #factor analyze
  prop_var_full = mean(fa$communality)
  loads_full = as.numeric(fa$loadings)

  #get subsets
  each_subset = get_each_subset_minus_1(df)

  #perform fa on each
  each_fa = lapply(each_subset, fa, ...)

  #get propVars from each fa
  each_propVar = ldply(each_fa, function(x) {
    return(as.numeric(mean(x$communality)))
  })

  #change in factor size
  return_df$CFS = (each_propVar - prop_var_full) %>% unlist %>% as.vector

  #absolute change in factor size
  return_df$ACFS = abs(return_df$CFS)

  #get the loadings from each fa
  each_loadingset = ldply(each_fa, function(x) {
    return(as.numeric(x$loadings))
  })

  #check and fix for reversed factors
  reversed = apply(each_loadingset, 1, function(x) {cor(x, loads_full)}) < 0
  each_loadingset[which(reversed), ] = each_loadingset[which(reversed), ] * -1

  #change in loadings
  each_loadingset_change = each_loadingset %>% t %>% - loads_full %>% t

  #mean abs loading change
  return_df$MeanALC = each_loadingset_change %>% abs %>% apply(., 1, mean)

  #max abs loading change
  return_df$MaxALC = each_loadingset_change %>% abs %>% apply(., 1, max)

  #rownames
  rownames(return_df) = rownames(df)

  return(return_df)
}



#' Rank order factor analysis
#'
#'
#' Runs factor analysis on a rank-ordered dataset which prevents outliers from having strong effects.
#' @param x A data.frame to extract factors from.
#' @param ... Parameters to fa().
#' @keywords psychometrics, factor analysis, robust, rank
#' @export
#' @examples
#' FA_rank_fa()
FA_rank_fa = function(x, ...) {
  #lib
  require(psych)

  #rank matrix
  rank_data = apply(x, 2, rank)
  rank_data = as.data.frame(rank_data)
  rownames(rank_data) = rownames(x)
  colnames(rank_data) = colnames(x)

  #fa
  rank_fa = fa(rank_data, ...)

  return(rank_fa)
}

#' Robust correlation matrix
#'
#'
#' Returns a correlation matrix with robust correlations. These are derived from either rlm() [MASS] or lmrob() [robustbase].
#' @param x A data.frame to correlate.
#' @param x method Which robust method to use. Options are "lmrob" [robustbase] or "rlm" [MASS]. Defaults to the first.
#' @keywords psychometrics, robust, correlation, matrix
#' @export
#' @examples
#' FA_robust_cormatrix()
FA_robust_cormatrix = function(x, method = "lmrob") {
  #lib
  require(gtools)

  #std dataset
  x.std = as.data.frame(scale(x))

  #combinations
  combos = combinations(ncol(x), 2)

  #df for results
  r_mat = as.data.frame(matrix(nrow = ncol(x), ncol = ncol(x)))
  rownames(r_mat) = colnames(r_mat) =  colnames(x)

  #fit each model
  for (row_idx in 1:nrow(combos)) {
    pred = colnames(x)[combos[row_idx, 1]]
    outcome = colnames(x)[combos[row_idx, 2]]

    #create models
    model = str_c(outcome, " ~ ", pred)
    model2 = str_c(pred, " ~ ", outcome)

    #fit models
    if (method == "lmrob") {
      require(robustbase)
      r_fit = lmrob(as.formula(model), x.std)
      r_fit2 = lmrob(as.formula(model2), x.std)
    }

    if (method == "rlm") {
      require(MASS)
      r_fit = rlm(as.formula(model), x.std)
      r_fit2 = rlm(as.formula(model2), x.std)
    }

    #take mean
    r_mean_coef = (coef(r_fit)[2] + coef(r_fit2))[2] / 2

    #save result
    r_mat[outcome, pred] = r_mean_coef

  }

  #make complete matrix
  r_mat = combine_upperlower(t(r_mat), r_mat, .diag = 1)

  return(r_mat)
}


#' Robust factor analysis.
#'
#'
#' Performs a robust factor analysis. It is done using the output from FA_robust_cormatrix(). Note that this uses a correlation matrix, so factor scores are not available.
#' @param x A data.frame to factor analyze.
#' @keywords psychometrics, robust, factor analysis.
#' @export
#' @examples
#' FA_robust_cormatrix()
FA_robust_fa = function(x, ..., .method = "lmrob") {
  #get robust matrix
  r_mat = FA_robust_cormatrix(x, method = .method)

  #fa
  r_fa = fa(r_mat, ...)

  return(r_fa)
}


#' Mixedness detection with a change in absolute factor loadings approach
#'
#'
#' Examines how the exclusion of each particular case influences the factor loadings.
#' @param x A data.frame to factor analyze.
#' @param ... Settings for fa().
#' @param sort Which column to sort results after. Defaults to the first column (mean absolute change). Set to anything else than 1 or 2 to avoid sorting.
#' @param include_full_sample Whether to include the full sample as a case. Defaults to true.
#' @keywords psychometrics, robust, factor analysis.
#' @export
#' @examples
#' FA_robust_cormatrix()
FA_CAFL = function(x, ..., sort = 1, include_full_sample = T) {
  library(plyr)

  #initial fa
  full_fa = fa(x, ...)

  #loadings object
  loadings = data.frame(matrix(ncol = ncol(x), nrow = nrow(x) + 1))
  rownames(loadings) = c(rownames(x), "Full data")
  colnames(loadings) = colnames(x)

  #insert loadings from full dataset
  loadings[nrow(loadings), ] = loadings(full_fa)

  #try each subset
  for (row_idx in 1:nrow(x)) {
    #subset data
    data_subset = x[-row_idx, ]

    #fa
    subset_fa = fa(data_subset, ...)

    #get loadings
    subset_loadings = loadings(subset_fa)

    #insert
    loadings[row_idx, ] = subset_loadings
  }


  #absolute change in loadings by case
  AC = adply(loadings, 1, function(k) {
    return(k - loadings[nrow(loadings), ])
  })

  #absolute value
  AC = abs(AC)

  #sensible rownames
  rownames(AC) = rownames(loadings)

  #mean value by subset
  MeCAFL = apply(AC, 1, mean)

  #max change by subset
  MaCAFL = apply(AC, 1, max)

  #list
  r = data.frame(mean_change = MeCAFL,
                 max_change = MaCAFL)

  #sort?
  if (sort == 1) {
    r = r[order(r[1], decreasing = T), ]
  }

  if (sort == 2) {
    r = r[order(r[2], decreasing = T), ]
  }

  #full sample?
  if (!include_full_sample) {
    r = r[-nrow(r), ]
  }

  return(r)
}


#' Repeated splithalf reliability with factor analysis
#'
#' Divides a dataset into 2 at random, extracts a factor from each and correlates them. Then saves the correlation. Repeats this any desired number of times. Can also return the factor scores instead of correlations.
#' @param data (data.frame or matrix) The data.
#' @param runs (integer scalar) The number of runs to do.
#' @param save_scores (boolean scalar) Whether to save scores. Default=F.
#' @param messages (lgl scalar) Whether to display messages, default yes.
#' @param progress (lgl scalar) Whether to display progress bar, default yes.
#' @param ... Extra parameters to pass to psych::fa().
#' @export
#' @examples
#' FA_splitsample_repeat(iris[-5]))
FA_splitsample_repeat = function(data, runs = 100, save_scores = F, messages = T, progress = T, ...){
  library(psych)
  library(stringr)

  #rename input
  df = data; rm(data)

  #input test
  if (!inherits(df, c("data.frame", "matrix"))) stop("data was not a data.frame or matrix", call. = F)
  df = as.data.frame(df)

  #missing values?
  if (any(is.na(df))) if (messages) messages("Cases with missing values were removed.")
  df = na.omit(df)

  #results
  results_scores = list()

  #loop
  if (progress) pb <- txtProgressBar(min = 1, max = runs, initial = 1, style = 3)
  for (run in 1:runs){
    if (progress) setTxtProgressBar(pb, value = run)

    #reorder df
    df = sample(df) #reorder at random

    #how to split
    split_at = ceiling(ncol(df)/2)
    df1_i = 1:split_at
    df2_i = (split_at+1):ncol(df)

    #split
    df1 = df[df1_i]
    df2 = df[df2_i]

    #fa
    df1_fa = fa(df1, ...)
    df2_fa = fa(df2, ...)

    #scores
    scores = data.frame(fa1 = as.vector(df1_fa$scores),
                        fa2 = as.vector(df2_fa$scores))

    #what to save
    if (save_scores) results_scores[[run]] = scores
    else results_scores[[run]] = cor(scores)[1, 2]
  }
  if (progress) close(pb)

  #simplify to vector if just saving correlations
  if(!save_scores) results_scores = data.frame(r = unlist(results_scores))

  #return
  return(results_scores)
}


#' Calculate congruence coefficient matrix.
#'
#' Takes an input either a list of factor analysis objects from fa() (psych package) or a data.frame/matrix of loadings. Returns a matrix of congruence coefficients. These are calculated using factor.congruence() from psych package.
#' @param x A list of factor analysis objects from fa() or a data.frame/matrix of factor loadings.
#' @keywords factor, congruence, matrix
#' @export
#' @examples
#' FA_congruence_matrix()
FA_congruence_matrix = function(x) {
  library(psych)

  #right input type?
  if (!class(x) %in% c("list", "data.frame", "matrix")) stop("Input was not a list, data.frame or matrix!")

  #if input is a list of fa's
  if (class(x) == "list") {
    #are they all factor analysis objects?
    if (all(unlist(lapply(x, function(x) class(x) == c("psych", "fa"))))) {
      #get loadings
      all_loadings = lapply(x, function(x) as.matrix(x$loadings))

      return(factor.congruence(all_loadings)) #return congruence matrix

    } else {
      stop("Input was a list of something other than factor analysis objects!")
    }
  }

  #not a list
  library(plyr)
  alply(x, 2, as.matrix) %>% factor.congruence %>% return
  #split by column, into a list of matrices, calls the congruence function and returns
}


#' Get loadings from a factor analysis object.
#'
#' Extract loadings from a factor analysis object of class "fa") into a data.frame.
#' @param fa (object of class "fa") A factor analysis object.
#' @keywords factor analysis, loadings
#' @export
#' @examples
#' library("psych")
#' iris_fa = fa(iris[-5])
#' get_loadings(iris_fa)
get_loadings = function(fa) {
  loads = loadings(fa)
  class(loads) = "matrix"
  df = as.data.frame(loads)
  return(df)
}


#' Get salient loadings from a factor analysis object.
#'
#' Find indicators that load above a given threshold for each factor. Can also force them into a data.frame for easy output. Uses input object from psych package's fa().
#' @param fa (object of class "fa") A factor analysis object.
#' @param threshold (numeric scalar) The cutoff to use for salient loadings. Default to .40.
#' @keywords factor analysis, loadings, salient, threshold
#' @export
#' @examples
#' get_salient_loadings()
get_salient_loadings = function(fa, threshold = .40, to_df = T) {
  library(magrittr)

  #checks
  if (!"fa" %in% class(fa)) stop("fa must be of class fa!")
  if (!is_between(threshold, 0, 1)) stop("threshold must be between 0 and 1!")

  #get loadings to df
  d_loads = get_loadings(fa)

  #loop and keep only the salient loadings
  l_salient = lapply(d_loads, function(x) {
    names(x) = rownames(d_loads)
    x_abs = abs(x)
    v_salient = x_abs >= threshold
    return(x[v_salient])
  })

  #to data.frame?
  if (to_df) {
    d_salient = named_vectors_to_df(l_salient, value_suffix = "_loadings", name_suffix = "_indicators")
    return(d_salient)
  }

  return(l_salient)
}


