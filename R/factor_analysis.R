#' Find residuals on case-level basis for all indicators in a factor analysis.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals.
#' @param data (data.frame) The data.
#' @param standardize (log scalar) Whether to standardize the residuals (default true). If not done, they may not have the same standard deviation.
#' @param ... (arguments to fa) Further arguments to \code{\link{fa}}.
#' @export
#' @examples
#' fa_residuals(iris[-5])
fa_residuals = function(data, standardize = T, ...) {

  #initial
  data = df_standardize(data) #standardize
  fa = psych::fa(data, ...) #factor analyze
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
#' @export
#' @examples
#' fa_MAR(iris[-5])
fa_MAR = function(data, sort = T, ...) {
  resids = fa_residuals(data, ...)

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
#' @export
#' @examples
#' fa_CFS(iris[-5])
fa_CFS = function(data, sort = T, include_full_sample = T) {
  #initial
  prop.vars = as.data.frame(matrix(nrow=nrow(data)+1, ncol=2)) #for results
  colnames(prop.vars) = c("Prop.var%", "IPV")

  #all cases
  fa = psych::fa(data) #factor analyze
  prop.var = mean(fa$communality) #the proportion of variance accounted for
  prop.vars[nrow(prop.vars),] = c(prop.var, 0) #insert
  rownames(prop.vars)[[nrow(prop.vars)]] = "All cases"

  #for each case
  for (case in 1:nrow(data)) {
    data2 = data[-case,] #get subset without that case
    tryCatch({
      fa = psych::fa(data2) #factor analyze
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
#' fa_all_methods(iris[-5])
fa_all_methods = function(DF, ..., skip_methods = "", messages = T) {

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
    silence({message(stringr::str_c(row, " out of ", nrow(perms), " - ", name))}, messages = messages)

    #skip methods
    if (!skip_methods == "") {
      if (any(str_detect(name, skip_methods))) {
        silence({message(str_c("Skipping method: ", name))}, messages = messages)
        next
      }
    }

    #analyze
    err = tryCatch({
      silence({.fa = psych::fa(DF, fm = extract_meth_name, scores = score_meth_name)}, messages = messages)
    },
    error = function(e) {
      scores[, name] = NA
      loadings[, name] = NA
      return("error")
    }
    )
    #skip on error
    if (is.character(err)) {
      silence({message(str_c("Skipping ", name, " due to extraction error"))}, messages = messages)

      next
    }

    #skip on Heywood case
    if (any(as.vector(.fa$loadings) > 1 | as.vector(.fa$loadings) < -1)) {
      silence({message(stringr::str_c("Heywood case found for ", name))}, messages = messages)
      next
    }

    #save
    silence({message(stringr::str_c("Saving results from ", name))}, messages = messages)
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
#' @param ... Parameters to \code{\link{fa_MAR}}. These get passed on to \code{\link{fa}}.
#' @export
#' @examples
#' fa_mixedness(iris[-5])
fa_mixedness = function(df, ...){

  #check if colnames contain illegal characters
  if (any(stringr::str_detect(colnames(df), " "))) stop("Colnames contain spaces. Remove and try again.")
  if (any(stringr::str_detect(colnames(df), "&"))) stop("Colnames contain ambersands. Remove and try again.")

  #for results
  return_df = data.frame(matrix(nrow=nrow(df), ncol=0))

  #mean abs. resids
  return_df$MAR = fa_MAR(df, sort = F, ...) %>% unlist %>% as.vector

  #all cases
  fa = psych::fa(df, ...) #factor analyze
  prop_var_full = mean(fa$communality)
  loads_full = as.numeric(fa$loadings)

  #get subsets
  each_subset = get_each_subset_minus_1(df)

  #perform fa on each
  each_fa = lapply(each_subset, fa, ...)

  #get propVars from each fa
  each_propVar = plyr::ldply(each_fa, function(x) {
    return(as.numeric(mean(x$communality)))
  })

  #change in factor size
  return_df$CFS = (each_propVar - prop_var_full) %>% unlist %>% as.vector

  #absolute change in factor size
  return_df$ACFS = abs(return_df$CFS)

  #get the loadings from each fa
  each_loadingset = plyr::ldply(each_fa, function(x) {
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
#' @export
#' @examples
#' fa_rank_fa()
fa_rank_fa = function(x, ...) {

  #rank matrix
  rank_data = apply(x, 2, rank)
  rank_data = as.data.frame(rank_data)
  rownames(rank_data) = rownames(x)
  colnames(rank_data) = colnames(x)

  #fa
  rank_fa = psych::fa(rank_data, ...)

  return(rank_fa)
}

#' Robust correlation matrix
#'
#'
#' Returns a correlation matrix with robust correlations. These are derived from either rlm() [MASS] or lmrob() [robustbase].
#' @param x A data.frame to correlate.
#' @param x method Which robust method to use. Options are "lmrob" [robustbase] or "rlm" [MASS]. Defaults to the first.
#' @export
#' @examples
#' fa_robust_cormatrix()
fa_robust_cormatrix = function(x, method = "lmrob") {
  #std dataset
  x.std = as.data.frame(scale(x))

  #combinations
  combos = gtools::combinations(ncol(x), 2)

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
      r_fit = robustbase::lmrob(as.formula(model), x.std)
      r_fit2 = robustbase::lmrob(as.formula(model2), x.std)
    }

    if (method == "rlm") {
      r_fit = MASS::rlm(as.formula(model), x.std)
      r_fit2 = MASS::rlm(as.formula(model2), x.std)
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
#' Performs a robust factor analysis. It is done using the output from fa_robust_cormatrix(). Note that this uses a correlation matrix, so factor scores are not available.
#' @param x A data.frame to factor analyze.
#' @export
#' @examples
#' fa_robust_cormatrix()
fa_robust_fa = function(x, ..., .method = "lmrob") {
  #get robust matrix
  r_mat = fa_robust_cormatrix(x, method = .method)

  #fa
  r_fa = psych::fa(r_mat, ...)

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
#' @export
#' @examples
#' fa_robust_cormatrix()
fa_CAFL = function(x, ..., sort = 1, include_full_sample = T) {

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
  AC = plyr::adply(loadings, 1, function(k) {
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
#' @param seed (num scalar) Seed to use for reproducible results. Default=1.
#' @param ... Extra parameters to pass to psych::fa().
#' @export
#' @examples
#' fa_splitsample_repeat(iris[-5])
fa_splitsample_repeat = function(data, runs = 100, save_scores = F, messages = T, progress = T, seed = 1, ...) {

  #rename input
  df = data; rm(data)

  #seed
  set.seed(seed)

  #input test
  if (!inherits(df, c("data.frame", "matrix"))) stop("data was not a data.frame or matrix", call. = F)
  df = as.data.frame(df)

  #missing values?
  if (any(is.na(df))) if (messages) message("Cases with missing values were removed.")
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
#' @export
#' @examples
#' fa_congruence_matrix()
fa_congruence_matrix = function(x) {

  #right input type?
  if (!class(x) %in% c("list", "data.frame", "matrix")) stop("Input was not a list, data.frame or matrix!")

  #if input is a list of fa's
  if (class(x) == "list") {
    #are they all factor analysis objects?
    if (all(unlist(lapply(x, function(x) inherits(x, "fa"))))) {
      #get loadings
      all_loadings = lapply(x, function(x) as.matrix(x$loadings))

      return(psych::factor.congruence(all_loadings)) #return congruence matrix

    } else {
      stop("Input was a list of something other than factor analysis objects!")
    }
  }

  #not a list
  plyr::alply(x, 2, as.matrix) %>% factor.congruence %>% return
  #split by column, into a list of matrices, calls the congruence function and returns
}


#' Get loadings from a factor analysis object.
#'
#' Extract loadings from a factor analysis object of class "fa") into a data.frame.
#' @param fa (object of class "fa") A factor analysis object.
#' @param threshold (num) A threshold for loadings to include.
#' @param long_form (lgl) Whether to return data frame in long format. Default is no. Useful for plotting.
#' @export
#' @examples
#' fa_loadings(psych::fa(iris[-5]))
#' fa_loadings(psych::fa(iris[-5], 2))
#' fa_loadings(psych::fa(iris[-5], 2), long_form = T)
#' fa_loadings(psych::fa(iris[-5], 2), .40)
#' fa_loadings(psych::fa(iris[-5], 2), .20, long_form = T)
fa_loadings = function(fa, threshold = NA, long_form = F) {
  loads = loadings(fa)
  class(loads) = "matrix"
  df = as.data.frame(loads)

  #salient loadings only?
  if (!is.na(threshold)) {
    #remove those below
    df[abs(df) < threshold] = NA
  }

  #long form if desired
  if (long_form) {
    df = cbind(indicator = rownames(df), df)
    df = tidyr::gather_(df, key_col = "factor", value_col = "loading", gather_cols = names(df)[-1]) %>%
      na.omit
  }

  df
}


#' Determine number of factors
#'
#' Convenience function for determining the number of factors in a fa object. Vectorized.
#' @param .fa (fa or list of fa objects) The object from which to determine the number of factors.
#' @export
#' @examples
#' fa(iris[-5]) %>% fa_nfactors
#' fa(iris[-5], nfactors = 2) %>% fa_nfactors
fa_nfactors = function(.fa) {
  #single
  if (inherits(.fa, what = "fa")) return (.fa$loadings %>% ncol)
  #multiple
  if (!all(sapply(.fa, inherits, what = "fa"))) stop("Input was not a single fa object or list or fa objects.")
  sapply(.fa, function(x) x$loadings %>% ncol)
}




#' Plot factor loadings
#'
#' Returns a ggplot2 plot with sorted loadings colored by the analysis they belong to. Supports reversing óf any factors that are reversed. Dodges to avoid overplotting.
#'
#' Non-overlapping indicates are put in the bottom. Note that internally, x and y coods have been flipped, so to modify the scales, use the opposite command, e.g. ylim to modify x axis limits.
#' @param fa_objects (list of fa-class objects) Factor analyses objects from the fa() function from the \code{\link{psych}} package.
#' @param fa_labels (chr vector) Names of the analyses.
#' @param factor_names (chr vectr) Names of the factors if multiple.
#' @param reverse_vector (num vector) Vector of numbers to use for reversing factors. Use e.g. c(1, -1) to reverse the second factor. Defaults not reversing.
#' @param reorder (chr scalar or NA) Which factor analysis to order the loadings by. Can be integers that reprensent each factor analysis. Can also be "mean", "median" to use the means and medians of the loadings. Use "all" for the old method. Default = "mean".
#' @export
#' @examples
#' library(psych)
#' fa_plot_loadings(fa(iris[-5])) #extract 1 factor and plot
#' fa_plot_loadings(fa(iris[-5], 2)) #extract 2 factors and plot
#' #list of FAs
#' fa_list = list(part1 = fa(iris[1:50, -c(1, 5)]),
#'                 part2 = fa(iris[51:100, -c(2, 5)]),
#'                 part3 = fa(iris[101:150, -c(3, 5)]))
#' #notice that it handles non-overlapping indicators
#' fa_plot_loadings(fa_list)
#' #reorder by a particular FA
#' fa_plot_loadings(fa_list, reorder = 1)
fa_plot_loadings = function (fa_objects, fa_labels = NA, factor_names = NA, reverse_vector = NA, reorder = "mean", clean_factor_labels = T, clean_indicator_labels = T) {

  #str cleaning func
  cleaner_func = function(x) {
    stringr::str_replace_all(x, pattern = "[._]", replacement = " ")
  }

  #which kind of plot?
  #1 unifactor analysis - 11
  #1 multifactor analysis - 1m
  #multiple unifactor analyses - m1
  #multiple multifactor analyses - mm
  if (inherits(fa_objects, "fa")) fa_objects = list(fa_objects) #put it in a list for consistency
  if (!all(sapply(fa_objects, inherits, what = "fa"))) stop("Could not recognize input. Input must be a single fa object, or a list of fa objects.")
  fa_num = length(fa_objects)
  factor_num = sapply(fa_objects, fa_nfactors)

  if (fa_num == 1 && all(1 == factor_num)) {
    plot_type = "11"
  } else if (fa_num == 1) {
    plot_type = "1m"
  } else if (all(factor_num == 1)) {
    plot_type = "m1"
  } else {
    plot_type = "mm"
  }

  #labels to use?
  fa_names = stringr::str_c("fa.", 1:fa_num)
  if (is_scalar_NA(fa_labels)) {
    #names on list?
    if (!is.null(names(fa_objects))) fa_labels = names(fa_objects) else fa_labels = 1:fa_num
  } else {
    if (length(fa_labels) != fa_num) stop("Factor analysis labels length is not identical to number of analyses.")
    fa_labels = fa_names
  }

  #one or more unifactor analyses
  if (plot_type %in% c("11", "m1")) {

    #check reverse_vector
    if (anyNA(reverse_vector)) {
      reverse_vector = rep(1, fa_num)
    } else if (length(reverse_vector) != fa_num) {
      stop("Length of reversing vector does not match number of factor analyses.")
    }

    #extract data
    d = data.frame()
    for (fa_idx in 1:fa_num) {
      #extract and reverse loadings if necessary
      loads = fa_objects[[fa_idx]]$loadings * reverse_vector[fa_idx]
      #indicators names
      rnames = rownames(loads)
      #as df
      loads = as.data.frame(as.vector(loads))
      #set dimnames
      rownames(loads) = rnames
      colnames(loads) = fa_names[fa_idx]
      #merge data
      silence({
        d = merge_datasets(d, loads)
      })
    }

    #reshape data to long form
    d2 = reshape(d, varying = 1:fa_num, direction = "long", ids = rownames(d))
    d2$time = as.factor(d2$time)
    d2$id = as.factor(d2$id)
    colnames(d2)[2] = "fa"

    #clean indicators?
    if (clean_indicator_labels) d2$id %<>% cleaner_func

    #reorder factor?
    if (reorder == "mean") {
      v_aggregate_values = plyr::daply(d2, .variables = "id", function(x) {
        mean(x$fa, na.rm=T)
      })

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_aggregate_values, decreasing = F)))

    } else if (reorder == "median") {
      v_aggregate_values = plyr::daply(d2, .(id), function(x) {
        median(x$fa, na.rm=T)
      })

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_aggregate_values, decreasing = F)))

    } else {
      d2_sub = d2[d2$time == reorder, ] #subset the analysis whose loading is to be used for the reorder

      #get vector of the chosen analysis
      v_values = d2_sub$fa; names(v_values) = d2_sub$id

      #re-level
      d2$id = factor(d2$id, levels = names(sort(v_values, decreasing = F, na.last = F)))
    }

    #plot
    if (fa_num > 1) {
      g = ggplot2::ggplot(d2, aes(x = id, y = fa, color = time, group = time)) +
        geom_point(position = position_dodge(width = 0.5)) +
        ylab("Loading") +
        xlab("Indicator") +
        scale_color_discrete(name = "Analysis", labels = fa_labels) +
        coord_flip()
    } else {
      g = ggplot2::ggplot(d2, aes(x = id, y = fa)) +
        geom_point(position = position_dodge(width = 0.5)) +
        ylab("Loading") +
        xlab("Indicator") +
        coord_flip()
    }
  }

  #1 fa with multiple factors
  if (plot_type == "1m") {

    #check reverse_vector
    if (anyNA(reverse_vector)) {
      reverse_vector = rep(1, fa_num)
    } else if (length(reverse_vector) != fa_num) {
      stop("Length of reversing vector does not match number of factor analyses.")
    }

    #extract data
    d = fa_objects[[1]]$loadings %>% unclass %>% as.data.frame
    #rename
    colnames(d) = 1:ncol(d)
    #indicator col
    d = cbind(indicator = rownames(d), d)
    #clean indicator labels?
    d$indicator %<>% stringr::str_replace_all("[._]", " ")
    #spread
    d = tidyr::gather_(d, gather_cols = colnames(d)[-1], key_col = "factor", value_col = "loading")

    #reorder indicators
    if (!is_scalar_NA(reorder)) {
      #sort by mean abs loading
      #this orders the indicators by their importance, overall
      d$indicator %<>% forcats::fct_reorder(d$loading, fun = function(x) {
        mean(abs(x))
      })
    }

    #factor names
    if (is_scalar_NA(factor_names)) factor_names = 1:factor_num

    #clean factor labels?
    if (clean_factor_labels) clean_factor_labels %<>% cleaner_func

    #reorder factor?
    #not clear how to do this

    #plot
    g = ggplot2::ggplot(d, aes(x = indicator, y = loading, color = factor, group = factor)) +
      geom_point(position = position_dodge(width = 0.1)) +
      ylab("Loading") +
      xlab("Indicator") +
      scale_color_discrete(labels = factor_names, name = "Factor") +
      coord_flip()
  }

  if (plot_type == "mm") {
    stop("Multiple multifactor plotting is not implemented yet.")
    }


  return(g + theme_bw())
}


#vectorized
suited_for_pearson = function(x, unique_min = 5) {
  #inner func
  inner_func = function(x) {
    if (inherits(x, "factor")) return(F)
    if (inherits(x, "logical")) return(F)
    if (length(unique(x)) < unique_min) return(F)
    T
  }

  #is input atomic?
  if (purrr::is_atomic(x)) return(inner_func(x))

  #if not, attempt to apply to each part
  purrr::map_lgl(x, inner_func)
}

#all together
suited_for_pearson_all = function(x, unique_min = 5) {
  all(suited_for_pearson(x, unique_min = unique_min))
}


#' Scatter plot of Jensen's method
#'
#' Takes a factor analysis, data frame and name of the criterion variable as inputs and returns a ggplot2 scatter plot with Jensen's method applied.
#'
#' #indicator_criterion_method
#' By default the function tries to automatically detect which kind of indicators are supplied. If indicators have any factors, logicals or numeric variables with less than 5 unique values, it it will call the hetcor function from polycor. If all are numeric with many levels, it will call the Pearson correlation (wtd.cors) from package weights with the supplied weights. Alternatively, one can input a numeric vector with the desired indicator-criterion relationships. These could be betas from complicated models.
#' @param fa (fa obj) A factor analysis object from fa().
#' @param df (data frame) A data frame that contains indicators and criterion.
#' @param criterion (chr) The name of the criterion variable.
#' @param reverse_factor (lgl) Whether to reverse the factor. Used when factor analysis results in a reversed factor.
#' @param loading_reversing (lgl) Whether to use loading reversing to avoid inflated results. Defaults to TRUE.
#' @param n_factor (int) Which factor to use? Default is 1st factor. Only relevant for multifactor analysis.
#' @param indicator_criterion_method (chr / num) Which method to use to compute indicator-criterion relationships?
#' @export
#' @examples
#' #load a subset of okcupid data
#' data(okcupid_social_prudence)
#' library(psych); library(polycor)
#' #estimate latent correlations
#' cors = polycor::hetcor(okcupid_social_prudence[-1]) %>% magrittr::extract2("correlations")
#' #factor analyze normally
#' fa = fa(cors)
#' #apply Jensen's method
#' fa_Jensens_method(fa, okcupid_social_prudence, criterion = "CA")
#' #output shows that indicators with larger loadings tend to be more positively related to cognitive ability
fa_Jensens_method = function(fa, df, criterion, reverse_factor = F, loading_reversing = T, check_overlap = TRUE, n_factor = 1, indicator_criterion_method = "auto", .weights = NA, ...) {
  args = list(...)

  if ("criteria" %in% args) stop("'criteria' changed name to 'criterion'")

  #get loadings
  if (ncol(fa$loadings) < n_factor) stop(sprintf("You tried to extract a factor that doesn't exist. n_factor was %d but fa only has %d factors.", n_factor, ncol(fa$loadings)))
  fa_loadings = fa$loadings %>% unclass %>% magrittr::extract(, n_factor)

  #weights
  if (is.na(.weights)) .weights = rep(1, nrow(df))

  #reverse factor is desired
  if (reverse_factor) fa_loadings = fa_loadings * -1

  #indicator_criterion_method
  if (is.numeric(indicator_criterion_method)) {
    indicator_criterion_vals = indicator_criterion_method
    indicator_criterion_method = "manual"
  }

  #get indicator names
  indicator_names = rownames(fa$loadings)
  indicator_num = length(indicator_names)

  #make new df
  df2 = df[c(indicator_names, criterion)]

  #get criterion x indicator relationships
  if (indicator_criterion_method == "auto") {
    if (!suited_for_pearson_all(df)) {
      message("Using latent correlations for the criterion-indicator relationships.")
      df2_cors = polycor::hetcor(df2, use = "pairwise.complete.obs") %>% magrittr::extract2("correlations")
    } else {
      message("Using Pearson correlations for the criterion-indicator relationships.")
      #convert all to numeric
      df2 = df_colFunc(df2, func = as.numeric)
      df2_cors = weights::wtd.cors(df2, weight = .weights)
    }
  } else if (indicator_criterion_method == "pearson") {
    df2 = df_colFunc(df2, func = as.numeric)
    df2_cors = weights::wtd.cors(df2, weight = .weights)
  } else if (indicator_criterion_method == "latent") {
    df2_cors = polycor::hetcor(df2) %>% magrittr::extract2("correlations")
  } else if (indicator_criterion_method == "manual") {
    #all good
  } else stop(sprintf("Could not recognize indicator_criterion_method: %s", indicator_criterion_method))


  #criterion x indicator vector
  # browser()
  if (!exists("indicator_criterion_vals")) indicator_criterion_vals = df2_cors[1:indicator_num, (indicator_num+1)]

  #make df for plotting
  df3 = data.frame(loading = fa_loadings,
                   crit_vals = indicator_criterion_vals)

  #reverse?
  if (loading_reversing) {
    for (i in 1:nrow(df3)) {
      if (df3[i, "loading"] < 0) {
        #reverse
        df3[i, ] = df3[i, ] * -1

        #name change
        indicator_names[i] = indicator_names[i] + "_r"
      }
    }
  }

  #plot
  g = GG_scatter(df3, x_var = "loading", y_var = "crit_vals", case_names_vector = indicator_names, ...) +
    ylab("Criterion-indicator relationship")+
    xlab("Loading")

  #return ggplot object
  return(g)
}
