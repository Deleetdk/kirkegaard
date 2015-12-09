## STATISTICS functions


#' Wrapper for rcorr()
#'
#' A wrapper for rcorr() that takes data.frames or matrices as opposed to only matrices.
#' @param x a matrix or data.frame
#' @keywords wrapper correlation matrix
#' @export
#' @examples
#' rcorr2()
rcorr2 = function(x, ...) {
  library(Hmisc) #for rcorr()
  x = as.matrix(x)
  x = rcorr(x, ...)
  return(x)
}

#' Jensen method (method of correlated vectors) plot
#'
#' Returns a ggplot2 scatter plot with numerical results in a corner. Also supports reversing for dealing with factors that have negative indicators.
#' @param loadings a vector of factor loadings.
#' @param loadings a vector of correlations of the indicators with the criteria variable.
#' @param reverse whether to reverse indicators with negative loadings. Default to true.
#' @param text_pos which corner to write the numerical results in. Options are "tl", "tr", "bl", "br". Defaults to "tl".
#' @keywords psychometrics psychology latent variable
#' @export
#' @examples
#' Jensen_plot()
Jensen_plot = function(loadings, cors, reverse = TRUE, text_pos, var_names = TRUE){
  #libs
  library(ggplot2)
  library(grid)

  #initial
  temp_loadings = as.numeric(loadings) #conver to vector
  names(temp_loadings) = rownames(loadings) #set names again
  loadings = temp_loadings #back to normal name
  DF = data.frame(loadings, cors) #DF

  #reverse
  if (reverse) {
    for (idx in 1:nrow(DF)) {
      if (DF[idx, 1] < 0){ #if loading <0
        DF[idx, ] = DF[idx, ] * -1 #reverse
        rownames(DF)[idx] = paste0(rownames(DF)[idx], "_r")
      }
    }
  }

  #method text
  if (reverse) {method_text = "Jensen's method with reversing\n"}
  else {method_text = "Jensen's method without reversing\n"}

  #correlation
  cor = round(cor(DF)[1, 2], 2) #get correlation, rounded

  #auto detect text position
  if (missing(text_pos)) {
    if (cor>0) text_pos = "tl" else text_pos = "tr"
  }

  #text object location
  if (text_pos == "tl") {
    x = .02
    y = .98
    hjust = 0
    vjust = 1
  }
  if (text_pos == "tr") {
    x = .98
    y = .98
    hjust = 1
    vjust = 1
  }
  if (text_pos == "bl") {
    x = .02
    y = .02
    hjust = 0
    vjust = -.1
  }
  if (text_pos == "br") {
    x = .98
    y = .02
    hjust = 1
    vjust = -.1
  }

  #text
  text = paste0(method_text,
                "r=", cor, " (orange line)",
                "\nn=", nrow(DF))

  #text object
  text_object = grobTree(textGrob(text, x = x,  y = y, hjust = hjust, vjust = vjust),
                         gp = gpar(fontsize = 11))

  #regression line
  model = lm(cors ~ loadings, DF)
  coefs = coef(model)

  #plot
  DF$rnames = rownames(DF)

  g = ggplot(data = DF, aes(x = loadings, y = cors)) +
    geom_point() +
    xlab("Loadings") +
    ylab("Correlation with criteria variable") +
    annotation_custom(text_object) +
    geom_abline(intercept = coefs[1], slope = coefs[2], color = "darkorange")

  #add var_names if desired
  if (var_names) g = g + geom_text(aes(label = rnames), alpha = .7, size = 3, vjust = 1)

  return(g)
}


# Correlates all variables, finds the pair with the highest correlation, and removes one of them using the specified method.
#' Remove the n most redundant variables from a data.frame.
#'
#' Removes the n top variables that highly correlated with another variable so as to avoid problems in analysis.
#' @param df a data.frame.
#' @param num.to.remove the number of variables to remove.
#' @param remove.method the method to use to remove variables. Methods are "c", "l", "r", "f" and "s": conversative, liberal, random, first or second.
#' @keywords psychometrics, psychology, latent variable, factor analysis, redundant
#' @export
#' @examples
#' remove_redundant_vars()
remove_redundant_vars = function(df, num.to.remove = 1, remove.method = "s") {
  if (!is.data.frame(df)) {
    stop(paste0("First parameter is not a data frame. Instead it is ", class(df)))
  }
  if (!is.numeric(num.to.remove)) {
    stop(paste0("Second parameter is not numeric. Instead is ", class(num.to.remove)))
  }
  remove.method.1 = substr(remove.method, 1,1) #get first char
  if (!remove.method %in% c("f", "s", "r")) { #conversative, liberal or random, first or second
    stop(paste0("Third parameter was neither identifable as first, second or random. It was: ", remove.method))
  }

  old.names = colnames(df) #save old variable names

  for (drop.num in 1:num.to.remove) {
    print(paste0("Dropping variable number ", drop.num))
    names = colnames(df) #current names

    #correlations
    cors = as.data.frame(cor(df, use="pair"))

    #remove diagnonal 1's
    for (idx in 1:nrow(cors)) {
      cors[idx, idx] = NA
    }

    #absolute values because we don't care if cor is .99 or -.99
    cors.abs = abs(cors)

    #dropping
    max.idx = which_max2(cors.abs) #indexes of max value (first one if multiple identical)

    topvars = paste(rownames(cors)[max.idx[2]], "and", rownames(cors)[max.idx[1]]) #names of top correlated variables
    r = round(cors[max.idx[1], max.idx[2]], 3)
    print(paste0("Most correlated vars are ", topvars, " r=", r)) #print info

    #first
    if (remove.method.1 == "f") {
      df[, max.idx[2]] = NULL #remove the second var
    }
    #second
    if (remove.method.1 == "s") {
      df[, max.idx[1]] = NULL #remove the second var
    }
    #random
    if (remove.method.1 == "r") {
      if (rnorm(1) > 0){
        df[, max.idx[1]] = NULL #remove the second var
      }
      else {
        df[, max.idx[2]] = NULL #remove the first var
      }

    }
  }
  #Which variables were dropped?
  new.names = colnames(df)
  dropped.names = setdiff(old.names, new.names)
  print("Dropped the following variables:")
  print(dropped.names)

  #return reduced df
  return(df)
}

# Remove redundant variables
#
#' Remove redundant variables from a data.frame based on a threshold value. This is done by calculating all the intercorrelations, then finding those that correlate at or above the threshold (absolute value), then removing the second pair of each variable and not removing more variables than strictly necessary.
#' @param df A data.frame with numeric variables.
#' @param threshold A threshold above which intercorrelations are removed. Defaults to .9.
#' @param cor_method The correlation method to use. Parameter is fed to cor(). Defaults to pearson.
#' @keywords psychometrics, latent variable, factor analysis, redundant, variable
#' @export
#' @examples
#' remove_redundant_vars()
remove_redundant_vars2 = function(df, threshold = .9, cor_method = "pearson") {
  #Check input
  if (!is.data.frame(df)) {
    stop(paste0("First parameter is not a data frame. Instead it is ", class(df)))
  }
  if (!is.numeric(threshold)) {
    stop(paste0("Second parameter is not numeric. Instead is ", class(num.to.remove)))
  }

  #Old variable names
  old_names = colnames(df) #save old variable names

  #remove data in diag and top
  m = cor(df, use = "p", method = cor_method)

  #to long form
  m_long = cbind(expand.grid(rownames(m), colnames(m), stringsAsFactors = F),
                 r = as.vector(m),
                 abs_r = as.vector(m) %>% abs,
                 keep = upper.tri(m) %>% as.vector)

  #remove self-correlations and duplicates
  m_long = m_long[m_long$keep, ]

  #sort by abs r
  m_long = m_long[order(m_long$abs_r, decreasing = T), ]

  #subset
  m_long = m_long[1:3]

  #over threshold message
  m_long_threshold = m_long[m_long$r >= threshold | m_long$r <= -threshold, ]
  if(nrow(m_long_threshold) != 0) {
    message("The following variable pairs had stornger intercorrelations than |", threshold, "|:")
    round_df(m_long_threshold) %>% print #round the print
  } else {
    message("No variables needed to be excluded.")
    return(df)
  }

  #exclude variables
  #one cannot just remove the ones in the Var2 col because this can result in variables being removed despite them not correlating >threshold with any variable
  vars_to_exclude = character() #for the varnames
  while(T) {
    #exit loop if done
    if (nrow(m_long_threshold) == 0) break

    #add top var in Var2 to the exclusion vector and remove top row
    vars_to_exclude = c(vars_to_exclude, m_long_threshold$Var2[1])
    m_long_threshold = m_long_threshold[-1, ]

    #remove rows that contain any variable from the exclusion vector
    exclude_rows = m_long_threshold$Var1 %in% vars_to_exclude | m_long_threshold$Var2 %in% vars_to_exclude
    m_long_threshold = m_long_threshold[!exclude_rows, ]
  }

  #exclude variables
  message(str_c("The following variables were excluded:"))
  message(str_c(vars_to_exclude, collapse = ", "))
  df = df[!colnames(df) %in% vars_to_exclude]

  #return reduced df
  return(df)
}




#' Find residuals on case-level basis for all indicators in a factor analysis.
#'
#' Extracts the first factor, then uses the factor scores to predict the indicator values for each indicator and for each case. Returns a data.frame with residuals.
#' @param data a data.frame.
#' @keywords psychometrics, psychology, latent variable, factor analysis, residuals
#' @export
#' @examples
#' FA_residuals()
FA_residuals = function(data, ...) {
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
    resids.table[, indicator] = resids #set into resids df
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
#' FA_MAR()
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
#' FA_CFS()
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
#' @keywords psychometrics, factor analysis
#' @export
#' @examples
#' FA_all_methods()
FA_all_methods = function(DF, ..., skip_methods = "") {
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
    print(str_c(row, " out of ", nrow(perms), " - ", name))

    #skip methods
    if (!skip_methods == "") {
      if (any(str_detect(name, skip_methods))) {
        print(str_c("Skipping method: ", name))
        next
      }
    }

    #analyze
    err = tryCatch({
      .fa = fa(DF, fm = extract_meth_name, scores = score_meth_name)
    },
    error = function(e) {
      scores[, name] = NA
      loadings[, name] = NA
      return("error")
    }
    )
    #skip on error
    if (is.character(err)) {
      print(str_c("Skipping ", name, " due to extraction error"))
      next
    }

    #skip on Heywood case
    if (any(as.vector(.fa$loadings) > 1 | as.vector(.fa$loadings) < -1)) {
      print(str_c("Heywood case found for ", name))
      next
    }

    #save
    print(str_c("Saving results from ", name))
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
#' @param ... Parameters to fa().
#' @keywords psychometrics, factor analysis, mixedness, outlier
#' @export
#' @examples
#' FA_mixedness()
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


#' Semi-partial correlation with weights
#'
#'
#' Returns the semi-partial correlation. Weights may be used.
#' @param x A numeric vector to correlate with y.
#' @param y A numeric vector to correlate with x after partialing out z.
#' @param z A numeric vector to partial out of y.
#' @param weights A numeric vector of weights to use. If none given, will return unweighted results.
#' @keywords psychometrics, partial, semi-partial, correlation, weights
#' @export
#' @examples
#' semi_par()
semi_par = function(x, y, z, weights = NA, complete_cases = T) {
  library(weights)

  #if no weights, set to vector of 1's
  if (length(weights) == 1) {
    if (is.na(weights)) {
      weights = rep(1, length(x))
    }
  }

  #data.frame
  df = data.frame(x = as.vector(x), #we vectorize the input because otherwise may get strange
                  y = as.vector(y), #results when input is a df or matrix
                  z = as.vector(z),
                  w = as.vector(weights))

  #complete cases only
  if (complete_cases) {
    df = df[complete.cases(df), ]
  }

  #model
  df$y_res = resid(lm(y ~ z, weights = w, data = df))
  r_sp = wtd.cor(df$x, df$y_res, weight = df$w)
  r = wtd.cor(df$x, df$y, weight = df$w)
  return(list(normal = r,
              semi_partial = r_sp))
}


#' Semi-partial correlation with weights
#'
#'
#' Returns a table of semi-partial correlations where a dependent variable has first been regressed on the primary predictor variable, and then correlated with each of the secondary predictors in turn.
#' @param df A data frame with the variables.
#' @param dependent A string with the name of the dependent variable.
#' @param primary A string with the name of the primary predictor variable.
#' @param secondaries A character vector with the names of the secondary predictor variables.
#' @param weights A string with the name of the variable to use for weights.
#' @param standardize Whether to standardize the data frame before running results. The weights variable will not be standardized.
#' @keywords psychometrics, partial, semi-partial, correlation, weights
#' @export
#' @examples
#' semi_par_serial()
semi_par_serial = function(df, dependent, primary, secondaries, weights=NA, standardize=T) {
  library(weights) #for weighted correlations
  library(stringr) #strings

  #subset and deal with lack of weights
  if (is.na(weights)) {
    weights = "weights_var"
    df[weights] = rep(1, nrow(df))
    df = subset(df, select = c(dependent, primary, secondaries, weights))
  } else {
    df["weights_var"] = df[weights] #move the weights var to another name
    weights = "weights_var"
    df = subset(df, select = c(dependent, primary, secondaries, weights))
  }

  #complete cases only
  df = na.omit(df)

  #standardize
  if (standardize) df = std_df(df, exclude = weights)

  #primary
  r_prim = round(wtd.cor(df[, dependent], df[, primary], weight = df[, weights]), 2)

  #make results object
  results = data.frame(matrix(nrow = length(secondaries), ncol = 2))
  rownames(results) = secondaries
  colnames(results) = c("Orig. cor", "Semi-partial cor")
  #loop over each secondary
  for (sec_idx in seq_along(secondaries)) {

    #the current secondary var
    tmp_secondary = secondaries[sec_idx]

    #make the model
    tmp_model = str_c(dependent, " ~ ", primary)

    #regress
    df$tmp_resids = resid(lm(as.formula(tmp_model), weights = df[, weights], data = df))

    #secondary original
    r_sec = wtd.cor(df[, dependent], df[, tmp_secondary], weight = df[, weights])[1]

    #semi-partial
    r_sec_sp = wtd.cor(df$tmp_resids, df[, tmp_secondary], weight = df[, weights])[1]

    #save
    results[sec_idx, ] = c(r_sec, r_sec_sp)
  }

  return(results)
}


#' Repeated splithalf reliability with factor analysis
#'
#' Divides a dataset into 2 at random, extracts a factor from each and correlates them. Then saves the correlation. Repeats this any desired number of times. Can also return the factor scores instead of correlations.
#' @param df A data.frame with variables.
#' @param var A string of the name of the variable to use.
#' @keywords ggplot2, plot, density, histogram
#' @export
#' @examples
#' FA_splitsample_repeat()
FA_splitsample_repeat = function(df, runs = 100, save_scores = F, ...){
  library(psych)
  library(stringr)

  #input test
  if (!class(df) %in% c("data.frame", "matrix")) stop("df was not a data.frame or matrix")
  df = as.data.frame(df)

  #missing values?
  if (any(is.na(df))) warning("cases with missing values were removed")
  df = na.omit(df)

  #results
  results_scores = list()

  #loop
  for (run in 1:runs){
    message(str_c("run ", run, " of ", runs))

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


#' Scatter plot of Jensens method.
#'
#' Takes a factor analysis, data.frame and name of the criteria variable as inputs and returns a ggplot2 scatter plot with Jensen's method applied.
#' @param fa A factor analysis object from fa().
#' @param df A data.frame that contains all the variables.
#' @param criteria A character string of the name of the criteria variable.
#' @param reverse_factor Whether to reverse the factor first.
#' @param loading_reversing Whether to use loading reversing to avoid inflated results. Defaults to TRUE.
#' @param text_pos Which corner to put the text in. Defaults to "tl". Other options: tr, bl, br.
#' @keywords factor analysis, Jensen, method of correlated vectors
#' @export
#' @examples
#' Jensens_method()
Jensens_method = function(fa, df, criteria, reverse_factor = F, loading_reversing = T, text_pos) {
  #get loadings
  fa_loadings = as.numeric(fa$loadings)

  #reverse factor is desired
  if (reverse_factor) fa_loadings = fa_loadings * -1

  #get indicator names
  indicator_names = rownames(fa$loadings)
  indicator_num = length(indicator_names)

  #make new df
  df2 = df[c(indicator_names, criteria)]

  #correlate
  df2_cors = cor(df2, use = "p")

  #criteria x indicator cor vector
  criteria_indi_cor = df2_cors[1:indicator_num, (indicator_num+1)]

  #call plotter
  g = Jensen_plot(fa_loadings, cors = criteria_indi_cor, reverse = loading_reversing, text_pos = text_pos)

  #return ggplot object
  return(g)
}


#' Calculate a correlation matrix with and without weights.
#'
#' Inputs a data.frame and a set of weights. The weights can be given either as the name of the variable to use for weights or as a numeric vector. Outputs a correlation matrix where the lower triangle are weighted correlations and the upper triangle are unweighted. Diagonals are set as NA. The weights variable is excluded from the matrix.
#' @param df A data.frame.
#' @param weight_var A character vector of the name of the weights variable.
#' @param weights A numeric vector of the weights to use.
#' @keywords correlation, matrix, weights
#' @export
#' @examples
#' cor_matrix_weights()
cor_matrix_weights = function(df, weight_var, weights) {
  library(weights)
  #for weights

  #if weights are in the data.frame
  if (!missing("weight_var")) {
    #extract weights
    weights = df[[weight_var]]

    #remove weights var
    df[weight_var] = NULL
  }

  #check lengths
  if (length(weights) != nrow(df)) stop("Lengths of weights vector and data.frame don't match!")

  #get cors
  r = wtd.cors(df)
  r_wt = wtd.cors(df, weight = weights)

  #combine
  r_combined = combine_upperlower(r, r_wt)

  #return
  return(r_combined)
}


#' Calculate a partial correlation.
#'
#' Calculates the partial correlation.
#' @param df A data.frame.
#' @param x String with the name of the first variable.
#' @param y String with the name of the second variable.
#' @param z String with the name of the control variable.
#' @param weights String with the name of the weights variable. Can be left out.
#' @keywords correlation, partial, weights
#' @export
#' @examples
#' MOD_partial()
MOD_partial = function(df, x, y, z, weights_var) {
  library(stringr)
  library(weights)

  #check input
  if (missing("df") | missing("x") | missing("y") | missing("z")) stop("df, x, y or z is missing!")

  #make or move weights
  if (missing("weights_var")) {
    df$weights___ = rep(1, nrow(df)) #make unit weights
  } else {
    df$weights___ = df[[weights_var]] #reassign weights var
  }
  weights_var = "weights___"

  #build models
  mod1 = str_c(x, " ~ ", str_c(z, collapse = " + "))
  mod2 = str_c(y, " ~ ", str_c(z, collapse = " + "))

  #fit models
  fit1 = lm(mod1, data = df, weights = weights___, na.action = na.exclude)
  fit2 = lm(mod2, data = df, weights = weights___, na.action = na.exclude)
  #na.exclude is important becus otherwise NA values are removed

  #get residuals
  resid1 = resid(fit1)
  resid2 = resid(fit2)

  #correlate
  r = wtd.cor(resid1, resid2, weight = df$weights___)

  return(r[1])
}



#' Find percentage of numbers that are above given thresholds.
#'
#' Takes a numeric vector and a numeric vector of thresholds. Returns the percent of numbers in the first above each of the numbers in the second.
#' @param x (numeric vector) A vector of numbers.
#' @param cutoffs (numeric vector) A vector of thresholds. Default=(.30, .50)
#' @param digits (numeric scalar) The number of digits to round output to. Default=2.
#' @keywords threshold, percent, proportion
#' @export
#' @examples
#' percent_cutoff()
percent_cutoff = function(x, cutoffs = c(.30, .50), digits = 2) {
  library(magrittr)

  #convert
  x = as.numeric(x)
  cutoffs = as.numeric(cutoffs)

  v_res = numeric()
  for (idx in seq_along(cutoffs)) {
    v_res[idx] = (x > cutoffs[idx]) %>%
      (function(x) {
        sum(x) / (na.omit(x) %>% length)
        })
  }
  names(v_res) = cutoffs

  #round
  if (!missing("digits")) v_res = round(v_res, digits = digits)

  return(v_res)
}



#' Score accuracy of estimates.
#'
#' Calculates accuracy measures from a data.frame of estimates using a vector of criteria values.
#' @param df (numeric data.frame) A data.frame with estimates. Rows must be cases.
#' @param criteria (numeric vector) A vector of criteria values to score estimates against.
#' @param methods (character vector) Which measures to return. Defaults to c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs"). Use "all" to get all.
#' @keywords score, estimates, accuracy
#' @export
#' @examples
#' score_accuracy()
score_accuracy = function(df, criteria, methods = c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs")) {

  #NAs
  if (any(is.na(df))) message("Note: some data were missing. This function uses pairwise complete cases.")

  #make df for results
  df = as.data.frame(df)
  criteria = unlist(criteria) %>% as.vector
  d_res = data.frame(matrix(nrow = nrow(df), ncol = 0))


  #Pearson r
  d_res$pearson_r = sapply(1:nrow(df), function(x) {
    cor(criteria, df[x, ] %>% unlist, use = "p")
  })


  #rank-order r
  d_res$rank_r = sapply(1:nrow(df), function(x) {
    cor(criteria, df[x, ] %>% unlist, method = "spearman", use = "p")
  })


  #delta (discrepancy) error
  d_deltas = (t(df) - criteria) %>% t %>% as.data.frame
  d_res$mean_abs_delta = apply(d_deltas, 1, function(x) {
    mean(abs(x), na.rm = T)
  })


  #dispersion error
  d_res$sd = apply(df, 1, sd, na.rm = T) #sd of each persons estimates
  d_res$sd_error = d_res$sd - sd(criteria, na.rm = T)
  d_res$sd_error_abs = d_res$sd_error %>% abs


  #elevation error
  d_res$mean_elevation = apply(df, 1, mean, na.rm = T)
  d_res$mean_elevation_error = d_res$mean_elevation - mean(criteria, na.rm = T)
  d_res$mean_elevation_error_abs = abs(d_res$mean_elevation_error)

  #subset and return
  if ("all" %in% methods) return(d_res)

  return(d_res[methods])
}
