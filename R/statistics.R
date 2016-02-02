## STATISTICS functions


#' Wrapper for rcorr()
#'
#' A wrapper for rcorr() that takes data.frames or matrices as opposed to only matrices.
#' @param x a matrix or data.frame
#' @export
#' @examples
#' rcorr2(iris[-5])
rcorr2 = function(x, ...) {
  library(Hmisc) #for rcorr()
  x = as.matrix(x)
  x = rcorr(x, ...)
  return(x)
}


#' Correlation matrix
#'
#' Outputs a correlation matrix. Supports weights, confidence intervals and rounding.
#' @param data (data.frame or coercible into data.frame) The data.
#' @param weights (numeric vector, numeric matrix/data.frame or character scalar) Weights to use for the correlations. Can be a numeric vector with weights, the name of a variable in data, or a matrix/data.frame with weights for each variable. If the latter, then harmonic means are used. If none given, defaults to rep(1, nrow(data)).
#' @param CI (numeric scalar) The confidence level to use as a fraction.
#' @param CI_template (character scalar) A template to use for formatting the confidence intervals. Defaults to "\%r [\%lower \%upper]".
#' @param skip_nonnumeric (logical scalar) Whether to skip non-numeric variables. Defaults to TRUE.
#' @param CI_round (whole number scalar) If confidence intervals are used, how many digits should be shown?
#' @export
#' @examples
#' cor_matrix(iris)
#' cor_matrix(iris, CI = .95)
cor_matrix = function(data, weights, CI, CI_template = "%r [%lower %upper]", skip_nonnumeric = T, CI_round = 2) {
  library(weights);library(stringr);library(psych);library(psychometric)

  #checks
  data = as.data.frame(data)
  if (skip_nonnumeric) data = extract_num_vars(data)
  if (!is_numeric(data)) stop("data contains non-numeric columns!")

  #weights not given or as character
  if (missing("weights")) weights = rep(1, nrow(data)) #fill 1's
  if (is.character(weights)) {
    weights = data[[weights]] #fetch from data
    data[weights] = NULL #remove from data
  }
  if (anyNA(weights)) stop("weights must not have missing values!")

  #simpel weights?
  simpleweights = length(get_dims(weights)) == 1
  if (simpleweights) {
    if (length(weights) != nrow(data)) stop("weights not the same length as the data!")
  }

  ##simple weights and no CI?
  if (simpleweights && missing("CI")) {
    m = wtd.cors(data, weight = weights)
    return(m)
  }

  #complex weights check
  if (length(get_dims(weights)) == 2) {

    #do weights fit the data?
    weights_data = all(get_dims(weights) == get_dims(data))

    #do weights fit the correlation matrix?
    #weights_cor_matrix = get_dims(weights) == rep(ncol(data), 2)

    if (!weights_data) stop(str_c("weights did not fit the data!"))
  }


  #make matrix
  m = matrix(ncol = ncol(data), nrow = ncol(data))


  #fill in results
  for (row in 1:nrow(m)) {
    for (col in 1:ncol(m)) {
      #next if diagonal or above
      if (col >= row) next

      #simple weights & CI
      if (simpleweights && !missing("CI")) {
        r_obj = wtd.cor(data[row], data[col], weight = weights)
        r_n = count.pairwise(data[row], data[col])
        r_r = r_obj[1] %>% format_digits(digits = CI_round)
        r_CI = CIr(r = r_obj[1], n = r_n, level = CI) %>%
          winsorise(1, -1) %>% #limit CIs to between -1 and 1
          format_digits(digits = CI_round)

        m[row, col] = str_replace(CI_template, "%r", r_r) %>%
          str_replace("%lower", r_CI[1]) %>%
          str_replace("%upper", r_CI[2])
      }

      #complex weights
      if (!simpleweights) {
        v_weights = harmonic.mean((weights[c(row, col)]) %>% t)

        if (missing("CI")) {
          m[row, col] = wtd.cors(data[row], data[col], weight = v_weights)
        } else {
          r_obj = wtd.cor(data[row], data[col], weight = v_weights)
          r_n = count.pairwise(data[row], data[col])
          r_r = r_obj[1] %>% format_digits(digits = CI_round)
          r_CI = CIr(r = r_obj[1], n = r_n, level = CI) %>%
            winsorise(1, -1) %>% #limit CIs to between -1 and 1
            format_digits(digits = CI_round)

          m[row, col] = str_replace(CI_template, "%r", r_r) %>%
            str_replace("%lower", r_CI[1]) %>%
            str_replace("%upper", r_CI[2])
        }

      }
    }
  }

  #make symmetric
  m = MAT_get_half(m) %>% MAT_vector2full

  #dimnames
  rownames(m) = colnames(m) = colnames(data)

  #diag
  diag(m) = 1

  m
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
    message(paste0("Dropping variable number ", drop.num))
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
    message(paste0("Most correlated vars are ", topvars, " r=", r)) #info

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
  message("Dropped the following variables:")
  message(dropped.names)

  #return reduced df
  return(df)
}


# Remove redundant variables
#
#' Remove redundant variables from a data.frame based on a threshold value. This is done by calculating all the intercorrelations, then finding those that correlate at or above the threshold (absolute value), then removing the second pair of each variable and not removing more variables than strictly necessary.
#' @param df (data.frame) A data.frame with numeric variables.
#' @param threshold (numeric scalar) A threshold above which intercorrelations are removed. Defaults to .9.
#' @param cor_method (character scalar) The correlation method to use. Parameter is fed to cor(). Defaults to "pearson".
#' @param messages (boolean) Whether to print diagnostic messages.
#' @keywords psychometrics, latent variable, factor analysis, redundant, variable
#' @export
#' @examples
#' remove_redundant_vars()
remove_redundant_vars2 = function(df, threshold = .9, cor_method = "pearson", messages = T) {
  #Check input
  if (!is.data.frame(df)) {
    stop(paste0("First parameter is not a data frame. Instead it is ", class(df)))
  }
  if (!is.numeric(threshold)) {
    stop(paste0("Second parameter is not numeric. Instead is ", class(num.to.remove)))
  }
  if (!(threshold > 0 && threshold <= 1)) stop("threshold must be 0>x>=1 !")
  if (!is.logical(messages)) stop("messages paramter was not a logical!")

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
    suppressor({message("The following variable pairs had stronger intercorrelations than |", threshold, "|:")}, messages = messages)
    if (messages) round_df(m_long_threshold) %>% print #round and print

  } else {
    suppressor({message("No variables needed to be excluded.")}, messages = messages)

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
  suppressor({message(str_c("The following variables were excluded:"))}, messages = messages)
  suppressor({message(str_c(vars_to_exclude, collapse = ", "))}, messages = messages)
  df = df[!colnames(df) %in% vars_to_exclude]

  #return reduced df
  return(df)
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







#' Score accuracy of estimates.
#'
#' Calculates accuracy measures from a data.frame of estimates using a vector of criteria values.
#' @param df (numeric data.frame) A data.frame with estimates. Rows must be cases.
#' @param criteria (numeric vector) A vector of criteria values to score estimates against.
#' @param methods (character vector) Which measures to return. Defaults to c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs"). Use "all" to get all.
#' @param aggregate (boolean) Whether to use aggregated estimates. Default=F.
#' @param aggregate_function (function) Which function to use for aggregation. Default=base::mean.
#' @param ... (named parameters) Additional parameters to pass to the aggregator function, such as na.rm=T to ignore missing data.
#' @keywords score, estimates, accuracy
#' @export
#' @examples
#' score_accuracy()
score_accuracy = function(df, criteria, methods = c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs"), aggregate = F, aggregate_function = base::mean, ...) {
  #check
  if (!is.function(aggregate_function)) stop("Aggregate function isn't a function!")
  if (missing("df")) stop("Estimates df is missing!")
  if (missing("criteria")) stop("Criteria values vector is missing!")

  #aggregate?
  if (aggregate) { #if the user wants aggregated results

    #convert the df to aggregate estimates
    df = apply(df, 2, aggregate_function, ...) %>% t %>% data.frame
  }

  #detect aggregate
  if (is.vector(df)) { #if df is a vector, assume it is aggregate
    df = df %>% t %>% data.frame #convert to df form
    aggregate = T
  }

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

  #check methods
  if (any(!methods %in% colnames(d_res))) stop(str_c("Some methods were not recognized!: "), setdiff(methods, colnames(d_res)))

  return(d_res[methods])
}


#' Get t value by confidence-level and degree of freedom
#'
#' Wrapper function for \code{qt} to get the t value needed for finding confidence intervals or p values.
#' @param conf (numeric scalar) The confidence level desired as a fraction.
#' @param df (numeric scalar) The degrees of freedom.
#' @param ... (other named arguments) Other arguments to pass to \code{qt}. See that function for details.
#' @export
#' @examples
#' #get t value for 95 pct. confidence interval with df = 20
#' get_t_value(.95, 20)
get_t_value = function(conf, df, ...) {
  value = conf + ((1 - conf) / 2)
  qt(value, df = df, ...)
}


#' Pooled sd
#'
#' Calculate pooled sd.
#' @param x (numeric vector) The data.
#' @param group (vector) Group membership.
#' @export
#' @examples
#' #Wikipedia's example https://en.wikipedia.org/wiki/Pooled_variance
#' v_test_vals = c(31, 30, 29, 42, 41, 40, 39, 31, 28, 23, 22, 21, 19, 18, 21, 20, 19, 18, 17)
#' v_test_group = c(rep(1, 3), rep(2, 4), rep(3, 2), rep(4, 5), rep(5, 5))
#' pool_sd(v_test_vals, v_test_group)
pool_sd = function(x, group) {
  library(plyr)

  #validate input
  if (!is_simple_vector(x)) stop("x must be a vector!")
  group = as.factor(group)
  if (!is.factor(group)) stop("x must be a factor or convertible to that!")

  #to df
  d = data.frame("x" = x, "group" = group)

  #summarize
  d_sum = ddply(d, "group", plyr::summarize,
                df = sum(!is.na(x)) - 1,
                var = var(x, na.rm = TRUE))

  #weighted sum divided by weights (with Bessel's correction), then square root
  #https://en.wikipedia.org/wiki/Pooled_variance
  sqrt(sum(d_sum$df * d_sum$var) / sum(d_sum$df))
}


#' Standardized mean differences
#'
#' Calculate standardized mean differneces between all groups.
#' @param x (numeric vector) A vector of values.
#' @param group (vector) A vector of group memberships.
#' @param central_tendency (function) A function to use for calculating the central tendency. Must support a parameter called na.rm. Ideal choices: mean, median.
#' @param dispersion (character or numeric scalar) Either the name of the metric to use (sd or mad) or a value to use.
#' @param dispersion_method (character scalar) If using one of the built in methods for dispersion, then a character indicating whether to use the pooled value from the total dataset (all), the pairwise comparison (pair), or the sd from the total dataset (total).
#' @export
#' @examples
#' #get t value for 95 pct. confidence interval with df = 20
SMD_matrix = function(x, group, central_tendency = mean, dispersion = "sd", dispersion_method = "all") {
  library(plyr)
  library(magrittr)

  #df form
  d_x = data.frame(x = x, group = as.factor(group))

  #find uniqs
  uniq = levels(d_x$group)

  #how manys groups
  n_groups = length(uniq)

  #make matrix for results
  m = matrix(NA, nrow = n_groups, ncol = n_groups)

  #set names
  colnames(m) = rownames(m) = uniq

  #calculate group centrals
  v_central = sapply(uniq, function(var) {
    central_tendency(x[group == var], na.rm = TRUE)
  })

  #loop for each combo
  for (row_i in seq_along(uniq)) {
    for (col_i in seq_along(uniq)) {
      #skip if dia/above diag
      if (col_i >= row_i) next

      #set valyes
      col = uniq[col_i]
      row = uniq[row_i]

      #partition data
      d_comb = d_x[d_x$group %in% c(col, row), ]

      #dispersion
      if (dispersion == "sd") {
        if (dispersion_method == "all") {
          disp = pool_sd(d_x$x, d_x$group)
        }
        if (dispersion_method == "pair") {
          disp = pool_sd(d_comb$x, d_comb$group)
        }
        if (dispersion_method == "total") disp = sd(d_x$x, na.rm = TRUE)
      } else if (dispersion == "mad") {
        #mean of medians, robust
        if (dispersion_method == "all") {
          disp = daply(d_x, "group", function(part) {
            stats::mad(part$x, na.rm = TRUE)
          }) %>% mean
        }
        if (dispersion_method == "pair") {
          disp = daply(d_comb, "group", function(part) {
            stats::mad(part$x, na.rm = TRUE)
          }) %>% mean
        }
      } else if (is.numeric(dispersion)) disp = dispersion #use given number

      #distance
      diff = central_tendency(d_comb$x[d_comb$group == col]) - central_tendency(d_comb$x[d_comb$group == row])
      SMD = diff / disp
      m[row, col] = SMD
    }
  }

  #expand to full
  m_old = m
  m = m %>% MAT_get_half() %>% MAT_vector2full()
  diag(m) = NA

  #names
  copy_names(m_old, m)

  #
  m
}
