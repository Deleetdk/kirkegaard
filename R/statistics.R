## STATISTICS functions


#' Correlation matrix
#'
#' Outputs a correlation matrix. Supports weights, confidence intervals, correcting for measurement error and rounding.
#'
#' Correction for measurement error is done using the standard Pearson formula: r_true = r_observed / sqrt(reliability_x * reliability_y).
#'
#' Weighted correlations are calculated using wtd.cor or wtd.cors from weights package.
#'
#' Confidence intervals are analytic confidence intervals based on the standard error.
#' @param data (data.frame or coercible into data.frame) The data.
#' @param weights (numeric vector, numeric matrix/data.frame or character scalar) Weights to use for the correlations. Can be a numeric vector with weights, the name of a variable in data, or a matrix/data.frame with weights for each variable. If the latter, then harmonic means are used. If none given, defaults to rep(1, nrow(data)).
#' @param reliabilities (num vector) Reliabities used to correct for measurement error. If not present, assumed to be 1.
#' @param CI (numeric scalar) The confidence level to use as a fraction.
#' @param CI_template (character scalar) A template to use for formatting the confidence intervals.
#' @param skip_nonnumeric (logical scalar) Whether to skip non-numeric variables. Defaults to TRUE.
#' @param CI_round (whole number scalar) If confidence intervals are used, how many digits should be shown?
#' @param p_val (log scalar) If p values are desired, the alpha level to use.
#' @param p_template (chr scalar) If p values are desired, the template to use.
#' @param p_round (int scalar) Number of digits to round p values to. Uses scientific notation for small numbers.
#' @export
#' @examples
#' cor_matrix(iris) #just correlations
#' cor_matrix(iris, CI = .95) #with confidence intervals
#' cor_matrix(iris, CI = .99) #with 99% confidence intervals
#' cor_matrix(iris, p_val = .95) #with p values
#' cor_matrix(iris, p_val = .95, p_template = "%r (%p)") #with p values, with an alternative template
#' cor_matrix(iris, reliabilities = c(.8, .9, .7, .75)) #correct for measurement error
#' cor_matrix(iris, reliabilities = c(.8, .9, .7, .75), CI = .95) #correct for measurement error + CI
cor_matrix = function(data, weights = NULL, reliabilities = NULL, CI = NULL, CI_template = "%r [%lower %upper]", skip_nonnumeric = T, CI_round = 2, p_val = NULL, p_template = "%r [p=%p]", p_round = 3) {

  #checks
  data = as.data.frame(data)
  if (skip_nonnumeric) data = extract_num_vars(data)
  if (!is_numeric(data)) stop("data contains non-numeric columns!")

  #CI and p vals
  if (!is.null(CI) && !is.null(p_val)) stop("Cannot both calculate CIs and p values!")
  v_noextras = is.null(CI) && is.null(p_val)

  #reliabities
  if (is.null(reliabilities)) {
    reliabilities = rep(1, ncol(data))
  } else {
    #check length
    if (length(reliabilities) != ncol(data)) stop("reliabilities length incorrect")
  }

  #weights not given or as character
  if (is.null(weights)) weights = rep(1, nrow(data))
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

  ##simple weights and no extras?
  if (simpleweights && v_noextras) {
    m = weights::wtd.cors(data, weight = weights)

    #correct for unreliability
    m = combine_upperlower(psych::correct.cor(m, reliabilities), psych::correct.cor(m, reliabilities) %>% t)

    #remove impossible values
    m[m > 1] = 1
    m[m < -1] = -1

    #reliabilities in diagonal
    diag(m) = reliabilities

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
      if (simpleweights && !is.null(CI)) {

        #weighted cor
        r_obj = weights::wtd.cor(data[row], data[col], weight = weights)

        #correct for unreliability
        r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

        #winsorize
        r_obj[1] %<>% winsorise(1, -1)

        #sample size
        r_n = psych::count.pairwise(data[row], data[col])

        #format cor
        r_r = r_obj[1] %>% format_digits(digits = CI_round)

        #confidence interval
        r_CI = psychometric::CIr(r = r_obj[1], n = r_n, level = CI) %>%
          winsorise(1, -1) %>% #limit CIs to between -1 and 1
          format_digits(digits = CI_round)

        #format and save
        m[row, col] = stringr::str_replace(CI_template, "%r", r_r) %>%
          str_replace("%lower", r_CI[1]) %>%
          str_replace("%upper", r_CI[2])
      }

      #simple weights & p_val
      if (simpleweights && !is.null(p_val)) {
        #observed r
        r_obj = weights::wtd.cor(data[row], data[col], weight = weights)

        #correct for unreliability
        r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

        #winsorize
        r_obj[1] %<>% winsorise(1, -1)

        #sample size
        r_n = psych::count.pairwise(data[row], data[col])

        #rounding
        r_r = r_obj[1] %>% format_digits(digits = CI_round)

        #format and save
        m[row, col] = stringr::str_replace(p_template, "%r", r_r) %>%
          str_replace("%p", r_obj[4] %>% format(digits = p_round, nsmall = p_round))
      }

      #complex weights
      if (!simpleweights) {
        v_weights = psych::harmonic.mean((weights[c(row, col)]) %>% t)

        if (v_noextras) {
          m[row, col] = weights::wtd.cors(data[row], data[col], weight = v_weights) / sqrt(reliabilities[row] * reliabilities[col])
        }

        if (!is.null(CI)) {
          #observed r
          r_obj = weights::wtd.cor(data[row], data[col], weight = v_weights)

          #correct for unreliability
          r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

          #winsorize
          r_obj[1] %<>% winsorise(1, -1)

          #sample size
          r_n = psych::count.pairwise(data[row], data[col])

          #format r
          r_r = r_obj[1] %>% format_digits(digits = CI_round)

          #CI
          r_CI = psychometric::CIr(r = r_obj[1], n = r_n, level = CI) %>%
            winsorise(1, -1) %>% #limit CIs to between -1 and 1
            format_digits(digits = CI_round)

          #format and save
          m[row, col] = stringr::str_replace(CI_template, "%r", r_r) %>%
            str_replace("%lower", r_CI[1]) %>%
            str_replace("%upper", r_CI[2])
        }

        if (!is.null(p_val)) {
          #observed r
          r_obj = weights::wtd.cor(data[row], data[col], weight = v_weights)

          #correct for unreliability
          r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

          #winsorize
          r_obj[1] %<>% winsorise(1, -1)

          #n
          r_n = psych::count.pairwise(data[row], data[col])

          #format r
          r_r = r_obj[1] %>% format_digits(digits = CI_round)

          #format and save
          m[row, col] = stringr::str_replace(p_template, "%r", r_r) %>%
            stringr::str_replace("%p", r_obj[4] %>% format(digits = p_round, nsmall = p_round))
        }

      }
    }
  }

  #make symmetric
  m = MAT_half(m) %>% MAT_vector2full

  #dimnames
  rownames(m) = colnames(m) = colnames(data)

  #diag
  diag(m) = reliabilities

  m
}


#' Remove redundant variables
#'
#' Remove redundant variables from a data.frame based on a threshold value. This is done by calculating all the intercorrelations, then finding those that correlate at or above the threshold (absolute value), then removing the second pair of each variable and not removing more variables than strictly necessary.
#' @param df (data.frame) A data.frame with numeric variables.
#' @param threshold (numeric scalar) A threshold above which intercorrelations are removed. Defaults to .9.
#' @param cor_method (character scalar) The correlation method to use. Parameter is fed to cor(). Defaults to "pearson".
#' @param messages (boolean) Whether to print diagnostic messages.
#' @export
#' @examples
#' remove_redundant_vars(iris[-5]) %>% head
remove_redundant_vars = function(df, threshold = .9, cor_method = "pearson", messages = T) {
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
    silence({message("The following variable pairs had stronger intercorrelations than |", threshold, "|:")}, messages = messages)
    if (messages) df_round(m_long_threshold) %>% print #round and print

  } else {
    silence({message("No variables needed to be excluded.")}, messages = messages)

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
  silence({message(str_c("The following variables were excluded:"))}, messages = messages)
  silence({message(str_c(vars_to_exclude, collapse = ", "))}, messages = messages)
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
#' @export
semi_par = function(x, y, z, weights = NULL, complete_cases = T) {

  #x vector
  x = as.vector(x)
  y = as.vector(y)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
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
  r_sp = weights::wtd.cor(df$x, df$y_res, weight = df$w)
  r = weights::wtd.cor(df$x, df$y, weight = df$w)
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
#' @export
semi_par_serial = function(df, dependent, primary, secondaries, weights = NULL, standardize = T) {

  #subset and deal with lack of weights
  if (is.null(weights)) {
    weights = "weights_var"
    df[weights] = rep(1, nrow(df))
  } else {
    df["weights_var"] = df[weights] #move the weights var to another name
    weights = "weights_var"
  }

  #subset
  df = subset(df, select = c(dependent, primary, secondaries, weights))

  #complete cases only
  df = na.omit(df)

  #standardize
  if (standardize) df = df_standardize(df, exclude = weights)

  #primary
  r_prim = round(weights::wtd.cor(df[, dependent], df[, primary], weight = df[, weights]), 2)

  #make results object
  results = data.frame(matrix(nrow = length(secondaries), ncol = 2))
  rownames(results) = secondaries
  colnames(results) = c("Orig. cor", "Semi-partial cor")
  #loop over each secondary
  for (sec_idx in seq_along(secondaries)) {

    #the current secondary var
    tmp_secondary = secondaries[sec_idx]

    #make the model
    tmp_model = stringr::str_c(dependent, " ~ ", primary)

    #regress
    df$tmp_resids = resid(lm(as.formula(tmp_model), weights = df[, weights], data = df))

    #secondary original
    r_sec = weights::wtd.cor(df[, dependent], df[, tmp_secondary], weight = df[, weights])[1]

    #semi-partial
    r_sec_sp = weights::wtd.cor(df$tmp_resids, df[, tmp_secondary], weight = df[, weights])[1]

    #save
    results[sec_idx, ] = c(r_sec, r_sec_sp)
  }

  return(results)
}



#' Calculate a partial correlation.
#'
#' Calculates the partial correlation.
#' @param df A data frame.
#' @param x String with the name of the first variable.
#' @param y String with the name of the second variable.
#' @param z String with the name of the control variable.
#' @param weights_var String with the name of the weights variable. Can be left out.
#' @export
MOD_partial = function(df, x, y, z, weights_var = NULL) {
  df
  x
  y
  z

  #make or move weights
  if (is.null(weights_var)) {
    df$weights___ = rep(1, nrow(df)) #make unit weights
  } else {
    df$weights___ = df[[weights_var]] #reassign weights var
  }
  weights_var = "weights___"

  #build models
  mod1 = stringr::str_c(x, " ~ ", str_c(z, collapse = " + "))
  mod2 = stringr::str_c(y, " ~ ", str_c(z, collapse = " + "))

  #fit models
  fit1 = lm(mod1, data = df, weights = weights___, na.action = na.exclude)
  fit2 = lm(mod2, data = df, weights = weights___, na.action = na.exclude)
  #na.exclude is important becus otherwise NA values are removed

  #get residuals
  resid1 = resid(fit1)
  resid2 = resid(fit2)

  #correlate
  r = weights::wtd.cor(resid1, resid2, weight = df$weights___)

  return(r[1])
}



#' Score accuracy of estimates.
#'
#' Calculates accuracy measures from a data.frame of estimates using a vector of criteria values.
#' @param x (numeric data.frame) A data.frame with estimates. Rows must be cases. Alternatively, a vector of values. If given a vector, it will assume the user wants aggregate-level estimates.
#' @param criterion (numeric vector) A vector of criteria values to score estimates against.
#' @param methods (character vector) Which measures to return. Defaults to c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs"). Use "all" to get all.
#' @param aggregate (boolean) Whether to use aggregated estimates. Default=F.
#' @param aggregate_function (function) Which function to use for aggregation. Default=base::mean.
#' @param ... (named parameters) Additional parameters to pass to the aggregator function, such as na.rm=T to ignore missing data.
#' @export
score_accuracy = function(x, criterion, methods = c("pearson_r", "mean_abs_delta", "sd_error_abs", "mean_elevation_error_abs"), aggregate = F, aggregate_function = base::mean, ...) {
  #save rownames
  v_rownames = rownames(x)

  #check
  x
  criterion
  if (!is.function(aggregate_function)) stop("Aggregate function isn't a function!")


  #aggregate?
  if (aggregate) { #if the user wants aggregated results

    #convert the x to aggregate estimates
    x = apply(x, 2, aggregate_function, ...) %>% t %>% data.frame
  }

  #detect aggregate
  if (is.vector(x)) { #if x is a vector, assume it is aggregate
    x = x %>% t %>% data.frame #convert to df form
    aggregate = T
  }

  #NAs
  if (any(is.na(x))) message("Note: some data were missing. This function uses pairwise complete cases.")

  #make df for results
  df = as.data.frame(x)
  criterion = unlist(criterion) %>% as.vector
  d_res = data.frame(matrix(nrow = nrow(df), ncol = 0))


  #Pearson r
  d_res$pearson_r = sapply(1:nrow(df), function(x) {
    cor(criterion, df[x, ] %>% unlist, use = "p")
  })


  #rank-order r
  d_res$rank_r = sapply(1:nrow(df), function(x) {
    cor(criterion, df[x, ] %>% unlist, method = "spearman", use = "p")
  })


  #delta (discrepancy) error
  d_deltas = (t(df) - criterion) %>% t %>% as.data.frame
  d_res$mean_abs_delta = apply(d_deltas, 1, function(x) {
    mean(abs(x), na.rm = T)
  })


  #dispersion error
  d_res$sd = apply(df, 1, sd, na.rm = T) #sd of each persons estimates
  d_res$sd_error = d_res$sd - sd(criterion, na.rm = T)
  d_res$sd_error_abs = d_res$sd_error %>% abs


  #elevation error
  d_res$mean_elevation = apply(df, 1, mean, na.rm = T)
  d_res$mean_elevation_error = d_res$mean_elevation - mean(criterion, na.rm = T)
  d_res$mean_elevation_error_abs = abs(d_res$mean_elevation_error)

  #rownames
  if (!aggregate) { #dont set if we are using aggregate data
    rownames(d_res) = v_rownames
  }

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

  #validate input
  if (!is_simple_vector(x)) stop("x must be a vector!")
  group = as.factor(group)
  if (!is.factor(group)) stop("x must be a factor or convertible to that!")

  #to df
  d = data.frame("x" = x, "group" = group)

  #summarize
  d_sum = plyr::ddply(d, "group", plyr::summarize,
                df = sum(!is.na(x)) - 1,
                var = var(x, na.rm = TRUE)) %>%
    na.omit() #missing data groups

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
#' @param ... (other arguments) Additional arguments to pass to the central tendency function.
#' @export
#' @examples
#' SMD_matrix(iris$Sepal.Length, iris$Species)
SMD_matrix = function(x, group, central_tendency = wtd_mean, dispersion = "sd", dispersion_method = "all", ...) {
  #input
  x
  group


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
          disp = plyr::daply(d_x, "group", function(part) {
            stats::mad(part$x, na.rm = TRUE)
          }) %>% mean
        }
        if (dispersion_method == "pair") {
          disp = plyr::daply(d_comb, "group", function(part) {
            stats::mad(part$x, na.rm = TRUE)
          }) %>% mean
        }
      } else if (is.numeric(dispersion)) disp = dispersion #use given number

      #difference
      diff = central_tendency(d_comb$x[d_comb$group == col], ...) - central_tendency(d_comb$x[d_comb$group == row], ...)

      #devide by dispersion measure
      SMD = diff / disp

      #save
      m[row, col] = SMD
    }
  }

  #expand to full
  m_old = m
  m = m %>% MAT_half() %>% MAT_vector2full()
  diag(m) = NA

  #names
  copy_names(m_old, m)

  #
  m
}


#' Calculate homogeneity/heterogeneity
#'
#' Calculate a simple index of homogeneity for nominal data, that is variously called Simpson's, Herfindahl's or Hirschman's index.
#' @param x (a vector) A vector of values.
#' @param reverse (log scalar) Whether to reverse the index to index heterogeneity (default false).
#' @param summary (log scalar) Whether to treat data as summary statistics of the group proportions (default false). If data are given in 0-100 format, it will automatically convert.
#' @export
#' @examples
#' homogeneity(iris$Species)
#' homogeneity(iris$Species, reverse = T)
#' homogeneity(c(.7, .2, .1), summary = T)
#' homogeneity(c(80, 15, 5), summary = T)
homogeneity = function(x, reverse = F, summary = F) {

  #not using summary statistics
  if (!summary) {
    #reversed
    if (!reverse) {
      return(table(x) %>% prop.table %>% as.vector %>% raise_to_power(2) %>% sum)
    }

    #reversed
    return(table(x) %>% prop.table %>% as.vector %>% raise_to_power(2) %>% sum %>% subtract(1, .))
  }

  #using summary statistics
  if (sum(x) %>% is_between(.99, 1.01)) {
    #not reversed
    if (!reverse) {
      return(x %>% raise_to_power(2) %>% sum)
    }

    #reversed
    return(x %>% raise_to_power(2) %>% sum() %>% subtract(1, .))
  } else if (sum(x) %>% is_between(99, 101)) {
    #not reversed
    if (!reverse) {
      return(x %>% divide_by(100) %>% raise_to_power(2) %>% sum)
    }

    #reversed
    return(x %>% divide_by(100) %>% raise_to_power(2) %>% sum %>% subtract(1, .))

  } else {
      stop("Tried to use summary statistics, but they did not sum to either around 1 or 100 (±1%)")
    }

}


#' Calculate weighted standard deviation
#'
#' Calculated the weighted standard deviation using a vector of values and a vector of weights.
#' @param x (num vector) A vector of values.
#' @param w (num vector) A vector of weights.
#' @param sample (log scalar) Whether this is a sample as opposed to a population (default true).
#' @param error (lgl scalr) Whether to throw an error if there is no data at all or no pairwise complete cases. Default yes.
#' @export
#' @examples
#' set.seed(1)
#' X = rnorm(100)
#' set.seed(1)
#' W = runif(100)
#' sd(X) #0.898
#' wtd_sd(X, W) #0.894, slightly different
#' wtd_sd(X) #0.898, not using weights
wtd_sd = function(x, w = NULL, sample = T, error = F) {
  #x vector
  x = as.vector(x)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
  }

  #make temp df
  d = data.frame(x = x, w = w) %>% na.omit

  #check sample
  if (nrow(d) == 0 & error) stop("There were no complete cases!")
  if (nrow(d) == 0) return(NA) #return NA on no cases

  #weighted mean
  wtd_mean = wtd_mean(x, w, error = error)

  #diffs squared
  diffs_sq = (x - wtd_mean)^2

  #weighted variance
  if (sample) wtd_var = sum(diffs_sq, na.rm = T) / (count_NA(x, reverse = T) - 1)
  if (!sample) wtd_var = sum(diffs_sq, na.rm = T) / count_NA(x, reverse = T)

  #weighted sd
  sqrt(wtd_var)
}


#' Calculate a weighted mean
#'
#' This is an improvement on \code{\link{weighted.mean}} in \code{base-r}.
#'
#' The original function returns \code{NA} when there are missing values in the weights vector despite na.rm=T. This function avoids that problem. It also returns a useful error message if there are no complete cases. The function wraps base-r's function.
#' @param x (num vector) A vector of values.
#' @param w (num vector) A vector of weights.
#' @param error (lgl scalr) Whether to throw an error if there is no data at all or no pairwise complete cases. Default yes.
#' @export
#' @examples
#' set.seed(1)
#' X = rnorm(100)
#' set.seed(1)
#' W = runif(100)
#' wtd_mean(X) # not using weights
#' mean(X) #same as above
#' wtd_mean(X, W) #slightly different
wtd_mean = function(x, w = NULL, error = F) {

  #x vector
  x = as.vector(x)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
  }

  #lengths
  if (!lengths_match(x, w)) stop("Lengths of x and w do not match!")

  #make temp df
  d = data.frame(x = x, w = w) %>% na.omit

  #check sample
  if (nrow(d) == 0 & error) stop("There were no complete cases!")
  if (nrow(d) == 0) return(NA) #return NA on no cases

  #else
  weighted.mean(x = d$x, w = d$w)
}

#' Calculate a weighted sum
#'
#' This is an improvement on \code{\link{sum}} in \code{base-r}.
#'
#' It automatically handles missing data. It returns a useful error message if there are no complete cases.
#' @param x (num vector) A vector of values.
#' @param w (num vector) A vector of weights.
#' @param error (lgl scalr) Whether to throw an error if there is no data at all or no pairwise complete cases. Default yes.
#' @export
#' @examples
#' set.seed(1)
#' X = rnorm(100)
#' set.seed(1)
#' W = runif(100)
#' wtd_sum(X) # not using weights
#' sum(X) #same as above
#' wtd_sum(X, W) #different
wtd_sum = function(x, w = NULL, error=F) {
  #x vector
  x = as.vector(x)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
  }

  #lengths
  lengths_match(x, w)

  #make temp df
  d = data.frame(x = x, w = w) %>% na.omit

  #check sample
  if (nrow(d) == 0 & error) stop("There were no complete cases!")
  if (nrow(d) == 0) return(NA) #return NA on no cases

  #calculate
  x_w = sum(d$x * d$w, na.rm = T) # sum of x * w
  w_sum = sum(d$w, na.rm = T) # sum of w
  (x_w/w_sum) * count_NA(d$x, reverse = T) #weighted sum
}


#' Standardize a vector
#'
#' Standardize a vector. Can use weights and robust measures of central tendency and dispersion. Returns a clean vector as opposed to base-r's \code{\link{scale}}.
#' @param x (num vector) A vector of values.
#' @param w (num vector) A vector of weights.
#' @param robust (log vector) Whether to use robust measures (default false). See \code{\link{mad}} and \code{\link{median}}.
#' @param sample (log scalar) Whether this is a sample as opposed to a population (default true).
#' @export
#' @examples
#' set.seed(1)
#' X = rnorm(100, mean = 10, sd = 5)
#' set.seed(1)
#' W = runif(100)
#' standardize(X, W)
#' standardize(X, robust = T) #almost the same for these data
standardize = function(x, w = NULL, robust = F, sample = T) {
  #x vector
  x = as.vector(x)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
  }

  #parametric
  if (!robust) {
    #weighted mean
    wtd_mean = weighted.mean(x, w, na.rm = T)

    #diffs squared
    diffs_sq = (x - wtd_mean)^2

    #weighted variance
    if (sample) wtd_var = sum(diffs_sq, na.rm = T) / (count_NA(x, reverse = T) - 1)
    if (!sample) wtd_var = sum(diffs_sq, na.rm = T) / count_NA(x, reverse = T)

    #weighted sd (sample)
    wtd_sd = sqrt(wtd_var)

    #standardize
    return((x - wtd_mean) / wtd_sd)
  }

  #robust
  #standard
  if (robust) {
    #median
    mdn = median(x, na.rm = T)

    #mad
    mad = mad(x, na.rm = T)

    #standardize
    return((x - mdn) / mad)
  }
}


#' Find a cutoff of a normal distribution that results in a given mean trait value above the cutoff
#'
#' Assuming a normal distribution for a trait and a cutoff value. Estimate what this cutoff value is to obtain a population above the cutoff with a known mean trait value.
#' @param mean_above (num scalar) Mean trait level of population above the cutoff.
#' @param mean_pop (num scalar) Mean trait level of population. Default = 100 (IQ scale).
#' @param sd_pop (num scalar) Standard deviation of the trait in the population. Default = 15 (IQ scale).
#' @param n (num scalar) Sample size to generate in the process. More results in higher precision and memory use. Default = 1e4.
#' @param precision (num scalar) The precision to use. Default = .1.
#' @param below (log scalar) Reverse the model to find cutoffs for
#' @export
#' @examples
#' #what cutoff is needed to get a population above the cutoff with a mean of 115 when the population mean is 100?
#' find_cutoff(115)
#' #try impossible
#' find_cutoff(95)
find_cutoff = function(mean_above, mean_pop = 100, sd_pop = 15, n = 1e4, precision = .1, below = F) {
  #chechk
  if (mean_above < mean_pop) stop("This model is inapplicable if the mean trait level is lower than the population mean!")

  #dangerous loop!
  cutoff = mean_pop #begin with unselected group
  iter = 1
  while (T) {
    #replicable
    set.seed(1)

    #make a population
    population = rnorm(n = n, mean = mean_pop, sd = sd_pop)

    #get the population above the cutoff
    population_above = population[population > cutoff]

    #mean above
    population_above_mean = mean(population_above)

    #check
    v_diff = mean_above - population_above_mean
    if (abs(v_diff) < precision) {
      return(cutoff)
    }

    #adjust cutoff
    if (v_diff > 0) cutoff = cutoff + precision
    if (v_diff < 0) cutoff = cutoff - precision

    #iter + 1
    iter = iter + 1

    #check if infinite
    if (iter >= 1e5) stop("Loop reached 100k iterations without finding a solution! Use a lower level of precision!")
  }
}
