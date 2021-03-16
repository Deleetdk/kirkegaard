## STATISTICS functions


#' Describe data
#'
#' @param ... Inut for [psych::describe]
#'
#' @return Tibble subset
#' @export
#'
#' @examples
#' describe2(iris)
#' describe2(mpg)
describe2 = function(...) {
  #skip factors
  dots = list(...)

  #get results
  y = psych::describe(...)

  #fix class
  class(y) = "data.frame"

  #subset, explicit rownames
  y %>% rownames_to_column(var = "var") %>% select(var, n, mean, median, sd, mad, min, max, skew, kurtosis) %>% as_tibble()
}


#' Adjust Cohen's d for measurement error
#'
#' Adjust' Cohen's d for measurement error using the Pearson r approach
#'
#' Convert the d to Pearson r, then applies Spearman's correction, and then converts back.
#' @param d (num) Cohen's d.
#' @param n1 (int) Sample size for group 1.
#' @param n2 (int) Sample size for group 2.
#' @param rxx (num) Reliability of x.
#' @param ryy (num) Reliability of y.
#' @export
#' @examples
#' a4me_cohen_d(1, 100, 100) #no adjustment
#' a4me_cohen_d(1, 100, 100, rxx = .5) #adjust based on x
#' a4me_cohen_d(1, 100, 100, ryy = .5) #based on y, same result
#' a4me_cohen_d(1, 100, 100, rxx = .5, ryy = .5) #based on both; large effect!
#' a4me_cohen_d(1, 100, 100, rxx = 0) #infinite d if no reliability
#' a4me_cohen_d(10, 100, 100, rxx = .1, ryy = .1) #infinite also if corrected correlation exceeds the bounds
a4me_cohen_d = function(d, n1, n2, rxx = 1, ryy = 1) {
  #check input
  d
  n1
  n2

  #if no adjust necessary
  if (rxx == 1 & ryy == 1) return(d)

  #impossible reliabilities
  if (!is_between(rxx, 0, 1)) stop("Reliability for x was not within bounds!")
  if (!is_between(ryy, 0, 1)) stop("Reliability for y was not within bounds!")

  #convert to pearson r
  a = (n1 + n2)^2/(n1 * n2)
  r = d / sqrt(d^2 + a)

  #adjust r
  r_adj = r / sqrt(rxx * ryy)

  #check if out of bounds
  if (r_adj >= 1) return(Inf)
  if (r_adj <= -1) return(-Inf)

  #convert to cohen d
  d_adj = (2 * r) / sqrt(1 - r_adj^2)

  d_adj
}



#' Correlation matrix
#'
#' Outputs a correlation matrix. Supports weights, confidence intervals, correcting for measurement error and rounding.
#'
#' Correction for measurement error is done using the standard Pearson formula: r_true = r_observed / sqrt(reliability_x * reliability_y).
#'
#' Weighted correlations are calculated using wtd.cor or wtd.cors from weights package.
#'
#' `rank_order` can take either a logical scalar or a character scalar. If given TRUE, it will use rank ranking method with the default settings (average ranks). If given a chr scalar, it will use that ranking method. If given FALSE, will not use rank data (default).
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
#' @param rank_order (lgl or chr) Whether to use rank ordered data so as to compute Spearman's correlations instead.
#' @export
#' @examples
#' cor_matrix(iris) #just correlations
#' cor_matrix(iris, CI = .95) #with confidence intervals
#' cor_matrix(iris, CI = .99) #with 99% confidence intervals
#' cor_matrix(iris, p_val = .95) #with p values
#' cor_matrix(iris, p_val = .95, p_template = "%r (%p)") #with p values, with an alternative template
#' cor_matrix(iris, reliabilities = c(.8, .9, .7, .75)) #correct for measurement error
#' cor_matrix(iris, reliabilities = c(.8, .9, .7, .75), CI = .95) #correct for measurement error + CI
#' cor_matrix(iris, rank_order = T) #rank order correlations, default method
#' cor_matrix(iris, rank_order = "first") #rank order correlations, specific method
#' cor_matrix(iris, weights = "Petal.Width") #weights from name
#' cor_matrix(iris, weights = 1:150) #weights from vector
cor_matrix = function(data, weights = NULL, reliabilities = NULL, CI = NULL, CI_template = "%r [%lower %upper]", skip_nonnumeric = T, CI_round = 2, p_val = F, p_template = "%r%a [p=%p]", p_round = 3, rank_order = F, asterisks = c(.01, .005, .001), asterisks_only = T) {


  #checks
  data = as.data.frame(data)
  if (skip_nonnumeric) data = extract_num_vars(data)
  if (!is_numeric(data)) stop("data contains non-numeric columns!")
  is_(rank_order, class = c("character", "logical"), size = 1, error_on_false = T)

  #CI and p vals
  if (!is.null(CI) && p_val) stop("Cannot both calculate CIs and p values!")
  v_noextras = is.null(CI) && !p_val

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
  }
  if (anyNA(weights)) stop("weights must not have missing values!")

  #simple weights?
  simpleweights = length(get_dims(weights)) == 1
  if (simpleweights) {
    if (length(weights) != nrow(data)) stop("weights not the same length as the data!")
  }

  #rank order data?
  if (is.logical(rank_order) && rank_order) {
    data = df_rank(data)
  }
  if (is.character(rank_order)) {
    data = df_rank(data, ties.method = rank_order)
  }

  ##simple weights and no extras?
  if (simpleweights && v_noextras) {

    m = weights::wtd.cors(data, weight = weights)

    #correct for unreliability
    m = combine_upperlower(psych::correct.cor(m, reliabilities), psych::correct.cor(m, reliabilities) %>% t)

    #winsor to -1 to 1
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
        r_n = psych::pairwiseCount(data[row], data[col])

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
      if (simpleweights && p_val) {

        #observed r
        r_obj = weights::wtd.cor(data[row], data[col], weight = weights)

        #correct for unreliability
        r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

        #winsorize
        r_obj[1] %<>% winsorise(1, -1)

        #sample size
        r_n = psych::pairwiseCount(data[row], data[col])

        #rounding
        r_r = r_obj[1] %>% format_digits(digits = CI_round)

        #finalize with p value
        if (asterisks_only) {
          m[row, col] = r_r + p_to_asterisk(r_obj[, "p.value"], asterisks = asterisks, asterisks_only = T)
        } else {

          m[row, col] = p_template %>%
            str_replace("%r", r_r) %>%
            str_replace("%p", p_to_asterisk(r_obj[, "p.value"], asterisks = asterisks, asterisks_only = F))
        }
      }

      #complex weights
      if (!simpleweights) {
        #for each case, compute the harmonic mean weight
        v_weights = weights[, c(row, col)] %>%
          t() %>%
          psych::harmonic.mean()

        #if plain correlation output, compute and be done
        if (v_noextras) {
          m[row, col] = weights::wtd.cors(data[row], data[col], weight = v_weights) / sqrt(reliabilities[row] * reliabilities[col])
        }

        #if CI wanted
        if (!is.null(CI)) {
          #observed r
          r_obj = weights::wtd.cor(data[row], data[col], weight = v_weights)

          #correct for unreliability
          r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

          #winsorize
          r_obj[1] %<>% winsorise(1, -1)

          #sample size
          r_n = psych::pairwiseCount(data[row], data[col])

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

        #if p value wanted
        if (p_val) {
          #observed r
          r_obj = weights::wtd.cor(data[row], data[col], weight = v_weights)

          #correct for unreliability
          r_obj[1] %<>% {. / sqrt(reliabilities[col] * reliabilities[row])}

          #winsorize
          r_obj[1] %<>% winsorise(1, -1)

          #n
          r_n = psych::pairwiseCount(data[row], data[col])

          #format r
          r_r = r_obj[1] %>% format_digits(digits = CI_round)

          #finalize with p value
          if (asterisks_only) {
            m[row, col] = r_r + p_to_asterisk(r_obj[, "p.value"], asterisks = asterisks, asterisks_only = T)
          } else {

            m[row, col] = p_template %>%
              str_replace("%r", r_r) %>%
              str_replace("%p", p_to_asterisk(r_obj[, "p.value"], asterisks = asterisks, asterisks_only = F))
          }
        }

      }
    }
  }

  #make symmetric
  m = MAT_half(m) %>% MAT_vector2full()

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
#' @param extended_output (lgl) Whether to output a list of matrices. Useful for computational reuse.
#' @param CI (num) Confidence interval coverage.
#' @param str_template (chr) A string template to use.
#' @param reliability (num) A reliability to use for correcting for measurement error. Done via [kirkegaard::a4me_cohen_d()].
#' @param se_analytic (lgl) Use analytic standard errors. If not, then it will bootstrapping (slower). Always uses bootstrapping for non-mean functions.
#' @param ... (other arguments) Additional arguments to pass to the central tendency function.
#' @export
#' @examples
#' SMD_matrix(iris$Sepal.Length, iris$Species)
#' SMD_matrix(iris$Sepal.Length, iris$Species, extended_output = T)
#' #pairwise SDs
#' SMD_matrix(iris$Sepal.Length, iris$Species, extended_output = T, dispersion_method = "pair")
SMD_matrix = function(x,
                      group,
                      central_tendency = wtd_mean,
                      dispersion = "sd",
                      dispersion_method = "all",
                      extended_output = F,
                      CI = .95,
                      str_template = "%d [%lower %upper]",
                      str_round_to = 2,
                      reliability = 1,
                      se_analytic = T,
                      ...) {
  #input
  x
  group

  #CI z score
  se_CI = qnorm(CI + (1 - CI)/2)

  #df form
  d_x = data.frame(x = x, group = as.factor(group)) %>%
    #remove missing data
    na.omit

  #find uniqs
  uniq = levels(d_x$group)

  #how many groups
  n_groups = length(uniq)

  #group sample sizes
  group_ns = table2(d_x[[2]], include_NA = F)
  group_ns = set_names(group_ns$Count, group_ns$Group) #change to named vector

  #make matrices for results
  m = matrix(NA, nrow = n_groups, ncol = n_groups)

  #set names
  colnames(m) = rownames(m) = uniq

  #copies
  CI_lower = m
  CI_upper = m
  se = m
  pairwise_n = m
  m_str = m
  pval = m

  #loop for each combo
  for (row_i in seq_along(uniq)) {
    for (col_i in seq_along(uniq)) {
      #skip effect size if dia/above diag
      if (col_i >= row_i) next

      #set values
      col = uniq[col_i]
      row = uniq[row_i]

      #partition data
      d_comb = d_x[d_x$group %in% c(col, row), ]

      #remove unused factor levels
      d_comb$group = forcats::fct_drop(d_comb$group)

      #dispersion
      if (dispersion == "sd") {
        if (dispersion_method == "all") {
          disp = pool_sd(d_x$x, d_x$group)
        }
        if (dispersion_method == "pair") {
          disp = pool_sd(d_comb$x, d_comb$group)
        }
        if (dispersion_method == "total") disp = sd(d_x$x)
      } else if (dispersion == "mad") {
        #mean of medians, robust
        if (dispersion_method == "all") {
          disp = plyr::daply(d_x, "group", function(part) {
            stats::mad(part$x)
          }) %>% mean
        }
        if (dispersion_method == "pair") {
          disp = plyr::daply(d_comb, "group", function(part) {
            stats::mad(part$x)
          }) %>% mean
        }
      } else if (is.numeric(dispersion)) disp = dispersion #use given number

      #difference
      diff = central_tendency(d_comb$x[d_comb$group == col], ...) - central_tendency(d_comb$x[d_comb$group == row], ...)

      #devide by dispersion measure
      SMD = diff / disp

      #save effect size
      m[row, col] = SMD

      #pairwise sample size
      pairwise_n[row, col] = d_comb %>% nrow

      #group sample sizes
      pair_ns = group_ns[c(row, col)]

      #adjust for reliability?
      if (reliability != 1) {
        m[row, col] = a4me_cohen_d(d = m[row, col], n1 = pair_ns[1], n2 = pair_ns[2], rxx = reliability)
      }

      #standard error
      #https://stats.stackexchange.com/questions/144084/variance-of-cohens-d-statistic
      #note variance vs. se (hence sqrt)!
      se[row, col] = sqrt((sum(pair_ns)/prod(pair_ns)) + ((m[row, col]^2) / (2*(sum(pair_ns) - 2))))

      #CIs
      CI_upper[row, col] = m[row, col] + se_CI * se[row, col]
      CI_lower[row, col] = m[row, col] - se_CI * se[row, col]

      #p val, 2-tailed
      pval[row, col] = pnorm(abs(abs(m[row, col]) / se[row, col]), lower.tail = F) * 2

      #make string
      formater = function(x, round_to = str_round_to) {
        #as chr
        format(x, digits = round_to, nsmall = round_to)
      }
      m_str[row, col] = str_template %>%
        #insert estimate
        str_replace_all(pattern = "%d", replacement = m[row, col] %>% formater) %>%
        #insert upper CI
        str_replace_all(pattern = "%upper", replacement = CI_upper[row, col] %>% formater) %>%
        #insert lower CI
        str_replace_all(pattern = "%lower", replacement = CI_lower[row, col] %>% formater) %>%
        #insert n
        str_replace_all(pattern = "%n", replacement = pairwise_n[row, col] %>% as.character) %>%
        #insert se
        str_replace_all(pattern = "%se", replacement = se[row, col] %>% formater)
    }
  }

  #names
  copy_names(m, m_str)
  copy_names(m, se)
  copy_names(m, CI_lower)
  copy_names(m, CI_upper)
  copy_names(m, pval)
  copy_names(m, pairwise_n)

  #fill upper halves
  m %<>% MAT_half2full(diag=T)
  m_str %<>% MAT_half2full(diag=T)
  se %<>% MAT_half2full(diag=T)
  CI_upper %<>% MAT_half2full(diag=T)
  CI_lower %<>% MAT_half2full(diag=T)
  pval %<>% MAT_half2full(diag=T)
  pairwise_n %<>% MAT_half2full(diag=T)
  #mode(pairwise_n) = "integer" #change to integer to prevent downstream problems

  #output
  if (!extended_output) {
    return(m)
  } else {
    list(
      d = m,
      d_string = m_str,
      CI_lower = CI_lower,
      CI_upper = CI_upper,
      se_z = se_CI,
      se = se,
      p = pval,
      pairwise_n = pairwise_n
    )
  }
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
      stop("Tried to use summary statistics, but they did not sum to either around 1 or 100 ( 1%)")
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
  if (sample) wtd_var = sum(diffs_sq, na.rm = T) / (miss_count(x, reverse = T) - 1)
  if (!sample) wtd_var = sum(diffs_sq, na.rm = T) / miss_count(x, reverse = T)

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
  stats::weighted.mean(x = d$x, w = d$w)
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
  (x_w / w_sum) * miss_count(d$x, reverse = T) #weighted sum
}


#' Standardize a vector
#'
#' Standardize a vector. Can use weights and robust measures of central tendency and dispersion. Returns a clean vector as opposed to base-r's \code{\link{scale}}.
#' @param x (num vector) A vector of values.
#' @param w (num vector) A vector of weights.
#' @param robust (log vector) Whether to use robust measures (default false). See \code{\link{mad}} and \code{\link{median}}.
#' @param sample (log scalar) Whether this is a sample as opposed to a population (default true).
#' @param focal_group (lgl vector) A subset of the data to standardize the values to. This is useful when you want one subgroup to be the focal group, using their mean/sd as 0/1.
#' @export
#' @examples
#' set.seed(1)
#' X = rnorm(100, mean = 10, sd = 5)
#' set.seed(1)
#' W = runif(100)
#' standardize(X, W)
#' standardize(X, robust = T) #almost the same for these data
standardize = function(x, w = NULL, robust = F, sample = T, focal_group = NULL) {
  #x vector
  x = as.vector(x)

  #weights
  if (is.null(w)) {
    w = rep(1, length(x))
  } else {
    w = as.vector(w)
  }

  #subset?
  if (is.null(focal_group)) focal_group = rep(T, length(x))
  #assert equal lengths
  assertthat::assert_that(length(focal_group) == length(x))
  assertthat::assert_that(is.logical(focal_group))

  #full x
  full_x = x
  x = x[focal_group]
  w = w[focal_group]

  #parametric
  if (!robust) {
    #weighted mean
    wtd_mean = weighted.mean(x, w, na.rm = T)

    #diffs squared
    diffs_sq = (x - wtd_mean)^2

    #weighted variance
    if (sample) wtd_var = sum(diffs_sq, na.rm = T) / (miss_count(x, reverse = T) - 1)
    if (!sample) wtd_var = sum(diffs_sq, na.rm = T) / miss_count(x, reverse = T)

    #weighted sd (sample)
    wtd_sd = sqrt(wtd_var)

    #standardize
    return((full_x - wtd_mean) / wtd_sd)
  }

  #robust
  #standard
  if (robust) {
    #median
    mdn = median(x, na.rm = T)

    #mad
    mad = mad(x, na.rm = T)

    #standardize
    return((full_x - mdn) / mad)
  }
}


#' Transform to 0-1 scale
#'
#' @param x Numeric factor
#'
#' @return Numeric factor on scale 0-1
#' @export
#'
#' @examples
#' transform_01(1:5)
#' transform_01(c(1:3, NA, 4:5))
transform_01 = function(x) {
  assertthat::assert_that(!is.character(x))
  assertthat::assert_that(!is.factor(x))

  #0 length
  if (length(x) == 0) return(c())

  #set to 0-1 scale
  #subtract minimum
  x = x - min(x, na.rm = T)

  #divide by maximum
  x = x / max(x, na.rm = T)

  x
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


#' Calculate row-wise representativeness
#'
#' @param x Data frame of numerical data
#' @param central_tendency_fun Function to use for central tendency
#' @param central_tendencies If custom values for central tendency, input here
#' @param standardize Whether to standardize values to ensure common metric. If not done, variables with larger SDs will be given more influence.
#' @param absolute Whether to use absolute errors (if not, then there is also an attempt to ensure the error directions are balanced across variables)
#'
#' @return A data frame of error values and their mean and medians
#' @export
#'
#' @examples
#' calc_row_representativeness(iris)
calc_row_representativeness = function(x, central_tendency_fun = median, central_tendencies = NULL, standardize = T, absolute = T) {

  #skip non-numeric variables
  is_num = map_lgl(x, is.numeric)
  if (any(!is_num)) message("Skipped non-numeric variables")
  x = x[is_num]

  #standardize if desired
  if (standardize) x = map_df(x, kirkegaard::standardize)

  #compute centencies
  if (is.null(central_tendencies)) {
    #median
    if (identical(central_tendency_fun, median)) {
      central_tendencies = map_dbl(x, median, na.rm = T)
    } else if (identical(central_tendency_fun, mean)) {
      #mean
      central_tendencies = map_dbl(x, mean, na.rm = T)
    } else {
      #something else
      central_tendencies = map_dbl(x, mean, na.rm = T)
    }
  } else {
    #ensure matching lengths
    assert_that(ncol(x) == length(central_tendencies))
  }

  #subtract central tendencies
  if (!absolute) {
    x_error = t(t(x) - central_tendencies) %>% as.data.frame()
  } else {
    x_error = abs(t(t(x) - central_tendencies)) %>% as.data.frame()
  }

  #add row means and medians, dplyr way
  #https://dplyr.tidyverse.org/articles/rowwise.html#per-row-summary-statistics-1
  x_error %>%
    rowwise() %>%
    mutate(
      mean = mean(c_across(everything()), na.rm = T),
      median = median(c_across(everything()), na.rm = T)
    ) %>% ungroup() %>%
    mutate(row = 1:n()) %>%
    select(row, everything())
}

#internal helper functions
#get model R2 adj
get_r2adj = function(x) {
  x %>% summary.lm() %>% .$adj.r.squared
}

#boomer code
#https://rdrr.io/cran/rms/src/R/ols.s#sym-print.ols
get_p = function(x) {
  1 - pchisq(x$stats['Model L.R.'], x$stats['d.f.'])
}

#' Test for heteroscedasticity
#'
#' @param resid Model residuals
#' @param x Model predictor of interest
#'
#' @return Data frame of results
#' @export
#'
#' @examples
#' #look for mostly nonexistent HS in iris
#' test_HS(resid = resid(lm(Sepal.Length ~ Petal.Length, data = iris2)), x = iris2$Petal.Length)
#' #a lot of HS here
#' test_HS(resid = resid(lm(Petal.Width ~ Petal.Length, data = iris2)), x = iris2$Petal.Length)
test_HS = function(resid, x) {
  #data
  d = tibble(
    resid = standardize(abs(resid)),
    x = standardize(x),
    resid_rank = rank(resid),
    x_rank = rank(x)
  )

  #fits
  mods = list(
    fit_linear = rms::ols(resid ~ x, data = d),
    fit_spline = rms::ols(resid ~ rms::rcs(x), data = d),
    fit_linear_rank = rms::ols(resid_rank ~ x_rank, data = d),
    fit_spline_rank = rms::ols(resid_rank ~ rms::rcs(x_rank), data = d)
  )

  #summarize
  y = tibble(
    test = c("linear raw", "spline raw", "linear rank", "spline rank"),
    r2adj = purrr::map_dbl(mods, get_r2adj),
    p = purrr::map_dbl(mods, get_p),
    fit = mods
  )

  #for the rcs, we have to compute the model improvement vs. linear
  #https://stackoverflow.com/questions/47937065/how-can-i-interpret-the-p-value-of-nonlinear-term-under-rms-anova
  #https://stats.stackexchange.com/questions/405961/is-there-formal-test-of-non-linearity-in-linear-regression
  y$p[2] = rms::lrtest(mods$fit_linear, mods$fit_spline)$stats["P"]
  y$p[4] = rms::lrtest(mods$fit_linear_rank, mods$fit_spline_rank)$stats["P"]

  y %>% dplyr::mutate(log10_p = -log10(p))
}


#' Quantile smoothening
#'
#' @param x Predictor variable
#' @param y Outcome variable
#' @param quantile Which quantile (e.g., .95)
#' @param method Method to use
#' @param k Number of knots for method qgam, see [qgam::qgam]
#' @param window Window size for method running, see [caTools::runquantile]
#'
#' @return Vector of fitted values
#' @export
#'
#' @examples
#' quantile_smooth(iris$Petal.Length, iris$Sepal.Length, quantile = .90)
quantile_smooth = function(x, y, quantile, method = c("qgam", "Rq", "running"), k = 5, window = 50) {

  #method
  method = match.arg(method[1], method)
  if (method == "qgam") {
    #same as in example here
    #https://rdrr.io/cran/qgam/man/qgam.html
    assertthat::assert_that(nchar(system.file(package = "qgam")) > 0)
    fit = qgam::qgam(list(y~s(x, k = k, bs = "cr"), ~ s(x, k = k, bs = "cr")),
                     data = data.frame(x = x, y = y), qu = quantile)
    return(predict(fit))

  } else if (method == "Rq") {
    #fit with spline
    #https://rdrr.io/cran/rms/man/Rq.html
    assertthat::assert_that(nchar(system.file(package = "rms")) > 0)
    fit = rms::Rq(y ~ rms::rcs(x), data = data.frame(x = x, y = y), tau = quantile)
    return(fitted(fit) %>% as.vector())

  } else if (method == "running") {
    #beware, need to sort the y by x first
    #and then return it to original state
    assertthat::assert_that(nchar(system.file(package = "caTools")) > 0)
    x_order = order(x)
    return(caTools::runquantile(y[x_order], k = k, probs = quantile)[x_order])

  } else {
    stop(str_glue("method not recognized: {method}"), call. = F)
  }

}
