## VARIOUS MODELING FUNCTIONS

#' Matrix of beta coefficients of all simple linear models.
#'
#' Returns a data.frame with beta coefficients for all possible simple (without interactions) linear models given a set of predictor variables and a dependent variable.
#' @param dependent (character scalar) The name of the dependent variable.
#' @param predictors (character vector) The names of the redictor variables.
#' @param data (data.frame) A data.frame with the variables.
#' @param standardized (boolean) Whether to standardize the results. Defaults to true.
#' @param .weights (numeric vector) A numeric vector of weights to use. Defaults to NA, which causes it to use unit weights for all cases.
#' @param return_models (character scalar) What to return. all = all models, best = best model. Defaults to best.
#' @param messages (boolean) Whether to show messages. Default=TRUE.
#' @export
#' @examples
#' #try all models in iris dataset to predict sepal length
#' lm_beta_matrix(dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"), data = iris)
lm_beta_matrix = function(dependent, predictors, data, standardized = T, .weights = NA, return_models = "b", messages = T) {
  library(gtools) #for combinations()
  library(stringr) #for str_c()
  library(faraway) #for vif()

  #find all the combinations
  num.inde = length(predictors) #how many indeps?
  num.cases = nrow(data) #how many cases?
  model_fit_names = c("AIC", "BIC", "r2", "r2.adj.", "N", "VIF")

  #standardize?
  if (standardized == T) {
    data = std_df(data, messages = messages)
  }

  sets = list() #list of all combinations
  for (num.choose in 1:num.inde) { #loop over numbers of variables to choose
    temp.sets = combinations(num.inde, num.choose) #all combinations of picking r out of n
    temp.sets = split(temp.sets, seq.int(nrow(temp.sets))) #as a list
    sets = c(sets, temp.sets)
  }


  #create all the models
  models = numeric() #empty vector for models
  for (set in sets) { #loop over each possible combination
    deps = predictors[set] #fetch predictor names
    deps = paste0(deps, collapse = " + ") #add " + " between predictors
    model = paste0(dependent, " ~ ", deps) #join up
    models = c(models,model) #add to vector
  }
  #print(models) #debug

  #fix weights
  if (length(.weights) == 1) {
    if (is.na(.weights)) { #if it is NA, insert weights = 1 in all cases (same as unweighted)
      .weights = rep(1, num.cases)
    }
  } else if (length(.weights) != num.cases) { #if number of weights is mismatched to cases
    stop("Number of .weights is not the same as number of cases!")
  }

  data$.weights = .weights

  #run each model
  betas = data.frame(matrix(ncol = length(model_fit_names), nrow = length(models))) #DF for betas
  model.fits = list()
  #number of cols is the predictors +1 because the last is R2 adj.
  colnames(betas) = model_fit_names #colnames
  for (model.idx in 1:length(models)) { #loop over the index of each model
    #progress
    if (messages) message(str_c("Model ", model.idx, " of ", length(models)))

    #fit model and extract betas
    lm.fit = lm(models[model.idx], data, weights = .weights) #fit the model
    model.fits[[model.idx]] = lm.fit

    #get betas, remove intercept
    lm.fit.betas = lm.fit$coefficients[-1]

    #insert data
    for (beta.idx in 1:length(lm.fit.betas)) { #loop over each beta
      beta.names = as.vector(names(lm.fit.betas)) #get the names
      beta.name = beta.names[beta.idx] #get the name
      betas[model.idx,beta.name] = lm.fit.betas[beta.idx] #insert beta in the right place
    }

    #insert AIC
    aic_ = AIC(lm.fit)
    betas[model.idx, "AIC"] = aic_

    #insert AIC
    bic_ = BIC(lm.fit)
    betas[model.idx, "BIC"] = bic_

    #insert r2
    r_sq = summary(lm.fit)$r.squared
    betas[model.idx, "r2"] = r_sq

    #insert r2 adj.
    r_sq_adj = summary(lm.fit)$adj.r.squared
    betas[model.idx, "r2.adj."] = r_sq_adj

    #sample N
    betas[model.idx, "N"] = nrow(model.frame(lm.fit))

    #VIF
    betas[model.idx, "VIF"] = max(faraway::vif(lm.fit))
    # fmsb::VIF(lm.fit)
    # car::vif(lm.fit)
    # HH::vif(lm.fit)
    # faraway::vif(lm.fit)

  }

  #rearrange colnames
  predictor_names = setdiff(colnames(betas), model_fit_names)
  betas = betas[c(predictor_names, model_fit_names)]

  #return models
  #all models
  if (str_sub(return_models, 1, 1) == "a") {
    return(list(beta_matrix = betas,
                all_models = model.fits))
  }

  #best model
  if (str_sub(return_models, 1, 1) == "b") {
    best_idx = which.max(unlist(betas["r2.adj."]))
    return(list(beta_matrix = betas,
                best_model = model.fits[[best_idx]]))
  }

  return(betas)
}


#' Convenient summary of an lm() model with analytic confidence intervals.
#'
#' Returns beta coefficients and confidence intervals from a fitted lm() model.
#' @param fitted_model (a lm or similar object) The fitted model.
#' @param level (num scalar) The level of confidence to use. Defaults to .95 (95\%).
#' @param round (num scalar) At which digit to round the numbers. Defaults to 2.
#' @param standardize (log scalar) Whether to report standardized betas (default true).
#' @export
#' @examples
#' #fit two models with iris data, one with normal and one with standardized data
#' fit1 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris)
#' fit2 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris %>% std_df())
#' #then summarize the two models
#' lm_CI(fit1, standardize = F) #unstd. data, don't std. betas
#' lm_CI(fit1, standardize = T) #unstd. data, then std. betas
#' lm_CI(fit2, standardize = F) #std data., don't std. betas
lm_CI = function(fitted_model, level = .95, round = 2, standardize = T) {
  library(magrittr)

  #summary
  sum.model = summary(fitted_model)
  # browser()
  #degrees of freedom
  df = sum.model$df[2]

  #R2 values
  model_effect_size = c(sum.model$r.squared, sum.model$adj.r.squared) %>% round(round)
  names(model_effect_size) = c("R2", "R2 adj.")

  #coefs
  coefs = sum.model$coef[-1,1:2, drop=F] #coefs without intercept
  coefs = as.data.frame(coefs) #conver to dataframe
  colnames(coefs) = c("Beta", "SE") #rename

  #standardize?
  if (standardize) {
    #calculate sds
    sds = sapply(fitted_model$model, sd, na.rm = T)

    #calculate the factors
    factors = sds[-1] / sds[1]

    #calculate std. betas
    coefs = coefs * factors
  }


  #calculate CIs
  multiplier = qt(1-((1-level)/2),df) #to calculate the CIs
  coefs$CI.lower = coefs[,1] - multiplier*coefs[,2] #lower
  coefs$CI.upper = coefs[,1] + multiplier*coefs[,2] #upper
  coefs = round(coefs, round) #round to desired digit

  #return
  return(list(coefs = coefs,
              effect_size = model_effect_size))
}


#' Get R2 and R2 adj. for each model.
#'
#' Returns a data.frame with each models R2 and R2 adj.
#' @param model_list (list) A list of model fits e.g. from lm().
#' @keywords model, fit
#' @export
#' @examples
#' lm_get_fits()
lm_get_fits = function(model_list) {
  d = as.data.frame(matrix(nrow = length(model_list), ncol = 2))
  colnames(d) = c("R2", "R2_adj")

  for (idx in 1:length(model_list)) {
    mod = model_list[[idx]]
    s = summary(mod)
    d[idx, "R2"] = s$r.squared
    d[idx, "R2_adj"] = s$adj.r.squared
  }

  return(d)
}



#' Find the best model by R2 adj. value.
#'
#' Returns the index of the model with the highest R2 adj. value.
#' @param model_list A list of model fits e.g. from lm().
#' @keywords model, fit, R2 adj., best
#' @export
#' @examples
#' lm_best()
lm_best = function(model_list) {
  fit_df = lm_get_fits(model_list)[2]
  return(which_max2(fit_df)[1])
}


#' Repeatedly run glmnet.cv()
#'
#' Returns a data frame of beta coefficients from glmnet.cv() fits.
#' @param data (data.frame) A data.frame with the data. Must contain dependent and predictor variables.
#' @param dependent (character scalar) The name of the dependent variable.
#' @param predictors (character vector) The names of the predictor variables.
#' @param weights_ (numeric vector) If weights should be used, a numeric vector of values to use. Defaults to equal weights.
#' @param standardize (boolean) Whether to standardize the data beforehand. Defaults to true.
#' @param runs (numeric/integer scalar) Number of times to run. Defaults to 100.
#' @param alpha_ (numeric scalar) The penalty to use. 1 = lasso regression, 0 = ridge regression. Defaults to 1.
#' @param NA_ignore (boolean) Whether to remove cases with missing data. Defaults to T.
#' @param messages (boolean) Whether to send messages to the user.
#' @keywords model, fit, cross-validation, glmnet, repeat
#' @export
#' @examples
#' MOD_repeat_cv_glmnet()
MOD_repeat_cv_glmnet = function(df, dependent, predictors, weights_ = NA, standardize = T, runs = 100, alpha_ = 1, NA_ignore = T, messages = T) {
  #load lib
  library(glmnet)
  library(stringr)

  #weights
  if (length(weights_) == 1) {
    if (is.na(weights_)) {
      df$weights_ = rep(1, nrow(df))
    }
  } else {
    if (length(weights_) != nrow(df)) stop("Length of weights does not match df!")
    df$weights_ = weights_
  }

  #subset
  df = df[c(dependent, predictors, "weights_")]

  #missing data
  if (NA_ignore) {
    if (any(is.na(df))) {
      df = df %>% na.omit()
      message("Missing data removed.")
    }
  }

  #standardize
  df = df
  if (standardize) {
    df = std_df(df, exclude = "weights_", messages = messages)
    message("Data standardized.")
  }

  #fit lasso

  #save object
  results_df = data.frame(matrix(nrow = runs, ncol = 0))

  #loop
  for (run in 1:runs) {
    if (messages) message(str_c("Run ", run, " of ", runs))

    #fit lasso
    fit_cv = cv.glmnet(x = as_num_matrix(df[predictors]), #predictor vars matrix
                       y = as_num_matrix(df[dependent]), #dep var matrix
                       weights = df$weights_, #weights
                       alpha = alpha_) #type of shrinkage

    #fetch coefs and names
    pred_betas = coef(fit_cv) #coefs
    pred_names = dimnames(pred_betas)[[1]] #names

    #save coefs
    for (beta_idx in seq_along(pred_names)) { #for each coef
      results_df[run, pred_names[beta_idx]] = pred_betas[beta_idx] #save it
    }
  }

  return(results_df)
}


#' Summarize model coefficients
#'
#' Calculates summary statistics for model betas.
#' @param df A data frame with betas for predictors across models.
#' @param digits The number of digits to round the results to. Defaults to 3.
#' @param desc A character vector of the desired descriptive statistics. These are extracted using describe() from the psych package. Defaults to c("mean", "median", "sd").
#' @keywords model, fit, summary, describe
#' @export
#' @examples
#' MOD_summarize_models()
MOD_summarize_models = function(df, digits = 3, desc = c("mean", "median", "sd", "mad", "fraction_zeroNA"), include_intercept = F) {
  #libs
  library(psych)

  #zero coefs
  df_zero = df == 0
  df_zero = apply(df_zero, 2, sum, na.rm = T) / nrow(df)

  #NA coefs
  df_NA = is.na(df)
  df_NA = apply(df_NA, 2, sum, na.rm = T) / nrow(df)

  #zero or NA coefs
  df_zeroNA = df_NA + df_zero #composite

  #desc stats
  df_desc = as.data.frame(psych::describe(df)) #default stats
  df_desc$fraction_zeroNA = df_zeroNA #zero or NA
  df_desc$fraction_zero = df_zero #zero
  df_desc$fraction_NA = df_NA #NA
  df_desc = as.data.frame(t(df_desc[desc])) #subset the desc stats desired

  #rounding
  df_desc = round(df_desc, digits)

  #include intercept
  if (!include_intercept) df_desc$`(Intercept)` = NULL

  return(df_desc)
}
