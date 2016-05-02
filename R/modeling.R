## VARIOUS MODELING FUNCTIONS

#' Fit all possible simple linear models
#'
#' Fits all possible simple (no interactions) linear models using \code{\link{lm}}.
#'
#' Use \code{\link{MOD_summarize_models}} on the data.frame with model summary information to get an overview of the results.
#' @return Returns a list with two elements: 1) A data.frame with betas and other summary statistics from the models. 2) A list with all the models.
#' @param dependent (chr scalar) The name of the dependent variable.
#' @param predictors (chr vector) The names of the redictor variables.
#' @param data (data.frame) A data.frame with the variables.
#' @param standardized (log scalar) Whether to standardize the results. Defaults to true.
#' @param .weights (num vector) A numeric vector of weights to use. Defaults to NA, which causes it to use unit weights for all cases.
#' @param messages (log scalar) Whether to show messages. Default=TRUE.
#' @export MOD_APSLM lm_beta_matrix
#' @aliases lm_beta_matrix
#' @examples
#' #try all models in iris dataset to predict sepal length
#' MOD_APSLM(dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"), data = iris)
MOD_APSLM = function(dependent, predictors, data, standardized = T, .weights = NA, messages = T) {
  library(gtools) #for combinations()
  library(stringr) #for str_c()

  #find all the combinations
  num.inde = length(predictors) #how many indeps?
  num.cases = nrow(data) #how many cases?
  model_fit_names = c("AIC", "BIC", "r2", "r2.adj.", "R", "R.adj.", "N")

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

    #insert R
    betas[model.idx, "R"] = sqrt(r_sq)

    #insert R adj.
    betas[model.idx, "R.adj."] = sqrt(r_sq_adj)

    #sample N
    betas[model.idx, "N"] = nrow(model.frame(lm.fit))

  }

  #rearrange colnames
  predictor_names = setdiff(colnames(betas), model_fit_names)
  betas = betas[c(predictor_names, model_fit_names)]

  #return
  return(list(beta_matrix = betas,
              all_models = model.fits))
}

#old name
lm_beta_matrix = MOD_APSLM


#' Convenient summary of an lm() model with analytic confidence intervals.
#'
#' Returns beta coefficients and confidence intervals from a fitted lm() model.
#' @param fitted_model (a lm or similar object) The fitted model.
#' @param level (num scalar) The level of confidence to use. Defaults to .95 (95\%).
#' @param round (num scalar) At which digit to round the numbers. Defaults to 2.
#' @param standardize (log scalar) Whether to report standardized betas (default true).
#' @param kfold (log scalar) Whether to also calculate a k fold cross-validated r2 value (default true).
#' @param folds (num scalar) The number of folds to use if using cross-validation.
#' @param ... (other args) Other arguments passed to \code{\link{MOD_k_fold_r2}}.
#' @export MOD_summary lm_CI
#' @aliases lm_CI
#' @examples
#' #fit two models with iris data, one with normal and one with standardized data
#' fit1 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris)
#' fit2 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris %>% std_df())
#' #then summarize the two models
#' MOD_summary(fit1, standardize = F) #unstd. data, don't std. betas
#' MOD_summary(fit1, standardize = T) #unstd. data, then std. betas
#' MOD_summary(fit2, standardize = F) #std data., don't std. betas
#' MOD_summary(fit1, standardize = T, kfold = F) #unstd. data, then std. betas, no cv
MOD_summary = function(fitted_model, level = .95, round = 2, standardize = T, kfold = T, folds = 10, ...) {
  library(magrittr)

  #higher order?
  if (!all(attr(fitted_model$terms, "order") == 1)) {
    stop("Higher order (interaction) variables are not supported at this time! As an alternative, standardize the data before running the model to get standardized values.")
  }

  #summary
  sum.model = summary(fitted_model)

  #degrees of freedom
  df = sum.model$df[2]

  #R2 values
  model_meta = c(nrow(fitted_model$model), sum.model$r.squared, sum.model$adj.r.squared)
  names(model_meta) = c("N", "R2", "R2 adj.")

  #cross validate?
  if (kfold) {
    model_meta = c(model_meta, MOD_k_fold_r2(fitted_model, folds = folds, ...)[2])
    names(model_meta) = c("N", "R2", "R2 adj.", "R2 " + folds + "-fold cv")
  }

  #rounding, dont round the first value
  model_meta = round(model_meta, round)

  #coefs
  coefs = sum.model$coef[-1,1:2, drop = F] #coefs without intercept
  coefs = as.data.frame(coefs) #conver to dataframe
  colnames(coefs) = c("Beta", "SE") #rename

  #fix predictor names
  v_newnames = character()
  for (i in seq_along(fitted_model$model[-1])) {
    #is character?
    if (is.character(fitted_model$model[-1][[i]])) {
      #change to factor
      fitted_model$model[-1][[i]] = as.factor(fitted_model$model[-1][[i]])
      warning("One variable " + names(fitted_model$model[-1])[i] + " was converted from chr to factor. You should probably convert this to a factor in your data.frame.")
    }

    #is factor?
    if (is.factor(fitted_model$model[-1][[i]])) {
      #add the non-first levels, with the variable name in front
      v_newnames = c(v_newnames, colnames(fitted_model$model[-1])[i] + ": " + levels(fitted_model$model[-1][[i]])[-1])
      #next
      next
    }
    #is weight? then skip
    if (colnames(fitted_model$model[-1])[i] == "(weights)") {
      next
    }

    #add regular name
    v_newnames = c(v_newnames, colnames(fitted_model$model[-1])[i])
  }
  #set the new names
  rownames(coefs) = v_newnames

  #standardize?
  if (standardize) {
    #calculate sds
    sds = sapply(fitted_model$assign+1, function(i) {

      #data
      var = fitted_model$model[[i]]

      #if var is a factor, sd = 1
      if (is.factor(var)) return(1)
      #otherwise, calculate it
      sd(var, na.rm = T)
    })[1:(nrow(coefs) + 1)]
    #we subset to avoid weights in they are in the model

    #calculate the factors
    factors = sds[-1] / sds[1]

    #calculate std. betas
    coefs = coefs * factors
  }


  #calculate CIs
  multiplier = qt(1-((1-level)/2), df) #to calculate the CIs
  coefs$CI.lower = coefs[, 1] - multiplier*coefs[, 2] #lower
  coefs$CI.upper = coefs[, 1] + multiplier*coefs[, 2] #upper
  coefs = round(coefs, round) #round to desired digit

  #return
  return(list(coefs = coefs,
              meta = model_meta))
}

#old name
lm_CI = MOD_summary


#' Get R2 and R2 adj. for each model.
#'
#' Returns a data.frame with each models R2 and R2 adj.
#' @param model_list (list) A list of model fits e.g. from lm().
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
#' @export
#' @examples
#' MOD_repeat_cv_glmnet(iris, "Sepal.Length", predictors = colnames(iris)[-1])
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
#' @param include_intercept (log scalar) Whether to include estimation of the intercept (default false).
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


# from
# http://stackoverflow.com/a/16030020/3980197
# via http://www.statmethods.net/stats/regression.html

#' Calculate k fold cross validated r2
#'
#' Using k fold cross-validation, estimate the true r2 in a new sample. This is better than using adjusted r2 values.
#' @param lmfit (lm fit) A lm fit object.
#' @param folds (whole number scalar) The number of folds to use (default 10).
#' @param runs (int scalar) The number of runs (default 20).
#' @param seed (int scalar) The seed for the random number generator (default 1). This ensures reproducible results.
#' @export
#' @examples
#' fit = lm("Petal.Length ~ Sepal.Length", data = iris) #a fit
#' fit_wtd = lm("Petal.Length ~ Sepal.Length", data = iris, weight = Sepal.Width) #with weights
#' MOD_k_fold_r2(fit) #raw r2 and cv r2
#' MOD_k_fold_r2(fit_wtd) #different r2 due to weights
MOD_k_fold_r2 = function(lmfit, folds = 10, runs = 20, seed = 1) {
  library(magrittr)

  #get data
  data = lmfit$model

  #seed
  if (!is.na(seed)) set.seed(seed)

  v_runs = sapply(1:runs, FUN = function(run) {
    #Randomly shuffle the data
    data2 = data[sample(nrow(data)), ]

    #weights
    if ("(weights)" %in% colnames(data2)) {
      data2$.weights = data2[["(weights)"]]
    } else {
      data2$.weights = rep(1, nrow(data2))
    }

    #Create n equally size folds
    folds_idx <- cut(seq(1, nrow(data2)), breaks = folds, labels = FALSE)

    #Perform n fold cross validation
    sapply(1:folds, function(i) {
      #Segement your data by fold using the which() function

      test_idx = which(folds_idx == i, arr.ind = TRUE)
      test_data = data2[test_idx, ]
      train_data = data2[-test_idx, ]

      #fit
      form = formula(lmfit)
      fit = lm(formula = form, data = train_data,
               weights = .weights)

      #predict
      preds = predict(fit, newdata = test_data)

      #calculate SSE and R2
      # http://stats.stackexchange.com/questions/32596/what-is-the-difference-between-coefficient-of-determination-and-mean-squared
      v_sq_errors = (test_data[[1]] - preds)^2 #squared errors
      v_sq_errors_sum = wtd_sum(v_sq_errors, test_data$.weights) #weighted sum
      v_squared_deviations = (test_data[[1]] - wtd_mean(train_data[[1]], train_data$.weights))^2 #squared deviations from mean
      v_sum_squared_deviations = wtd_sum(v_squared_deviations, test_data$.weights)

      #R2
      1 - (v_sq_errors_sum / v_sum_squared_deviations)
    }) %>%
      mean()
  })

  #return
  c("raw_r2" = summary(lmfit)$r.squared, "cv_r2" = mean(v_runs))
}


