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
#' @param messages (log scalar) Whether to show messages with the models. Default no.
#' @param progress (lgl scalar) Whether to show a progress bar. Default yes.
#' @param cv_runs (num scalar) How many runs to use in cross-validation. Default 20. Increase this value for smaller datasets to get more reliable results.
#' @export
#' @examples
#' #try all models in iris dataset to predict sepal length
#' MOD_APSLM(dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"), data = iris)
MOD_APSLM = function(dependent, predictors, data, standardized = T, .weights = NA, messages = F, progress = T, cv_runs = 20) {

  #find all the combinations
  num.inde = length(predictors) #how many indeps?
  num.cases = nrow(data) #how many cases?
  model_fit_names = c("AIC", "BIC", "r2", "r2.adj.", "r2_cv", "R", "R.adj.", "N")

  #standardize?
  if (standardized == T) {
    data = df_standardize(data[c(dependent, predictors)], messages = messages)
  }

  sets = list() #list of all combinations
  for (num.choose in 1:num.inde) { #loop over numbers of variables to choose
    temp.sets = gtools::combinations(num.inde, num.choose) #all combinations of picking r out of n
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

  #progress bar
  if (progress) pb <- txtProgressBar(min = 1, max = length(models), initial = 1, style = 3)
  for (model.idx in seq_along(models)) { #loop over the index of each model
    #progress
    if (progress) setTxtProgressBar(pb, value = model.idx)
    if (messages) message(stringr::str_c("Model ", model.idx, " of ", length(models)))

    #fit model and extract betas
    lm.fit = lm(models[model.idx], data, weights = .weights) #fit the model
    model.fits[[model.idx]] = lm.fit

    #cross validated r2
    v_cv_r2 = MOD_k_fold_r2(lm.fit, runs = cv_runs, progress = F)

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

    #insert r2_cv
    betas[model.idx, "r2_cv"] = v_cv_r2[2]

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

#Function to make a nicely formatted coefficient data frame
make_nicer_coefs = function(fitted_model, coefs = NULL, model_data = NULL) {

  #check for model errors
  fail_if_NA(fitted_model$coef, msg = "The model parameters contained missing values. This is usually because of perfect linear dependency between predictors.")

  #summarize the model
  model_sum = summary(fitted_model)

  #get model data from fit unless given a set
  if (is.null(model_data)) {
    model_data = fitted_model$model#get predictors
  }

  #get predictors from model data
  predictor_data = model_data[-1] %>% df_subset_by_pattern(pattern = "(weights)", inverse = T)

  #get coefs from fit unless given a set
  if (is.null(coefs)) {
    coefs = model_sum$coef[-1,1:2, drop = F] #coefs without intercept
    coefs = as.data.frame(coefs) #conver to dataframe
    colnames(coefs) = c("Beta", "SE") #rename cols
  }

  #rename predictors if they are logicals
  lgl_preds = fitted_model$model %>%
    #exclude y
    `[`(-1) %>%
    #check logical
    map_lgl(inherits, what = "logical") %>%
    #index
    which

  #loop over the predictors
  for (pred_i in lgl_preds) {
    rownames(coefs)[pred_i] %<>% stringr::str_sub(start = 1, end = -5)
  }

  #insert reference levels

  #the assign vector
  v_assign = fitted_model$assign + 1

  #first we have to find where they are
  v_duplicate = duplicated(v_assign[-1]) #we create this to find all the non-first betas from a factor
  #otherwise, we would insert a new row above every factor level

  v_factorsbegin = numeric() #the rows where the new factors begin
  for (row in seq_along(v_assign[-1])) {
    i = v_assign[-1][row] #the value

    if (!is.factor(model_data[[i]])) next

    #check if already covered
    if (v_duplicate[row]) next #if so, then skip

    #then save the position
    v_factorsbegin = c(v_factorsbegin, row)
  }



  #add reference levels
  #are there any?
  if (length(v_factorsbegin) != 0) {
    row = 1
    while (T) {
      #insert ref?
      if (row %in% v_factorsbegin) {
        #add the ref row
        coefs = rbind(coefs[0:(row-1), ], #the rows above
                      c(0, NA, NA, NA), #ref row
                      coefs[(row):nrow(coefs), ]) #the rows below

        #remove the current from the vector
        v_factorsbegin = v_factorsbegin[-1]

        #no more? then we are done
        if (length(v_factorsbegin) == 0) break

        #increment insert rows by 1 because we added another row
        v_factorsbegin = v_factorsbegin + 1
      }

      #iterater + 1
      row = row + 1

      #stop if reached the last row
      if (row > nrow(coefs)) stop("Bug in the code!")
    }

    #set the rownames
    v_rownames = sapply(seq_along(predictor_data), function(i) {
      v_varname = colnames(predictor_data)[i]
      #factor?
      if (is.factor(predictor_data[[i]])) {
        return(v_varname + ": " + levels(predictor_data[[i]]))
      } else {
        return(v_varname)
      }
    }) %>% unlist()

    rownames(coefs) = v_rownames
  }

  coefs
}



#' Get etas from analysis of variance
#'
#' Converts a \code{lm} or \code{glm} to an analysis of variance, and calculates the etas (square rooted values of traditional eta^2).
#'
#' Is actually a wrapper for \code{\link{lsr::etaSquared}}, but returns the etas instead of eta squared, and returns a data frame not a matrix.
#' @param fitted_model (model) A model.
#' @return A data frame. Eta is the square root of the eta^2 metric (equivalent to r^2), eta_partial is the square root of the partial eta^2.
#' @export
#' @examples
#' lm(Sepal.Length ~ Species + Petal.Width, data = iris) %>% MOD_etas
MOD_etas = function(fitted_model) {
  fitted_model %>%
    aov %>%
    lsr::etaSquared() ->
    etassq

  #sqrt
  etas = etassq %>% sqrt

  #rename
  colnames(etas) = c("Eta", "Eta_partial")

  as.data.frame(etas)
}


#' Custom printing function for model_summary class
#' @export
print.model_summary  = function(x) {

  cat("\n    ---- Model summary ----    \nModel coefficients\n")
  print(x$coefs)

  cat("\n\nModel meta-data\n")
  print(x$meta)

  cat("\n\nEtas from analysis of variance\n")
  print(x$aov_etas)

  invisible(NULL)
}


#' Convenient summary of a \code{\link{lm}} or \code{\link{glm}} with analytic confidence intervals.
#'
#' Tidys information from linear models or generalized linear models.
#' @param fitted_model (lm or glm) The fitted model.
#' @param level (num scalar) The level of confidence to use.
#' @param round (num scalar) At which digit to round the numbers.
#' @param standardize (log scalar) Whether to report standardized betas.
#' @param kfold (log scalar) Whether to also calculate a k fold cross-validated r2 value.
#' @param folds (num scalar) The number of folds to use if using cross-validation.
#' @param runs (int scalar) The number of runs to use for cross-validation.
#' @param ... (other args) Other arguments passed to \code{\link{MOD_k_fold_r2}}.
#' @export
#' @examples
#' fit1 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris)
#' #then summarize the model
#' MOD_summary(fit1) #unstd. data, std. betas
#' MOD_summary(fit1, standardize = F) #unstd. data, don't std. betas, similar to base \code{summary}
MOD_summary = function(fitted_model, level = .95, standardize = T, kfold = T, folds = 10, runs = 20, ...) {
  #check for model errors
  fail_if_NA(fitted_model$coef, msg = "The model parameters contained missing values. This is usually because of perfect linear dependency between predictors.")

  #init
  return_list = list(coefs = NULL,
                     meta = NULL,
                     model_obj = NULL,
                     aov_etas = NULL
                     )

  #fetch data
  model_data = fitted_model$model

  #convert factors
  model_data[] = lapply(model_data, FUN = function(x) {
    if (is.character(x)) {
      message("The model data contains characters. These were automatically converteed but you should probably do this before calling this function.")
      return(as.factor(x))
    }
    x
  })

  #get predictors
  predictor_data = model_data[-1] %>% df_subset_by_pattern(pattern = "(weights)", inverse = T)

  #summary
  model_sum = summary(fitted_model)

  #degrees of freedom
  df = model_sum$df[2]

  #init coefs
  coefs = model_sum$coefficients[-1, 1:2, drop = F] %>% as.data.frame
  colnames(coefs) = c("Beta", "SE") #rename cols
  p_vals = model_sum$coefficients[, 4]


  if (all(class(fitted_model) == "lm")) {
    #higher order?
    if (!all(attr(fitted_model$terms, "order") == 1)) {
      stop("Higher order (interaction) variables are not supported at this time! As an alternative, standardize the data before running the model to get standardized values.")
    }


    #the assign vector
    v_assign = fitted_model$assign + 1

    #model meta
    model_meta = data.frame("outcome" = fitted_model$model %>% names %>% `[`(1),
                            "N" = nrow(model_data),
                            "df" = df,
                            "R2" = model_sum$r.squared,
                            "R2-adj." = model_sum$adj.r.squared,
                            "R2-cv" = NA,
                            check.names = F
                            )

    #cross validate?
    if (kfold) {
      model_meta$`R2-cv` = MOD_k_fold_r2(fitted_model, folds = folds, runs = runs, ...)[2]
    }

    #standardize?
    if (standardize) {
      #calculate sds
      sds = sapply(v_assign, function(i) {

        #data
        var = model_data[[i]]

        #if var is a factor, sd = 1
        if (is.factor(var) || is.logical(var)) return(1)
        #otherwise, calculate it
        sd(var)
      })[1:(nrow(coefs) + 1)]

      #calculate the factors
      factors = sds[-1] / sds[1]

      #calculate std. betas
      coefs = coefs * factors
    }

    #coefs
    coefs = make_nicer_coefs(fitted_model = fitted_model, coefs = coefs, model_data = model_data)

    #calculate CIs
    multiplier = qt(1-((1-level)/2), df) #to calculate the CIs
    coefs$CI_lower = coefs[, 1] - multiplier*coefs[, 2] #lower
    coefs$CI_upper = coefs[, 1] + multiplier*coefs[, 2] #upper

  }


  if ("glm" %in% class(fitted_model)) {

    #higher order?
    if (!all(attr(fitted_model$terms, "order") == 1)) {
      stop("Higher order (interaction) variables are not supported at this time! As an alternative, standardize the data before running the model to get standardized values.")
    }

    #values
    pseudo_r2 = 1 - (model_sum$deviance / model_sum$null.deviance)

    #meta values
    model_meta = data.frame("outcome" = fitted_model$model %>% names %>% `[`(1),
                            "N" = nrow(model_data),
                            "df" = df,
                            "pseudo-R2" = pseudo_r2,
                            "deviance" = model_sum$deviance,
                            "pseudo-R2-cv" = NA,
                            check.names = F
                            )

    #cross validate?
    #TODO

    #since the assign vector isn't given, we create one
    v_assign = sapply(seq_along(model_data), function(i) {
      #if it is numeric, then it has 1 level
      if (is.null(levels(model_data[[i]]))) return(i)

      #if it has levels, then return that number repeated minus 1 (ref level)
      rep(i, length(levels(model_data[[i]])) - 1)
    }) %>% unlist()

    #standardize?
    if (standardize) {

      #calculate sds
      sds = sapply(v_assign, function(i) {

        #data
        var = model_data[[i]]

        #if var is a factor, sd = 1
        if (is.factor(var) || is.logical(var)) return(1)

        #otherwise, calculate it
        sd(var)
      })[1:(nrow(coefs) + 1)]
      #we subset to avoid weights in they are in the model

      #calculate the factors
      factors = sds[-1] / sds[1]

      #calculate std. betas
      coefs = coefs * factors
    }

    #coefs
    coefs = make_nicer_coefs(fitted_model = fitted_model, coefs = coefs)

    #calculate CIs
    multiplier = qt(1-((1-level)/2), df) #to calculate the CIs
    coefs$CI_lower = coefs[, 1] - multiplier*coefs[, 2] #lower
    coefs$CI_upper = coefs[, 1] + multiplier*coefs[, 2] #upper

  }

  #fill in object
  return_list[["coefs"]] = coefs
  return_list[["meta"]] = model_meta
  return_list[["model_obj"]] = fitted_model
  return_list[["aov"]] = fitted_model %>% aov
  return_list[["aov_etas"]] = fitted_model %>% MOD_etas

  #change class for custom printing
  class(return_list) = "model_summary"


  return(return_list)
}


#' Repeatedly run glmnet.cv()
#'
#' Returns a data frame of beta coefficients from glmnet.cv() fits.
#'
#' The messages parameter is depreceated in favor of the progress parameter.
#' @param data (data.frame) A data.frame with the data. Must contain dependent and predictor variables.
#' @param dependent (chr scalar) The name of the dependent variable.
#' @param predictors (chr vector) The names of the predictor variables.
#' @param weights_ (num vector/chr scalar) If weights should be used, a numeric vector of values to use or chr vector.
#' @param standardize (log scalar) Whether to standardize the data beforehand.
#' @param runs (int scalar) Number of times to run.
#' @param nfolds (int) Number of folds.
#' @param alpha_ (num scalar) The penalty to use. 1 = lasso regression, 0 = ridge regression.
#' @param NA_ignore (log scalar) Whether to remove cases with missing data.´´
#' @param messages (log scalar) Whether to show messages.
#' @param progress (log scalar) Whether to show a progress bar.
#' @param seed (int scalar) The seed to use. For reproducible results.
#' @export
#' @examples
#' MOD_LASSO(iris, "Sepal.Length", predictors = colnames(iris)[-1])
MOD_LASSO = function(data, dependent, predictors, weights_ = NULL, standardize = T, runs = 100, nfolds = 10, alpha_ = 1, NA_ignore = T, seed = 1, messages = T, progress = T) {

  #check input
  data
  dependent
  predictors

  if (length(predictors) < 2) stop("There must be at least two predictors. This is a limitation of glmnet::glmnet.")
  is_(data, class="data.frame", error_on_false = T)
  is_(dependent, class="character", size = 1, error_on_false = T)
  if (dependent %in% predictors) stop(sprintf("The dependent variable cannot be a predictor! %s was both", dependent))

  #check variables exist in data
  sapply(c(dependent, predictors), FUN = function(var) {
    if (!var %in% names(data)) stop(sprintf("Some variables were not in the supplied data: %s", var), call. = F)
  })

  #rename data
  df = data; rm(data)

  #set seed
  if (!is.na(seed)) set.seed(seed)

  #weights
  if (is.null(weights_)) {
    df$weights_ = rep(1, nrow(df))
  } else if (is_(weights_, class = "character", size = 1)) {
    df$weights_ = df[[weights_]]
  } else if (is_(weights_, class = "numeric", size = nrow(df))) {
    df$weights_ = weights_
  } else {
    stop("Length of weights does not match df!")
  }

  #subset
  df = df[c(dependent, predictors, "weights_")]

  #missing data
  if (NA_ignore) {
    if (any(is.na(df))) {
      df = df %>% na.omit()
      if (messages) message("Missing data removed.")
    }
  }

  #standardize
  if (standardize) {
    df = df_standardize(df, exclude = "weights_", messages = messages)
    if (messages) message("Data standardized.")
  }

  #save object
  results_df = data.frame(matrix(nrow = runs, ncol = 0))

  #loop
  if (progress) pb <- txtProgressBar(min = 1, max = runs, initial = 1, style = 3)
  for (run in 1:runs) {
    if (progress) setTxtProgressBar(pb, value = run)

    #fit lasso
    fit_cv = glmnet::cv.glmnet(x = as_num_matrix(df[predictors]), #predictor vars matrix
                       y = as_num_matrix(df[dependent]), #dep var matrix
                       weights = df$weights_, #weights
                       alpha = alpha_,
                       nfolds = nfolds)

    #fetch coefs and names
    pred_betas = coef(fit_cv) #coefs
    pred_names = dimnames(pred_betas)[[1]] #names

    #save coefs
    for (beta_idx in seq_along(pred_names)) { #for each coef
      results_df[run, pred_names[beta_idx]] = pred_betas[beta_idx] #save it
    }
  }
  if (progress) close(pb)

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
MOD_summarize_models = function(df, digits = 3, desc = c("mean", "median", "sd", "mad", "fraction_zeroNA"), include_intercept = F) {

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
#' @param progress (lgl scalar) Whether to show a progress bar, default yes.
#' @export
#' @examples
#' fit = lm("Petal.Length ~ Sepal.Length", data = iris) #a fit
#' fit_wtd = lm("Petal.Length ~ Sepal.Length", data = iris, weight = Sepal.Width) #with weights
#' MOD_k_fold_r2(fit) #raw r2 and cv r2
#' MOD_k_fold_r2(fit_wtd) #different r2 due to weights
MOD_k_fold_r2 = function(lmfit, folds = 10, runs = 20, seed = 1, progress = interactive()) {

  #get data
  data = lmfit$model

  #seed
  if (!is.na(seed)) set.seed(seed)

  if (progress) pb <- txtProgressBar(min = 1, max = runs, initial = 1, style = 3)
  v_runs = sapply(1:runs, FUN = function(run) {
    if (progress) setTxtProgressBar(pb, value = run)
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
      trial = try({ #we try because it can fail if new levels appear in the test set not in the train set
        preds = predict(fit, newdata = test_data)
      }, silent = T)
      if (is_error(trial)) return(NA)

      #calculate SSE and R2
      # http://stats.stackexchange.com/questions/32596/what-is-the-difference-between-coefficient-of-determination-and-mean-squared
      v_sq_errors = (test_data[[1]] - preds)^2 #squared errors
      v_sq_errors_sum = wtd_sum(v_sq_errors, test_data$.weights) #weighted sum
      v_squared_deviations = (test_data[[1]] - wtd_mean(train_data[[1]], train_data$.weights))^2 #squared deviations from mean
      v_sum_squared_deviations = wtd_sum(v_squared_deviations, test_data$.weights)

      #R2
      1 - (v_sq_errors_sum / v_sum_squared_deviations)
    }) %>%
      mean(, na.rm=T)
  })
  if (progress) close(pb)

  #return
  c("raw_r2" = summary(lmfit)$r.squared, "cv_r2" = mean(v_runs, na.rm=T))
}



