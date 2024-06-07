library(tidyverse)
library(earth)
library(ggeffects)

#make data range
make_focal_data_range = function(x, length = 1000) {
  #if numeric
  if (is.numeric(x)) {
    y = seq(min(x), max(x), length.out = length)
  } else {
    y = unique(x)
  }

  y
}

#make data range for non-first focal term
make_focal_data_range_ordinal = function(x, centiles = pnorm(seq(-2, 2))) {
  #if numeric, find the 0.0228 0.1587 0.5000 0.8413 0.9772 centile values
  if (is.numeric(x)) {
    y = quantile(x, centiles)
  } else {
    y = unique(x)
  }

  y
}

#keep covariates at constant value
make_covar_data = function(x) {
  if (is.numeric(x)) {
    y = mean(x)
  } else {
    x_table = table(x)
    which_mode = which.max(x_table)
    y = x[x == names(x_table)[which_mode]][1]
  }

  y
}

#prep newdata for model predictions
prep_newdata = function(focal_terms, covar_terms, data) {
  #prep a call for expand_grid
  call_args = list()

  #make ranges for the first term
  for (t in focal_terms) {
    #the data values depend on the order
    if (t == focal_terms[1]) {
      #if it's the first, use continuous range
      call_args[[t]] = make_focal_data_range(data[[t]])
    } else {
      #are explicit values given?
      if (str_detect(t, "\\[")) {
        #get the term by itself
        t_clean = str_remove(t, " \\[.*")
        t_bracket = str_match(t, "\\[(.*)\\]")[, 2]
        call_args[[t_clean]] = str_match_all(t_bracket, "\\d+") %>% extract2(1) %>% as.numeric() %>% sort()
      } else {
        #if it's 2nd or later, split the range into ordinals
        call_args[[t]] = make_focal_data_range_ordinal(data[[t]])
      }
    }
  }

  #make cover data
  for (t in covar_terms) {
    #skip if exists
    #this happens if the term has a custom range set
    if (t %in% names(call_args)) next

    call_args[[t]] = make_covar_data(data[[t]])
  }

  #expand data
  newdata = rlang::exec(
    expand_grid,
    !!!call_args
  )

  newdata
}

#get model predictions
get_model_preds = function(model, newdata) {
  #get classes
  model_classes = class(model)

  #get predictions from basic model types
  if ("lm" %in% model_classes) {
    newdata_preds = predict(model, newdata = as.data.frame(newdata), interval = "confidence") %>%
      as_tibble() %>%
      set_names(c("pred", "pred_lwr", "pred_upr"))
  } else if ("earth" %in% model_classes) {
    newdata_preds = tibble(
      pred = predict(model, newdata = as.data.frame(newdata)) %>% as.vector(),
      pred_lwr = NA,
      pred_upr = NA
    )
  } else {
    warning(str_glue("model class `{str_c(model_classes, collapse = ', ')}` may not be supported"))
    newdata_preds = predict(model, newdata = as.data.frame(newdata), interval = "confidence") %>%
      as_tibble() %>%
      set_names(c("pred", "pred_lwr", "pred_upr"))
  }

  newdata_preds
}

#poor man's ggpredict
ggpredict2 = function(model, focal_terms, data) {
  #determine covar terms
  all_pred_terms = model$namesx
  covar_terms = setdiff(all_pred_terms, focal_terms)

  #make newdata data frame
  newdata = prep_newdata(focal_terms, covar_terms, data)

  #add predictions
  newdata_preds = get_model_preds(model, newdata)

  bind_cols(
    newdata,
    newdata_preds
  )
}

#' Bootstrap earth (MARS) models
#'
#' This function fits a set of earth models to the data, where each model is fit to a bootstrap sample. The function returns a list of models.
#'
#' @param y The outcome variable
#' @param x The predictor variables
#' @param n_boot Number of bootstrap samples. Default is 1000.
#' @param seed Seed for reproducibility. Default is 1.
#' @param progress Whether to show progress bar. Default is TRUE.
#' @param ... Additional arguments to pass to `earth::earth`. See `?earth::earth` for more information. Most useful are `degree` and `nprune`.
#'
#' @return A list of models
#' @export
#'
#' @examples
#' iris_earth_fits = earth_bootstrap(iris$Sepal.Length, iris[-1])
earth_bootstrap = function(y, x, n_boot = 1000, seed = 1, progress = T, ...) {
  #seed
  set.seed(seed)

  #make y a column vector if its a vector
  #its so that we can be consistent with subsetting rows whehther there's 1 or more dependent variables
  if (is.vector(y)) y = matrix(y, ncol = 1)

  #bootstrap
  boot_models = purrr::map(1:n_boot, .progress = progress, .f = \(i) {
    #sample
    idx = sample(1:nrow(y), replace = T)

    #fit model
    model = earth::earth(
      x = x[idx, ],
      y = y[idx, ],
      ...
    )

    #return
    model
  })

  #return
  boot_models
}

#impute some personality data
bfi = psych::bfi %>% na.omit()
bfi$gender = mapvalues(bfi$gender, from = c(1, 2), to = c("male", "female")) %>% factor()

#try it out
bfi_mars_bs = earth_bootstrap(
  y = bfi$age,
  x = bfi %>% select(A1:O5, gender),
  n_boot = 100,
  degree = 2
)

#example
bfi_mars_bs[[1]] %>% summary()

#make a set of predictions
ggpredict2(
  model = bfi_mars_bs[[1]],
  focal_terms = c("A1"),
  data = bfi
)


#get predictions for the models
bfi_mars_bs_preds = map_dfr(seq_along(bfi_mars_bs), .f = \(i) {
  #make predictions
  ggpredict2(
    model = bfi_mars_bs[[i]],
    focal_terms = c("A1"),
    data = bfi
  ) %>%
    mutate(model = i)
})


#plot the predictions together as many lines
ggplot(bfi_mars_bs_preds, aes(x = A1, y = pred, group = model)) +
  geom_line(alpha = 0.1) +
  theme_minimal()

#aggregate centiles
bfi_mars_bs_preds_agg = bfi_mars_bs_preds %>%
  group_by(A1) %>%
  summarise(
    pred_mean = mean(pred),
    pred_lwr = quantile(pred, 0.025),
    pred_upr = quantile(pred, 0.975)
  )

#plot
ggplot(bfi_mars_bs_preds_agg, aes(x = A1, y = pred_mean, ymin = pred_lwr, ymax = pred_upr)) +
  geom_line() +
  geom_ribbon(alpha = 0.1) +
  theme_minimal()

#2 variables
#get predictions for the models
bfi_mars_bs_preds2 = map_dfr(seq_along(bfi_mars_bs), .f = \(i) {
  #make predictions
  ggpredict2(
    model = bfi_mars_bs[[i]],
    focal_terms = c("A1", "A4 [1, 6]"),
    data = bfi
  ) %>%
    mutate(model = i)
})


#aggregate centiles
bfi_mars_bs_preds_agg2 = bfi_mars_bs_preds2 %>%
  mutate(
    A4 = as.factor(A4)
  ) %>%
  group_by(A1, A4) %>%
  summarise(
    pred_mean = mean(pred),
    pred_lwr = quantile(pred, 0.025),
    pred_upr = quantile(pred, 0.975)
  )

#plot
ggplot(bfi_mars_bs_preds_agg2, aes(x = A1, y = pred_mean, ymin = pred_lwr, ymax = pred_upr, color = A4)) +
  geom_line() +
  geom_ribbon(alpha = 0.1) +
  theme_minimal()

