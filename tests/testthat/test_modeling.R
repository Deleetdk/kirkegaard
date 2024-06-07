context("MOD_")



#test calls
test_that("summarize_models", {
  #fit some models
  #RMS
  models1 = list(
    rms::ols(Sepal.Width ~ Sepal.Length, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length * Petal.Width, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width + Species, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width * Species, data = iris)
  )

  #base R
  models2 = list(
    lm(Sepal.Width ~ Sepal.Length, data = iris),
    lm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
    lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
  )

  #RMS with rcs()
  models3 = list(
    rms::ols(Sepal.Width ~ Sepal.Length, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris),
    rms::ols(Sepal.Width ~ rms::rcs(Sepal.Length) + Petal.Width + Petal.Length, data = iris)
  )

  #MASS::rlm()
  models_rlm = list(
    MASS::rlm(Sepal.Width ~ Sepal.Length, data = iris),
    MASS::rlm(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
    MASS::rlm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris),
    MASS::rlm(Sepal.Width ~ rms::rcs(Sepal.Length) + Petal.Width + Petal.Length, data = iris)
  )

  #these are lists
  expect_true(class(models1) == "list")
  expect_true(class(models2) == "list")
  expect_true(class(models3) == "list")
  expect_true(class(models3) == "list")

  #summaries
  models_sum1a = models1 %>% summarize_models()
  models_sum1b = models1 %>% summarize_models(asterisks = NULL)
  models_sum1c = models1 %>% summarize_models(asterisks_only = F)

  models_sum2 = models2 %>% summarize_models()
  models_sum3 = models3 %>% summarize_models()
  models_sum_rlm = models_rlm %>% summarize_models()

  #these are data frames
  expect_true(is.data.frame(models_sum1a))
  expect_true(is.data.frame(models_sum1b))
  expect_true(is.data.frame(models_sum1c))
  expect_true(is.data.frame(models_sum2))
  expect_true(is.data.frame(models_sum3))
  expect_true(is.data.frame(models_sum_rlm))

  #special iris for testing collapsing and reference classes
  iris_test = iris %>%
    mutate(
      other_factor = rep(
      letters[1:2],
      length.out = 150) %>%
        factor(),
      aaa = rnorm(150)
           )

  #fit sets of models with different summarize pars
  res1 = list(mod1 = rms::ols(Sepal.Length ~ Sepal.Width , data = iris_test),
              mod2 = rms::ols(Sepal.Length ~ Sepal.Width + Species, data = iris_test),
              mod3 = rms::ols(Sepal.Length ~ Sepal.Width + Species + other_factor, data = iris_test)
  ) %>% summarize_models(collapse_factors = T)

  res2 = list(mod1 = rms::ols(Sepal.Length ~ Sepal.Width , data = iris_test),
              mod2 = rms::ols(Sepal.Length ~ Sepal.Width + Species, data = iris_test),
              mod3 = rms::ols(Sepal.Length ~ Sepal.Width + Species + other_factor, data = iris_test)
  ) %>% summarize_models(collapse_factors = "Species")

  res3 = list(mod1 = rms::ols(Sepal.Length ~ Sepal.Width , data = iris_test),
              mod2 = rms::ols(Sepal.Length ~ Sepal.Width + Species, data = iris_test),
              mod3 = rms::ols(Sepal.Length ~ Sepal.Width + Species + other_factor, data = iris_test)
  ) %>% summarize_models()

  res4 = list(mod1 = rms::ols(Sepal.Length ~ Sepal.Width , data = iris_test),
              mod2 = rms::ols(Sepal.Length ~ Sepal.Width + Species, data = iris_test),
              mod3 = rms::ols(Sepal.Length ~ Sepal.Width + Species + other_factor, data = iris_test)
  ) %>% summarize_models(add_ref_level = F, collapse_factors = T)

  res5 = list(mod1 = rms::ols(Sepal.Length ~ Sepal.Width , data = iris_test),
              mod2 = rms::ols(Sepal.Length ~ Sepal.Width + Species, data = iris_test),
              mod3 = rms::ols(Sepal.Length ~ Sepal.Width + Species + other_factor, data = iris_test)
  ) %>% summarize_models(add_ref_level = F, collapse_factors = F)

  #test variable orderings
  res6 = list(bb = rms::ols(Sepal.Length ~ Species + aaa, data = iris_test),
              aa = rms::ols(Sepal.Length ~ Species + other_factor + aaa, data = iris_test)
  ) %>% summarize_models()

  #test contents
  #set 1
  #no predictors with = present
  expect_true(!any(str_detect(res1$`Predictor/Model`, "=")))
  #no "yes" for first model
  expect_true(!any(str_detect(res1$mod1, "yes"), na.rm = T))
  #but in both second and third
  expect_true(any(str_detect(res1$mod2, "yes")))
  expect_true(any(str_detect(res1$mod3, "yes")))

  #set 2
  #no Species= present
  expect_true(!any(str_detect(res2$`Predictor/Model`, "Species=")))
  #no "yes" for first model
  expect_true(!any(str_detect(res2$mod1, "yes"), na.rm = T))
  #but in both second and third
  expect_true(any(str_detect(res2$mod2, "yes")))
  expect_true(any(str_detect(res2$mod3, "yes")))
  #and ref for third
  expect_true(any(str_detect(res2$mod3, "ref")))
  #two factor levels are given
  expect_true(sum(str_detect(res2$`Predictor/Model`, "=")) == 2)

  #set 3
  #no "yes" or "ref" for first model
  expect_true(!any(str_detect(res3$mod1, "yes|ref"), na.rm = T))
  #no yes in 2nd and 3rd
  expect_true(!any(str_detect(res3$mod2, "yes"), na.rm = T))
  expect_true(!any(str_detect(res3$mod3, "yes"), na.rm = T))

  #1 ref in model 2
  expect_true(sum(str_detect(res3$mod2, "ref"), na.rm = T) == 1)
  #2 ref for third
  expect_true(sum(str_detect(res3$mod3, "ref"), na.rm = T) == 2)

  #set 4
  #no "ref" in model 3
  expect_true(!any(str_detect(res4$mod3, "ref"), na.rm = T))

  #1 yes in model 2
  expect_true(sum(str_detect(res4$mod2, "yes"), na.rm = T) == 1)
  #2 yes for third
  expect_true(sum(str_detect(res4$mod3, "yes"), na.rm = T) == 2)
  #equivalent to res1 with the default arg
  expect_equivalent(res1, res4)

  #set 5, clean
  #no "ref" or "yes" anywhere
  expect_true(!any(str_detect(res5$mod1, "ref|yes"), na.rm = T))
  expect_true(!any(str_detect(res5$mod2, "ref|yes"), na.rm = T))
  expect_true(!any(str_detect(res5$mod3, "ref|yes"), na.rm = T))

  #set 6, variable orders, should ignore alphabetic sorting
  expect_true(res6$`Predictor/Model`[1] == "Intercept")
  expect_true(names(res6)[2] == "bb")

  #prior bug with missing models
  #bug
  brokenmodels = read_rds(system.file("data/brokenmodels.rds", package = "kirkegaard"))
  brokenmodels %>%
    summarize_models(asterisks_only = F, collapse_factors = "region") ->
    res7

  expect_equal(ncol(res7), 4)
  expect_equal(nrow(res7), 13)

})

#get_model_coefs
test_that("get_model_coefs", {
  options(contrasts=c("contr.treatment", "contr.treatment"))
  #some data to use with each type of variable type
  set.seed(1)
  ex_data_n = 1000
  ex_data = tibble(
    y = rnorm(ex_data_n),
    num = rnorm(ex_data_n),
    int = rpois(ex_data_n, 1),
    fac = sample(letters[1:2], replace = T, size = ex_data_n) %>% factor(),
    ord = sample(letters[1:3], replace = T, size = ex_data_n) %>% ordered(),
    chr = sample(letters[1:3], replace = T, size = ex_data_n),
    lgl = rbinom(ex_data_n, 1, 0.5) %>% as.logical()
  )

  #base R
  models_lm = list(
    lm(y ~ num + int, data = ex_data),
    lm(y ~ num + int + fac + ord + chr + lgl, data = ex_data),
    lm(y ~ num + int + fac + ord + chr + lgl + num * int + num * fac + fac * ord, data = ex_data)
  )

  #fit the same models as above but with rms::ols
  models_ols = list(
    rms::ols(y ~ num + int, data = ex_data),
    rms::ols(y ~ num + int + fac + ord + chr + lgl, data = ex_data),
    rms::ols(y ~ rms::rcs(num) + int + fac + ord + chr + lgl, data = ex_data),
    rms::ols(y ~ num + int + fac + ord + chr + lgl + num * int + num * fac + fac * ord, data = ex_data)
  )

  #MASS::rlm()
  models_rlm = list(
    MASS::rlm(y ~ num + int, data = ex_data),
    MASS::rlm(y ~ num + int + fac + ord + chr + lgl, data = ex_data),
    MASS::rlm(y ~ num + int + fac + ord + chr + lgl + num * int + num * fac + fac * ord, data = ex_data)
  )

  #logistic
  models_glm_logit = list(
    glm(lgl ~ num + int, data = ex_data, family = binomial),
    glm(lgl ~ num + int + fac + ord + chr, data = ex_data, family = binomial),
    glm(lgl ~ num + int + fac + ord + chr + num * int + num * fac + fac * ord, data = ex_data, family = binomial)
  )

  #these are lists
  expect_true(class(models_lm) == "list")
  expect_true(class(models_ols) == "list")
  expect_true(class(models_rlm) == "list")
  expect_true(class(models_glm_logit) == "list")

  #get coefs
  coefs_lm = models_lm %>% get_model_coefs()
  coefs_rlm = models_rlm %>% get_model_coefs()
  coefs_ols = models_ols %>% get_model_coefs()
  coefs_glm_logit = models_glm_logit %>% get_model_coefs()

  #these are data frames
  expect_true(is.data.frame(coefs_lm))
  expect_true(is.data.frame(coefs_rlm))
  expect_true(is.data.frame(coefs_ols))
  expect_true(is.data.frame(coefs_glm_logit))

  #check if columsn are correct, at least some of them
  expected_cols = c("term", "estimate", "std.error", "statistic", "p.value")
  expect_true(all(expected_cols %in% colnames(coefs_lm)))
  expect_true(all(expected_cols %in% colnames(coefs_rlm)))
  expect_true(all(expected_cols %in% colnames(coefs_ols)))
  expect_true(all(expected_cols %in% colnames(coefs_glm_logit)))
})

#compare_predictors
test_that("compare_predictors", {
  #fit linear models for built in datasets
  iris_models = compare_predictors(iris, names(iris)[1], names(iris)[-1])
  iris_models2 = compare_predictors(iris, names(iris)[1], names(iris)[-1], additional_models = list(petal = c("Petal.Length", "Petal.Width")))
  mpg_models = compare_predictors(mpg, names(mpg)[3], names(mpg)[-3])

  #fit logistic models for another dataset
  mpg$audi = mpg$manufacturer == "audi"
  mpg_models_logit = compare_predictors(mpg, outcome = "audi", predictors = c("displ", "year", "cty", "hwy"), family = binomial)

  #check that the output is a data frame
  expect_true(is.data.frame(iris_models))
  expect_true(is.data.frame(iris_models2))
  expect_true(is.data.frame(mpg_models))
  expect_true(is.data.frame(mpg_models_logit))

  #check if columsn are correct, at least some of them
  expected_cols = c("term", "estimate", "std.error", "statistic", "p.value")
  expect_true(all(expected_cols %in% colnames(iris_models)))
  expect_true(all(expected_cols %in% colnames(iris_models2)))
  expect_true(all(expected_cols %in% colnames(mpg_models)))
  expect_true(all(expected_cols %in% colnames(mpg_models_logit)))
})
