context("MOD_")


# summarize_models -------------------------------------------------------------------------

#test calls
test_that("summarize_models", {
  #fit some models
  #RMS
  models1 = list(
    rms::ols(Sepal.Width ~ Sepal.Length, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width, data = iris),
    rms::ols(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
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
    mutate(other_factor = rep(
      letters[1:2],
      length.out = 150) %>%
        factor()
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

})

