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

  #these are lists
  expect_true(class(models1) == "list")
  expect_true(class(models2) == "list")
  expect_true(class(models3) == "list")

  #summaries
  models_sum1a = models1 %>% summarize_models()
  models_sum1b = models1 %>% summarize_models(asterisks = NULL)
  models_sum1c = models1 %>% summarize_models(asterisks_only = F)
  models_sum2 = models2 %>% summarize_models()
  models_sum3 = models3 %>% summarize_models()

  #these are data frames
  expect_true(is.data.frame(models_sum1a))
  expect_true(is.data.frame(models_sum1b))
  expect_true(is.data.frame(models_sum1c))
  expect_true(is.data.frame(models_sum2))
  expect_true(is.data.frame(models_sum3))
})

