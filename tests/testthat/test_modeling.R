context("MOD_")


# MOD_summary -------------------------------------------------------------

#test with all types of preds
set.seed(1)
tmp_data = tibble(x_num = rnorm(100),
                             x_lgl = rbinom(100, 1, .5) %>% as.logical(),
                             x_fct = factor(sample(letters[1:3], size = 100, replace = T)),
                             x_ord = ordered(sample(1:3, size = 100, replace = T)),
                             x_chr = sample(letters[1:3], size = 100, replace = T),
                             x_num_rev = x_num * -1,

                             y_num = rnorm(100),
                             y_lgl = rbinom(100, 1, .5) %>% as.logical(),
                             y_fct = factor(sample(letters[1:3], size = 100, replace = T)),
                             y_ord = ordered(sample(1:3, size = 100, replace = T)),
                             y_chr = sample(letters[1:3], size = 100, replace = T),
                             y_fct2 = factor(sample(letters[1:2], size = 100, replace = T)),
                             y_ord2 = ordered(sample(letters[1:2], size = 100, replace = T)),
                             stringsAsFactors = F
)
tmp_data_std = tmp_data %>% df_standardize(messages = F)
tmp_data_miss = tmp_data_std %>% miss_add_random()

#fit to standardized data
lm_1 = lm("y_num ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data_std) %>% MOD_summary(standardize = F, kfold = F)
#fit to unstd. data, then std parameters
lm_2 = lm("y_num ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data) %>% MOD_summary(kfold = F)
#test cv
lm_3 = lm("y_num ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data) %>% MOD_summary(progress = F, runs = 3)
#test chr
lm_4 = lm("y_num ~ x_num + x_lgl + x_fct + x_ord + x_chr", data = tmp_data) %>% {silence(MOD_summary(., kfold = F))}
#test linear depdendency error
lm_5 = lm(y_num ~ x_num + x_num_rev, data = tmp_data)
#test 1 pred model
lm_6 = lm(Sepal.Length ~ Petal.Length, data = iris) %>% MOD_summary(kfold = F)

test_that("MOD_summary_lm", {
  ## lm
  #class
  expect_is(lm_1, "model_summary")
  expect_is(lm_2, "model_summary")
  expect_is(lm_3, "model_summary")
  expect_is(lm_4, "model_summary")
  expect_is(lm_6, "model_summary")

  #parameter estimates
  expect_equivalent(lm_1$coefs, lm_2$coefs)
  expect_equivalent(lm_1$meta, lm_2$meta)
  expect_equivalent(lm_1$aov_etas, lm_2$aov_etas)

  #model objs
  expect_false(are_equal(lm_1$model_obj, lm_2$model_obj))
  expect_false(are_equal(lm_1$aov, lm_2$aov))

  #test cv
  expect_true(lm_3$meta$`R2-cv` %>% is.na)
  expect_true(lm_3$meta$`R2-cv` %>% is.na)

  #error
  expect_error(lm_5 %>% MOD_summary())
})


### glm
#fit to standardized data
glm_1 = glm("y_num ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data_std) %>% MOD_summary(standardize = F, kfold = F)
#fit to unstd. data, then std parameters
glm_2 = glm("y_num ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data) %>% MOD_summary(kfold = F)
#lgl outcome
#glm_3 = glm("y_lgl ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data, family = binomial) %>% MOD_summary(kfold = F)
#factor outcome
#glm_4 = glm("y_fct2 ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data, family = binomial) %>% MOD_summary(kfold = F)
#ord outcome
#glm_5 = glm("y_ord2 ~ x_num + x_lgl + x_fct + x_ord", data = tmp_data, family = binomial) %>% MOD_summary(kfold = F)

test_that("MOD_summary_glm", {
  ## lm
  #class
  expect_is(glm_1, "model_summary")
  expect_is(glm_2, "model_summary")
  #expect_is(glm_3, "model_summary")

  #parameter estimates
  expect_equivalent(glm_1$coefs, glm_2$coefs)
  expect_equivalent(glm_1$meta$`pseudo-R2`, glm_2$meta$`pseudo-R2`) #deviance is scale dependent, so not comparable
  expect_equivalent(glm_1$aov_etas, glm_2$aov_etas)

  #model objs
  expect_false(are_equal(glm_1$model_obj, glm_2$model_obj))
  expect_false(are_equal(glm_1$aov, glm_2$aov))

  #test cv
  #expect_false(glm_3$meta$`R2-cv` %>% is.na)
})


# p_to_asterisk -----------------------------------------------------------

test_that("p_to_asterisk", {
  #basics
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(),
                   c("0.123", "0.009*", "0.004**", "<0.001***", "<0.001***"))

  #other thresholds
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c(.05, .01)),
                   c("0.12", "<0.01**", "<0.01**", "<0.01**", "<0.01**"))

  #just 1 threshold
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c(.05)),
                   c("0.12", "<0.01*", "<0.01*", "<0.01*", "<0.01*"))

  #no thresholds
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = c()),
                   c(.123, .009, .004, .0009, .0001))
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks = NULL),
                   c(.123, .009, .004, .0009, .0001))

  #no values
  expect_identical(c(.123, .009, .004, .0009, .0001) %>% p_to_asterisk(asterisks_only = T),
                   c("", "*", "**", "***", "***"))


})


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

