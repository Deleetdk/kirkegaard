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

#TODO add tests with missing data
#TODO add tests with weights

