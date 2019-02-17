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


# MOD_LASSO ---------------------------------------------------------------
#frozen results for comparison
#if these change, there must be a change somewhere

fronzen_lasso_results_1 = structure(list(`(Intercept)` = c(1.73655009992001e-16, 1.98347443814766e-16, 1.83633778654183e-16, 1.83633778654183e-16, 1.64632023345824e-16 ), Sepal.Width = c(0.297148980532032, 0.276529631256926, 0.292747731735248, 0.292747731735248, 0.301135657355685), Petal.Length = c(1.1041969609792, 0.969132600423144, 1.06477735354131, 1.06477735354131, 1.13984930455443 ), Petal.Width = c(-0.123208127060014, 0, -0.0855719138957813, -0.0855719138957813, -0.15725399348871)), row.names = c(NA, -5L ), .Names = c("(Intercept)", "Sepal.Width", "Petal.Length", "Petal.Width" ), class = "data.frame")

fronzen_lasso_results_2 = structure(list( `(Intercept)` = c(1.35900310401617e-16, 1.08229580884873e-16, 1.64632023345824e-16, 2.02585158754315e-16, 1.94562186997914e-16 ), Sepal.Width = c(0.228182761916663, 0.326019759131687, 0.301135657355685, 0.279810804430053, 0.287925286697761), Petal.Length = c(0.920778323339888, 1.36266719668455, 1.13984930455443, 0.972412635641068, 1.02160351472909), Petal.Width = c(0, -0.36999796279568, -0.15725399348871, 0, -0.0443491384825481)), row.names = c(NA, -5L), .Names = c("(Intercept)", "Sepal.Width", "Petal.Length", "Petal.Width"), class = "data.frame")


test_that("MOD_LASSO", {
  #results
  expect_equivalent(MOD_LASSO(iris, names(iris)[1], predictors = names(iris)[2:4], progress = F, messages = F, runs = 5), fronzen_lasso_results_1)
  expect_equivalent(MOD_LASSO(iris, names(iris)[1], predictors = names(iris)[2:4], progress = F, messages = F, runs = 5, nfolds = 3), fronzen_lasso_results_2)
})
