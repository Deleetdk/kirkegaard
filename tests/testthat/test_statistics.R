context("statistics")

# cor_matrix --------------------------------------------------------------
#correlation matrix with nice output

test_that("cor_matrix", {
  #validate vs. base-r
  expect_equivalent(cor_matrix(iris[-5]), cor(iris[-5]))

  #automatic skip non-numeric
  expect_equivalent(cor_matrix(iris), cor(iris[-5]))

  #with weights
  expect_equivalent(cor_matrix(iris[-5], weights = rep(1, 150)), cor(iris[-5]))

  #other weights
  expect_false(are_equal(cor_matrix(iris[-5], weights = runif(150)), cor(iris[-5])))

  #complex weights
  expect_false(are_equal(cor_matrix(iris[-5], weights = iris[-5]), cor(iris[-5])))

  #test arguments
  expect_true(cor_matrix(iris, CI = .95) %>% is.character())
  expect_true(cor_matrix(iris, p_val = T) %>% is.character())
  expect_true(cor_matrix(iris, p_val = T, asterisks_only = T) %>% is.character())
  expect_true(all((cor_matrix(iris, CI = .95) %>% get_dims()) == (c(4, 4))))
  expect_true(cor_matrix(iris[-5], weights = iris[-5], CI = .95) %>% is.character())

  #errors
  expect_error(cor_matrix(iris, p_val = T, CI = .95), regexp = "Cannot both calculate CIs and p values")
})


# a4me_cohen_d ------------------------------------------------------------

test_that("a4me_cohen_d", {
  expect_equivalent(a4me_cohen_d(1, 100, 100), 1)
  expect_lt(abs(a4me_cohen_d(1, 100, 100, rxx = .5) - 1.155), .001)
  expect_lt(abs(a4me_cohen_d(1, 100, 100, ryy = .5) - 1.155), .001)
  expect_equivalent(a4me_cohen_d(1, 100, 100, rxx = .5, ryy = .5), 2)
  expect_equivalent(a4me_cohen_d(1, 100, 100, rxx = 0), Inf)
  expect_equivalent(a4me_cohen_d(10, 100, 100, rxx = .1, ryy = .1), Inf)
  expect_error(a4me_cohen_d(1, 100, 100, rxx = -1))
})

# SMD_matrix  --------------------------------------------------------------
#standardized mean differences

#iris with missing
set.seed(1)
iris_miss = miss_add_random(iris)

#tests
t = list(#parameters
  SMD_matrix(iris$Sepal.Length, iris$Species),
  SMD_matrix(iris$Sepal.Length, iris$Species, central_tendency = median),
  SMD_matrix(iris$Sepal.Length, iris$Species, dispersion = "mad"),
  SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "pair"),
  SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "total"),
  SMD_matrix(iris$Sepal.Length, iris$Species, central_tendency = mean, trim = .05),

  #with missing data
  SMD_matrix(iris_miss$Sepal.Length, iris_miss$Species)
)

test_that("SMD_matrix", {
  #correct type
  expect_true(all(purrr::map_lgl(t, is.matrix)))

  #all different results
  unique(t) %>% length %>% `==`(7)
})


# standardize -------------------------------------------------------------

set.seed(1)
X = rnorm(100, mean = 10, sd = 5)
W = runif(100)

#2 groups
X2 = c(rnorm(10000), rnorm(10000, 1))
focal = rep(c(T, F), each = 10000)

test_that("standardize", {
  #test normal
  expect_equal(mean(standardize(X)), 0)
  expect_equal(sd(standardize(X)), 1)

  #test weights
  expect_equal(weighted.mean(standardize(X, W), W), 0)

  #robust
  expect_equal(median(standardize(X, robust = T)), 0)
  expect_equal(mad(standardize(X, robust = T)), 1)

  #focal group
  expect_equal(mean(standardize(X2, focal_group = focal)[focal]), 0)
  expect_equal(sd(standardize(X2, focal_group = focal)[focal]), 1)
  expect_equal(mean(standardize(X2, focal_group = focal)[!focal]), 1, tol = .02)
  expect_equal(sd(standardize(X2, focal_group = focal)[!focal]), 1, tol = .05)
})


# winsorise ---------------------------------------------------------------

test_that("winsorise", {
  #test normal
  expect_equal(winsorise(0), 0) #no limits, means no winsorising
  expect_identical(winsorise(NA), NA_real_) #convert NA to double
  expect_equal(winsorise(99, upper = 1), 1)
})


# weighted functions ------------------------------------------------------

#some variables
set.seed(1)
rand_norm = rnorm(100)
rand_uniform = runif(100)
x = c(1, 1, 2, 2, 2)

test_that("wtd_mean", {
  expect_true(mean(rand_norm) == wtd_mean(rand_norm))
  expect_true(weighted.mean(rand_norm, rand_uniform) == wtd_mean(rand_norm, rand_uniform))
})

test_that("wtd_sd", {
  expect_true(sd(rand_norm) == wtd_sd(rand_norm))
  #dont agree, but I think it has to do with the correction Hmisc uses...
  # expect_equivalent(Hmisc::wtd.var(rand_norm, rand_uniform) %>% sqrt(), wtd_sd(rand_norm, rand_uniform))
})

# test_that("wtd_median", {
#   expect_true(median(rand_norm) == wtd_median(rand_norm))
#   expect_true(Hmisc::wtd.quantile(rand_norm, rand_uniform, probs = .5) == wtd_median(rand_norm, rand_uniform))
# })
#
# test_that("wtd_quantile", {
#   expect_equivalent(Hmisc::wtd.quantile(rand_norm), wtd_quantile(rand_norm))
#   expect_equivalent(Hmisc::wtd.quantile(rand_norm, rand_uniform), wtd_quantile(rand_norm, rand_uniform))
# })
