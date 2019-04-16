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
  expect_true(cor_matrix(iris, CI = .95) %>% is.character)
  expect_true(all((cor_matrix(iris, CI = .95) %>% get_dims()) == (c(4, 4))))
  expect_true(cor_matrix(iris[-5], weights = iris[-5], CI = .95) %>% is.character)
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
focal = sample(c(T, F), 100, replace = T)

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
  expect_equal(mean(standardize(X, focal_group = focal)[focal]), 0)
  expect_equal(sd(standardize(X, focal_group = focal)[focal]), 1)
})

