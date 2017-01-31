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
  expect_true(all(cor_matrix(iris, CI = .95) %>% get_dims %>% equals(c(4, 4))))
  expect_true(cor_matrix(iris[-5], weights = iris[-5], CI = .95) %>% is.character)
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
  SMD_matrix(iris$Sepal.Length, iris$Species, trim = .05),

  #with missing data
  SMD_matrix(iris_miss$Sepal.Length, iris_miss$Species)
)

test_that("SMD_matrix", {
  #correct type
  expect_true(all(purrr::map_lgl(t, is.matrix)))

  #all different results
  unique(t) %>% length %>% equals(7)
})

