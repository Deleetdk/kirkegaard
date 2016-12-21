### tests for simple functions for weights
#test functions in the order they rely on each other
#test with and without weights
#test error parameter

context("wtd_")

# wtd_sum -----------------------------------------------------------------

test_that("wtd_sum", {
  expect_equal(wtd_sum(1:10), 55)
  expect_equal(wtd_sum(1:10, w = 1:10), 70)
  expect_true(throws_error(wtd_sum(NA)))
  expect_equal(wtd_sum(NA, error = F), NaN)
})


# wtd_mean ----------------------------------------------------------------

test_that("wtd_mean", {
  expect_equal(wtd_mean(1:10), 5.5)
  expect_equal(wtd_mean(1:10, w = 1:10), 7.0)
  expect_true(throws_error(wtd_mean(NA)))
  expect_equal(wtd_mean(NA, error = F), NaN)
})


# wtd_sd -----------------------------------------------------------------

test_that("wtd_sd", {
  expect_equal(wtd_sd(1:10), sd(1:10))
  expect_equal(wtd_sd(1:10, w = 1:10), 3.41565, tolerance = 1e-6)
  expect_true(throws_error(wtd_sd(NA)))
  expect_equal(wtd_sd(NA, error = F), NaN)
})
