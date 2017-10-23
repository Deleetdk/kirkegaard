### tests for df_ functions

context("simple_math")

# rescale ------------------------------------------------------------------

test_that("rescale", {
  #basic
  expect_true(min_max(rescale(1:10, new_min = 0, new_max = 1)) %equals% c(0, 1))
  expect_true(min_max(rescale(1:10, new_min = 1, new_max = 5)) %equals% c(1, 5))
  expect_true(min_max(rescale(rnorm(100), new_min = 1, new_max = 0)) %equals% c(0, 1))

  #complex
  #assume old numbers belong to scale 0-1, rescale to 10-20 scale, i.e. 11, 15, 20
  expect_true(all(rescale(c(.1, .5, 1), new_min = 10, new_max = 20, old_min = 0, old_max = 1) == c(11, 15, 20)))

  #NAs
  expect_true(min_max(rescale(c(1:5, NA, 6:10), 0, 1)) %equals% c(0, 1))
  expect_true(rescale(rep(NA_real_, 1e4), 0, 1) %equals% rep(NA_real_, 1e4))

  #empty input
  expect_true(rescale(numeric(), 0, 1) %equals% numeric())
})

