context("miss_")


# missing data ------------------------------------------------------------
set.seed(1)
test_data = miss_add_random(iris)

# miss_plot ---------------------------------------------------------------

test_that("miss_plot", {
  expect_is(miss_plot(test_data), "ggplot")
  expect_is(miss_plot(test_data, percent = F), "ggplot")
  expect_is(miss_plot(test_data, case = F), "ggplot")
  expect_is(miss_plot(test_data, case = F, percent = F), "ggplot")
  expect_is(miss_plot(test_data, reverse = T), "ggplot")
})

