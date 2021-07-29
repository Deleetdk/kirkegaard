context("packages")

test_that("load_packages", {
  expect_true(load_packages(ggplot2))

  expect_error(load_packages(ggplot3))
})


test_that("load_packages", {
  expect_true(assert_installed(ggplot2))

  expect_error(assert_installed(ggplot3))
})

