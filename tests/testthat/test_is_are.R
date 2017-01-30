context("is/are")


# lengths_match -----------------------------------------------------------

test_that("lengths_match", {
  expect_true(lengths_match(1:4, 5:8))
  expect_true(lengths_match(iris, iris[1:2]))
  expect_false(lengths_match(iris, iris[1:2], dimension = 2))
  expect_false(silence(lengths_match(iris, 1:3, dimension = 2)))
  expect_warning(lengths_match(iris, 1:3, dimension = 2))
})

