context("is/are")


# lengths_match -----------------------------------------------------------

test_that("lengths_match", {
  expect_true(lengths_match(1:4, 5:8))
  expect_true(lengths_match(iris, iris[1:2]))
  expect_false(lengths_match(iris, iris[1:2], dimension = 2))
  expect_false(silence(lengths_match(iris, 1:3, dimension = 2)))
  expect_warning(lengths_match(iris, 1:3, dimension = 2))
})


# all_the_same ------------------------------------------------------------

test_that("all_the_same", {
  expect_true(all_the_same(rep(1, 100)))
  expect_true(!all_the_same(1:100))
})


# is_simple_vector --------------------------------------------------------

test_that("is_simple_vector", {
  expect_true(is.vector(list(1:3)))
  expect_true(is.list(list(1:3)))
  expect_true(!is_simple_vector(list(1:3)))
  expect_true(is.vector(1:3))
  expect_true(!is.list(1:3))
  expect_true(is_simple_vector(1:3))
})


# is_whole_number ---------------------------------------------------------

test_that("is_whole_number", {
  expect_equivalent(is_whole_number(seq(0, 2, .5)), c(T, F, T, F, T))
})


# is_negative, is_positive ------------------------------------------------

test_that("is_positive", {
  expect_equivalent(is_negative(-2:2), c(T, T, F, F, F))
  expect_equivalent(is_positive(-2:2), c(F, F, F, T, T))
})



# is_ ---------------------------------------------------------------------
#flexible function

test_that("is_", {
  expect_true(is_(iris, class = "data.frame"))
  expect_true(is_(iris, class = c("data.frame", "logical", "matrix")))
  expect_true(is_(iris, class = "data.frame", size = c(150, 5)))
  expect_true(is_(iris, size = c(150, 5)))
  expect_true(!is_(iris, size = 1))
  expect_true(is_(iris, type = "list"))
  expect_true(!is_(iris, type = "factor"))
  expect_true(throws_error(is_(iris, class = "list", error_on_false = T)))
})


# is_between --------------------------------------------------------------
#tests whether values are between two limits.

test_that("is_between", {
  #scalars
  expect_true(is_between(5, 0, 10))
  expect_true(is_between(5, 5, 5))
  expect_true(!is_between(1, 5, 5))

  #vectors
  expect_true(all(is_between(0:10, 0, 10)))
  expect_true(all(!is_between(-10:-1, 0, 10)))

  #test arguments
  expect_true(!is_between(1, 1, 2, include_lower = F))
  expect_true(!is_between(2, 1, 2, include_upper = F))
  expect_true(!is_between(1, 1, 1, include_upper = F, include_lower = F))
})


# is_numeric  --------------------------------------------------------------
#recursive function

test_that("is_numeric", {
  expect_true(is_numeric(1:3))
  expect_true(!is_numeric("123"))
  expect_true(!is_numeric(iris))
  expect_true(is_numeric(iris[-5]))
  expect_true(!is_numeric(iris[-5], F))
  expect_true(!is_numeric(T))
  expect_true(is_numeric(array(1:8, dim = rep(2, 3))))
  expect_true(is_numeric(list(1, 2, 3)))
  expect_true(!is_numeric(list(1, 2, 3), recursive = F))
  expect_true(is_numeric(list(list(1, 2, 3), 5, list(1, 2, 3), 4, list(list(10)))))

})

#df variant
test_that("is_numeric_by_col", {
            expect_equivalent(is_numeric_by_col(iris), c(T, T, T, T, F))
})

