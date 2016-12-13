### tests for error related functions

context("error-handling")



# is_error ----------------------------------------------------------------

#an error
trial_error = try(log("k"), silent = T)

test_that("is_error",
          expect_true(is_error(trial_error))
          )


# throws_error ------------------------------------------------------------

test_that("throws_error", {
  expect_false(throws_error(log(1)))
  expect_true(throws_error(log("")))
}
)

