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
  expect_false(throws_error("log(1)"))
  expect_true(throws_error("log('')"))
}
)


# fail_if -----------------------------------------------------------------

test_that("fail_if", {
  expect_error(fail_if(T))
  expect_error(fail_if(1, extended = T))
  expect_equivalent(fail_if(1), 1)
})


# fail_if_NA --------------------------------------------------------------

test_that("fail_if_NA", {
  expect_error(fail_if_NA(NA))
  expect_error(fail_if_NA(matrix(NA, rep(1, 8))))
  expect_equivalent(fail_if_NA(1:3), 1:3)
})

# browser -----------------------------------------------------------------

#TODO: figure out how to make tests for browser() related functions
#browse_if
#browse_if_equals
#try_browse


# try_else ----------------------------------------------------------------
#better version of fail_with

test_that("try_else", {
  expect_equivalent(try_else(log(1)), 0)
  expect_equivalent(try_else(log("abc")), NULL)
  expect_equivalent(try_else(log("abc"), else. = 5), 5)
  #expect_output(try_else(log("abc"), silent = F)) #does not work for some reason
})
