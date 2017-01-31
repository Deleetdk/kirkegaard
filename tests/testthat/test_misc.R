### Misc tests

context("misc")

# find_duplicates ---------------------------------------------------------

test_that("find_duplicates",
          expect_equal(find_duplicates(c(1, 1, 2, 2, 3, 4, 5, 5)), structure(list(`1` = 1:2, `2` = 3:4, `5` = 7:8), .Names = c("1", "2", "5"))))


# last_value --------------------------------------------------------------

test_that("last_value", {
  expect_equivalent(last_value(1:3), 3)
  expect_equivalent(last_value(c(1:3, NA)), 3)
  expect_true(is.na(last_value(c(1:3, NA), na.rm=F)))
  expect_true(is.na(last_value(rep(NA, 3))))

})
