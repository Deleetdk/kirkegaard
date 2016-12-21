### Misc tests

context("misc")

# find_duplicates ---------------------------------------------------------

test_that("find_duplicates",
          expect_equal(find_duplicates(c(1, 1, 2, 2, 3, 4, 5, 5)), structure(list(`1` = 1:2, `2` = 3:4, `5` = 7:8), .Names = c("1", "2", "5"))))


