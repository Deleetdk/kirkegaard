### Misc tests

context("misc")



# NA_to_F -----------------------------------------------------------------

test_that("NA_to_F", {
  x = c(T, F, NA, T)
  expect_equivalent(NA_to_F(x), c(T, F, F, T))
})

# restore_NAs -------------------------------------------------------------

test_that("restore_NAs", {
  expect_equivalent(restore_NAs(c(1, 3, 5), c()), c(1, 3, 5))
  expect_equivalent(restore_NAs(c(1, 3, 5), 4:10), c(1, 3, 5, rep(NA, 7)))
  expect_equivalent(restore_NAs(c(1, 3, 5), 1:5), c(rep(NA, 5), 1, 3, 5))
  expect_equivalent(restore_NAs(c(1, 3, 5), c(2, 4, 6), 6), c(1, NA, 3, NA, 5, NA))
  expect_equivalent(restore_NAs(c(1, 3, 5), c(2, 4, 6)), c(1, NA, 3, NA, 5, NA))
})


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


# table2 ------------------------------------------------------------------
set.seed(1)
some_letters = sample(letters[1:10], size = 100, replace = T)
base_table = table(some_letters)

test_that("table2", {
  #sorting
  expect_equivalent(table2(some_letters, include_NA = F) %>% `[[`(3), sort(base_table, decreasing = T) %>% as.vector)
  expect_equivalent(table2(some_letters, include_NA = F, sort_descending = F) %>% `[[`(3), sort(base_table, decreasing = F) %>% as.vector)
  expect_equivalent(table2(some_letters, include_NA = F, sort_descending = NULL) %>% `[[`(3), base_table %>% as.vector)

  #with NA
  expect_equivalent(table2(some_letters) %>% nrow, 11)

  #proportion
  expect_equivalent(table2(some_letters, prop = T) %>% names %>% `[`(3), "Proportion")
})



# seq_along_rows ----------------------------------------------------------

test_that("seq_along_rows", {
  #normal
  expect_equivalent(seq_along_rows(iris), 1:150)
  expect_equivalent(seq_along_rows(iris[-c(1:150), ]), integer())

  #errors
  expect_error(seq_along_rows(NULL))
  expect_error(seq_along_rows(NA))
})


# get_dims total_cells --------------------------------------------------------------
#a better version of dim() from base-r

test_that("get_dims", {
  #normal stuff
  expect_equivalent(get_dims(1:2), 2)
  expect_equivalent(get_dims(list(1, 2)), 2)
  expect_equivalent(get_dims(matrix(1:4, nrow=2)), c(2, 2))
  expect_equivalent(matrix(1:4, nrow=2) %>% as.data.frame %>% get_dims, c(2, 2))
  expect_equivalent(array(1:16, dim = c(2, 2, 2)) %>% dim, c(2, 2, 2))
})


# total cells -------------------------------------------------------------

test_that("total_cells", {
  #easy stuff
  expect_equivalent(total_cells(iris), 750)
  expect_equivalent(total_cells(1:3), 3)
  expect_equivalent(total_cells(array(1:27, dim = c(3, 3, 3))), 27)

  #questionable input
  expect_equivalent(total_cells(NULL), 0)
})



# uniq_encoding -----------------------------------------------------------

test_that("uniq_encoding", {
  #simple tests
  expect_identical(c(1, 2, 2, 3, 3, 3),
                   c(1, 2, 2, 3, 3, 3) %>% uniq_encoding() %>% rev_uniq_encoding())
  expect_identical(c(3, 1, 3, 2, 3, 2),
                   c(3, 1, 3, 2, 3, 2) %>% uniq_encoding() %>% rev_uniq_encoding())

  #with NAs
  expect_identical(c(NA, 1, 2, 2, 3, 3, 3, NA),
                   c(NA, 1, 2, 2, 3, 3, 3, NA) %>% uniq_encoding() %>% rev_uniq_encoding())

  #empty
  expect_identical(c(),
                   c() %>% uniq_encoding() %>% rev_uniq_encoding())

  #classes
  expect_identical(c(1, 2, 2, 3, 3, 3) %>% as.character(),
                   c(1, 2, 2, 3, 3, 3) %>% as.character() %>% uniq_encoding() %>% rev_uniq_encoding())
  expect_identical(c(1, 2, 2, 3, 3, 3) %>% as.character(),
                   c(1, 2, 2, 3, 3, 3) %>% as.factor() %>% {suppressWarnings(uniq_encoding(.))} %>% rev_uniq_encoding())
  expect_identical(c(1, 2, 2, 3, 3, 3) %>% as.character(),
                   c(1, 2, 2, 3, 3, 3) %>% as.ordered() %>% {suppressWarnings(uniq_encoding(.))} %>% rev_uniq_encoding())
  expect_identical(c(1, 2, 2, 3, 3, 3) %>% as.integer(),
                   c(1, 2, 2, 3, 3, 3) %>% as.integer() %>% uniq_encoding() %>% rev_uniq_encoding())

  #warning
  expect_warning(c(1, 2, 2, 3, 3, 3) %>% as.factor() %>% uniq_encoding())
})




# count_decimals ----------------------------------------------------------

test_that("count_decimals", {
  #basics
  expect_identical(c(1, 1.1, 1.12, 1.123, 1.1234, 1.1, 1.10, 1.100, 1.1000) %>% count_decimals(),
                   c(0, 1, 2, 3, 4, 1, 1, 1, 1))
  expect_identical(seq(0, 1000, by = 100) %>% count_decimals(),
                   rep(0, 11))

  #negatives
  expect_identical(c(100.1234, -100.1234) %>% count_decimals(),
                   c(4, 4))

  #NULL or zero length vector
  expect_identical(NULL %>% count_decimals(),
                   numeric())
  expect_identical(c() %>% count_decimals(),
                   numeric())

})



# inv_table ---------------------------------------------------------------

test_that("inv_table", {
  #row counts should match before and after back and forth
  expect_equivalent(nrow(mpg), inv_table(table(mpg$cyl, mpg$manufacturer, mpg$class)) %>% nrow())
})
