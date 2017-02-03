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


