### tests for df_ functions

context("df_")

# df_standardize ------------------------------------------------------------------
set.seed(1)
l_dfs = list(t1 = df_standardize(iris, messages = F),
             t2 = df_standardize(iris, exclude_factors = F, messages = F),
             t3 = df_standardize(iris, w = runif(150), messages = F)
             )

test_that("df_standardize", {
  #not equal
  expect_true(all(head(l_dfs$t1[-5]) != head(iris)[-5]))
  expect_true(all(head(l_dfs$t2) != head(iris)))
  expect_true(all(l_dfs$t1[-5] != l_dfs$t3[-5]))

  #numeric
  expect_equivalent(map_dbl(l_dfs$t2, sd), rep(1, 5))
  expect_false(are_equal(map_dbl(l_dfs$t3[-5], sd), rep(1, 4), check.attributes = F))
})

# df_round ----------------------------------------------------------------

test_that("df_round", {
  expect_equivalent(df_round(iris[-5]), iris[-5] %>% map_df(round, 2))
})

# df_as_num ---------------------------------------------------------------
#converts string vectors in a data.frame to numeric ones if possible
#make iris into a df with strings
iris_chr = map_df(iris, as.character)

test_that("df_as_num", {
  expect_equivalent(map_chr(df_as_num(iris_chr), class),
                    c("numeric", "numeric", "numeric", "numeric", "character"))
  expect_equivalent(map_chr(df_as_num(iris_chr, stringsAsFactors = T), class),
                    c("numeric", "numeric", "numeric", "numeric", "factor"))
  expect_equivalent(map_chr(df_as_num(iris, always_skip_factors = F, smart_factor_conversion = F), class),
                    c("numeric", "numeric", "numeric", "numeric", "numeric"))
  expect_equivalent(map_chr(df_as_num(iris_chr, stringsAsFactors = T), class),
                    c("numeric", "numeric", "numeric", "numeric", "factor"))

})


# df_add_delta ------------------------------------------------------------
#this function adds delta (difference) variables to a df, in a semi-intelligent fashion

#these should work
l_deltas = silence(
  list(df_add_delta(iris, primary_var = 1),
       df_add_delta(iris, primary_var = "Sepal.Length"),
       df_add_delta(iris, primary_var = pi),
       df_add_delta(iris, primary_var = 1, standardize = T)
      )
)

#errors
l_deltas_errors = list(
  try({df_add_delta(iris, primary_var = 1, secondary_vars = 1:4)}, T),
  try({df_add_delta(iris, primary_var = 0)}, T),
  try({df_add_delta(iris, primary_var = 1, secondary_vars = 99)}, T),
  try({df_add_delta(iris, primary_var = 1, secondary_vars = -1:1)}, T)
)

test_that("df_add_delta", {
  expect_equal(map_chr(l_deltas, class), rep("data.frame", length(l_deltas)))
  expect_equal(map_chr(l_deltas_errors, class), rep("try-error", length(l_deltas)))
})


# df_rowFunc ------------------------------------------------------------------------
#

#tests
l_rowfuncs = list(
  df_rowFunc(iris[1:4], progress = "none"),
  df_rowFunc(iris[1], iris[2], iris[3], iris[4], progress = "none"),
  df_rowFunc(iris[1:4], func = median, progress = "none"),
  df_rowFunc(iris[1:4], standardize = T, progress = "none"),
  df_rowFunc(iris[1:4], standardize = T, func = median, progress = "none")
)

#errors
l_rowfuncs_errors = list(
  try({df_rowFunc(iris)}, T)
)

test_that("df_rowFunc", {
  expect_equal(map_chr(l_rowfuncs, class), rep("numeric", length(l_rowfuncs)))
  expect_equal(map_chr(l_rowfuncs_errors, class), rep("try-error", length(l_rowfuncs_errors)))
})


# df_sort ---------------------------------------------------------------
#worse version of dplyr::arrange
#but keepts rownames

test_that("df_sort", {
  expect_true(!are_equal(df_sort(iris, "Sepal.Length"), df_sort(iris, "Sepal.Length", decreasing = T)))
  expect_true(!are_equal(df_sort(iris, "Sepal.Length"), df_sort(iris, 2)))
  expect_true(!are_equal(df_sort(iris, "Sepal.Length"), df_sort(iris, 4)))
})


# df_remove_NA_vars -------------------------------------------------------

test_that("df_remove_NA_vars", {
  expect_equivalent(df_remove_NA_vars(data.frame(a = NA, b = 1)), data.frame(b = 1))
  expect_equivalent(df_remove_NA_vars(data.frame(a = NA, b = 1), keep = "a"), data.frame(a = NA, b = 1))
  expect_equivalent(df_remove_NA_vars(data.frame(a = NA, b = 1), keep = 1), data.frame(a = NA, b = 1))
})
