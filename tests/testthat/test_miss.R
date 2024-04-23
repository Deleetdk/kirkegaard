context("miss_")




test_that("miss_plot", {
  set.seed(1)
  test_data = miss_add_random(iris)

  expect_is(miss_plot(test_data), "ggplot")
  expect_is(miss_plot(test_data, percent = F), "ggplot")
  expect_is(miss_plot(test_data, case = F), "ggplot")
  expect_is(miss_plot(test_data, case = F, percent = F), "ggplot")
  expect_is(miss_plot(test_data, reverse = T), "ggplot")
})



test_that("miss_count", {
  expect_true(miss_count(c(1:10, rep(NA, 5), 1:10)) == 5)
  expect_true(miss_count(c(1:10, rep(NA, 5), 1:10), reverse = T) == 20)
  expect_true(miss_count(c(1:10, rep(NA, 5), 1:10), reverse = T, prop = T) == 0.8)
})



#filters data by number of missing values per case

test_that("miss_filter", {
  set.seed(1)
  df = tibble(ints = 1:10, letters = letters[1:10], unif = runif(10), norm = rnorm(10))
  df = miss_add_random(df, prop = .25) %>% mutate(nomiss = T, allmiss = NA)
  df

  #no missing allow
  expect_equivalent(miss_filter(df) %>% nrow(), 0)
  #allow 1 missing
  expect_equivalent(miss_filter(df, missing = 1) %>% nrow(), 3)
  #allow 40% missing
  expect_equivalent(miss_filter(df, missing = .4) %>% nrow(), 7)
  #reverse count, at least 3 non-missing values
  expect_equivalent(miss_filter(df, missing = 3, reverse = T) %>% nrow(), 3)
  #reverse fraction
  expect_equivalent(miss_filter(df, missing = .6, reverse = T) %>% nrow(), 7)
  #only count specific variables
  expect_equivalent(miss_filter(df, missing = 0, vars = c("ints", "letters")) %>% nrow(), 5)
  #subset variables instead
  expect_equivalent(miss_filter(df, missing = 0, by_case = F) %>% ncol(), 1)
  #at most 2 missing values by column
  expect_equivalent(miss_filter(df, missing = 2, by_case = F) %>% ncol(), 3)
  #at most 20% missing by column
  expect_equivalent(miss_filter(df, missing = .2, by_case = F) %>% ncol(), 3)
  #expect warning for unused argument
  expect_warning(miss_filter(df, missing = 0, by_case = F, vars = "ints") %>% ncol(), regexp = "will not affect results")
})




#large dataset with missing data


test_that("miss_analyze", {
  set.seed(1)
  t2 = rnorm(10e3) %>% matrix(nrow = 1000) %>% as.data.frame() %>% miss_add_random() %>% miss_analyze()

  expect_true(all(get_dims(t2) == c(10, 10)))
  expect_true((t2 < .8) %>% sum(na.rm = T) == 90)
  expect_equivalent(diag(as.matrix(t2)), rep(NA_real_, 10))
})





test_that("miss_impute", {
  set.seed(1)

  #test the dealing with ordinals with 2 levels
  iris_with_ord2 = iris %>% dplyr::filter(Species %in% c("setosa", "versicolor")) %>% dplyr::mutate(Species = ordered(Species))

  #rownames preserve
  df = data.frame(a = 1:5, b = rnorm(5), c = c(1, NA, NA, 1, 4)) %>% set_rownames(letters[1:5])
  df2 = data.frame(a = 1:5, b = rnorm(5), c = c(1, NA, NA, 1, 4)) %>% set_rownames(letters[5:1])
  df3 = data.frame(a = 1:5, b = rnorm(5), c = c(1, NA, NA, 1, 4)) %>% set_rownames(letters[1:5])

  #ordinary
  expect_is(iris %>% miss_add_random() %>% miss_impute(), class = "data.frame")

  #ordinal with 2 levels
  #buggy function won't shut up
  rf_imputed = iris_with_ord2 %>% miss_add_random() %>% {
    sink("/dev/null")
    y = miss_impute(., method = "rf")
    sink()
    y
  }
  expect_true(rf_imputed %>% miss_count() %>% magrittr::equals(0))
  #does not work
  expect_warning(iris_with_ord2 %>% miss_add_random() %>% miss_impute(method = "irmi"))

  #preserve rownames
  expect_equivalent(rownames(miss_impute(df)), letters[1:5])
  expect_equivalent(rownames(miss_impute(df, method = "irmi")), letters[1:5])
  expect_equivalent(rownames(miss_impute(df2)), letters[5:1])
  expect_equivalent(rownames(miss_impute(df3, max_na = Inf)), letters[1:5])

  #preserve tibble status
  iris %>%
    miss_add_random() %>%
    as_tibble() %>%
    miss_impute() %>%
    expect_is("tbl_df")
})




test_that("miss_amount", {
  set.seed(1)

  expect_equivalent(miss_amount(iris %>% miss_add_random()) %>% unname(), c(.40, 1, .10), tolerance = .02)
})




test_that("miss_amount", {
  set.seed(1)
  test_data = miss_add_random(iris)

  expect_s3_class(test_data %>% miss_by_group("Species"), "data.frame")

  expect_error(test_data %>% miss_by_group("abc"), "`grouping_vars`")
  expect_error(test_data %>% miss_by_group("Species", "abc"), "`vars`")
})




test_that("miss_fill", {
  #ok input
  expect_identical(miss_fill(c(1, NA, NA), c(9, 2, NA), c(9, 9, 3)), c(1, 2, 3))
  expect_identical(list(c(1, NA, NA), c(9, 2, NA), c(9, 9, 3)) %>% miss_fill(), c(1, 2, 3))
  expect_identical(data.frame(
    x = c(1, NA, NA),
    y = c(9, 2, NA),
    z = c(9, 9, 3)
  ) %>% miss_fill(), c(1, 2, 3))

  #special types
  expect_true(miss_fill(as.Date(c("2000-01-01", NA)),
                        as.Date(c(NA, "2001-01-01"))) %>% {class(.) == "Date"})

  #factor issue
  expect_identical(miss_fill(c("a", NA, "c"), c("x", "b", "x")), c("a", "b", "c"))

  #errors
  expect_error(list(1:3, 1) %>% miss_fill(), regexp = "vectors")
  expect_error(miss_fill(mean), regexp = "Bad input")
})




test_that("miss_locf", {
  expect_identical(c(NA, 1, 1, 2, 2),
                   c(NA, 1, NA, 2, NA) %>% miss_locf())

  #longer series of NAs
  expect_identical(c(NA, 1, 1, 2, 2, 2, 2),
                   c(NA, 1, NA, 2, NA, NA, NA) %>% miss_locf())

  #reverse
  expect_identical(c(1, 1, 2, 2, NA),
                   c(NA, 1, NA, 2, NA) %>% miss_locf(reverse = T))
})





test_that("miss_add_random", {
  #check that data types don't change
  iris_with_random = iris %>% miss_add_random()
  iris_with_random_tibble = iris %>% as_tibble() %>% miss_add_random()

  expect_is(iris_with_random, "data.frame")
  expect_is(iris_with_random_tibble, "data.frame")
  expect_is(iris_with_random_tibble, "tbl_df")
})


test_that("miss_combine_duplicate_vars", {
  d1 = tibble(
    id = 1:3,
    y = c(1, 2, 3),
    x = c(1, NA, NA)
  )

  d2 = tibble(
    id = 1:3,
    x = c(NA, 2, NA),
    z = c(1, 2, 3)
  )

  d3 = tibble(
    id = 1:3,
    x = c(NA, NA, 3),
    a = letters[1:3]
  )

  d_expected = tibble(
    id = 1:3,
    x = c(1, 2, 3),
    y = c(1, 2, 3),
    z = c(1, 2, 3),
    a = letters[1:3]
  )

  d = d1 %>%
    left_join(d2, by = "id") %>%
    miss_combine_duplicate_vars() %>%
    left_join(d3, by = "id") %>%
    miss_combine_duplicate_vars() %>%
    select(id, x, y, z, a)

  expect_equivalent(d, d_expected)

  #alternative order, shouldnt affect things
  d = d3 %>%
    left_join(d1, by = "id") %>%
    miss_combine_duplicate_vars() %>%
    left_join(d2, by = "id") %>%
    miss_combine_duplicate_vars() %>%
    select(id, x, y, z, a)

  expect_equivalent(d, d_expected)

})
