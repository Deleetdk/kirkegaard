context("miss_")


# missing data ------------------------------------------------------------
set.seed(1)
test_data = miss_add_random(iris)

# miss_plot ---------------------------------------------------------------

test_that("miss_plot", {
  expect_is(miss_plot(test_data), "ggplot")
  expect_is(miss_plot(test_data, percent = F), "ggplot")
  expect_is(miss_plot(test_data, case = F), "ggplot")
  expect_is(miss_plot(test_data, case = F, percent = F), "ggplot")
  expect_is(miss_plot(test_data, reverse = T), "ggplot")
})

# count_NA ---------------------------------------------------

test_that("count_NA", {
  expect_true(count_NA(c(1:10, rep(NA, 5), 1:10)) == 5)
  expect_true(count_NA(c(1:10, rep(NA, 5), 1:10), reverse = T) == 20)
})


# miss_filter ------------------------------------------------
#filters data by number of missing values per case

df = data.frame(1:10, letters[1:10])
set.seed(1)
df = miss_add_random(df)

test_that("miss_filter", {
  expect_true(miss_filter(df) %>% nrow %>% equals(8))
})



# miss_analyze --------------------------------------------------
#large dataset with missing data
set.seed(1)
t2 = rnorm(10e3) %>% matrix(nrow = 1000) %>% as.data.frame() %>% miss_add_random() %>% miss_analyze()

test_that("miss_analyze", {
  expect_true(all(get_dims(t2) == c(10, 10)))
  expect_true((t2 < .8) %>% sum(na.rm = T) == 90)
})


# miss_impute -------------------------------------------------------------
set.seed(1)

#test the dealing with ordinals with 2 levels
iris_with_ord2 = iris %>% dplyr::filter(Species %in% c("setosa", "versicolor")) %>% dplyr::mutate(Species = ordered(Species))

test_that("miss_impute", {
  #ordinary
  expect_is(iris %>% miss_add_random() %>% miss_impute(), class = "data.frame")

  #ordinal with 2 levels
  expect_warning(iris_with_ord2 %>% miss_add_random() %>%  miss_impute())
})


# miss_amount -------------------------------------------------------------
set.seed(1)

test_that("miss_amount", {
  expect_equivalent(miss_amount(iris %>% miss_add_random()) %>% unname, c(.42, 1, .10))
})

