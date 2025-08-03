### tests for df_ functions

context("df_")

# df_standardize ------------------------------------------------------------------
set.seed(1)

#special iris
testdata = tibble(
  normals = rnorm(100),
  normals_with_missing = rnorm(100) %>% {y = .; y[1] = NA; y},
  zero_one = runif(100, min = 0, max = 1),
  logicals = sample(c(T, F), size = 100, replace = T),
  ordinals = sample(letters[1:3], size = 100, replace = T) %>% ordered(levels = letters[1:3]),
  factors = sample(letters[1:3], size = 100, replace = T) %>% factor()
)


test_that("df_standardize", {
  l_dfs = list(
    #default
    t1 = df_standardize(testdata, messages = F),
    #weights
    t2 = df_standardize(testdata, w = runif(100), messages = F),
    #convert everything
    t3 = df_standardize(testdata, exclude_logicals = F, exclude_range_01 = F, messages = F),
    #specific exclusion, convert nothing
    t4 = df_standardize(testdata, exclude = names(testdata), messages = F)
  )

  #not equal
  expect_true(!are_equal(testdata, l_dfs$t1, check.attributes = F))
  expect_true(!are_equal(testdata, l_dfs$t2, check.attributes = F))
  expect_true(!are_equal(testdata, l_dfs$t3, check.attributes = F))

  #weights matter
  expect_true(!are_equal(l_dfs$t1, l_dfs$t2, check.attributes = F))

  #equal when we ignore all variables
  expect_equivalent(testdata, l_dfs$t4)

  #they get standardized right
  expect_equivalent(map_dbl(l_dfs$t3[1:4], sd, na.rm = T), rep(1, 4))

  #handle NAs

})

# df_round ----------------------------------------------------------------
test_that("df_round", {
  expect_equivalent(iris %>% head(1) %>% df_round,
                    data.frame(Sepal.Length = "5.10",
                               Sepal.Width = "3.50",
                               Petal.Length = "1.40",
                               Petal.Width = "0.20",
                               Species = "setosa",
                               stringsAsFactors = F)
                    )

  expect_equivalent(iris %>% head(1) %>% df_round(digits = 0),
                    data.frame(Sepal.Length = "5",
                               Sepal.Width = "4",
                               Petal.Length = "1",
                               Petal.Width = "0",
                               Species = "setosa",
                               stringsAsFactors = F)
                    )

  expect_equivalent(iris %>% .[2, ] %>% df_round(digits = 1, simple = T),
                    data.frame(Sepal.Length = 4.9,
                               Sepal.Width = 3.0,
                               Petal.Length = 1.4,
                               Petal.Width = 0.2,
                               Species = iris$Species[2])
  )


})

# df_as_num ---------------------------------------------------------------
#converts string vectors in a data.frame to numeric ones if possible


test_that("df_as_num", {
  #make iris into a df with strings
  iris_chr = map_df(iris, as.character)

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



test_that("df_add_delta", {
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

  expect_equal(map_chr(l_deltas, class), rep("data.frame", length(l_deltas)))
  expect_equal(map_chr(l_deltas_errors, class), rep("try-error", length(l_deltas)))
})




# df_t ---------------------------------------------------------------

test_that("df_t", {
  expect_equivalent(df_t(iris), iris %>% t %>% as.data.frame %>% set_colnames(rownames(iris)) %>% set_rownames(colnames(iris)))
})



# df_merge_cols -----------------------------------------------------------


test_that("df_merge_cols", {
  expect_equivalent(tibble(
    a = c(1, NA, NA),
    b = c(-1, 2, NA),
    c = c(-5, -9, 3)
  ) %>% df_merge_cols(letters[1:3]), 1:3)
})


# df_merge_rows -----------------------------------------------------------
#performs row-wise merging



test_that("df_merge_rows", {
  #some objects
  t = data.frame(id = c("a", "a", "b", "b", "c"), value = 1:5)
  t_true = data.frame(id = c("a", "b", "c"), value = c(3, 7, 5))
  t_true2 = data.frame(id = c("a", "b", "c"), value = c(1.5, 3.5, 5))

  t1 = data.frame(X = c(1, 2, 3, NA), Y = c(1, 2, NA, 3));rownames(t1) = LETTERS[1:4]
  t1_cor = data.frame(X = c(1, 2, 3), Y = c(1, 2, 3));rownames(t1_cor) = LETTERS[1:3]

  ##key column
  #basic
  expect_equivalent(df_merge_rows(t, "id"), t_true)
  expect_equivalent(df_merge_rows(t, "id", func = mean), t_true2)

  #error due to non-numeric
  expect_error(df_merge_rows(t, 'id', numeric = FALSE))

  ##rownames
  #basic
  expect_true(all(silence(df_merge_rows(data = t1, names = c("C", "D")) == t1_cor)))
  expect_equivalent(df_merge_rows(t1, names = c("C", "D"), new_name = "D") %>% rownames, c("A", "B", "D"))

  #no new name given
  expect_message(df_merge_rows(data = t1, names = c("C", "D")) == t1_cor)
})



# df_no_list_cols ---------------------------------------------------------

test_that("df_no_list_cols", {
  iris2 = iris
  iris2$list = list(1)
  iris2$df = list(iris)

  expect_equivalent(datasets::iris, df_no_list_cols(iris2))
})


# df_legalize_names -------------------------------------------------------


test_that("df_legalize_names", {
  #with spaces
  iris_bad = iris
  names(iris_bad) = str_replace(names(iris_bad), "\\.", " ") #replace with spaces

  #duplicated empty
  iris_bad2 = iris
  names(iris_bad2)[1] = "" #empty name
  names(iris_bad2)[2] = "" #another

  #duplicated NAs
  iris_bad3 = iris
  names(iris_bad3)[1] = NA #NA
  names(iris_bad3)[2] = NA #NA

  #duplicated normal
  iris_bad4 = iris
  names(iris_bad4)[1] = "x"
  names(iris_bad4)[2] = "x"

  #spaces
  expect_equivalent(df_legalize_names(iris_bad) %>% names(), names(iris) %>% str_replace("\\.", "_"))

  #empty
  expect_false(df_legalize_names(iris_bad2) %>% names() %>% str_detect("^$") %>% any())

  #NA
  expect_false(df_legalize_names(iris_bad3) %>% names() %>% {any(is.na(.))})

  #duplicated
  expect_false(df_legalize_names(iris_bad4) %>% names() %>% {any(duplicated(.))})
})


# df_var_table ------------------------------------------------------------


test_that("df_var_table", {
  #read some SPSS data
  spss_data = haven::read_sav(system.file("extdata", "survey.sav", package = "kirkegaard"))
  spss_data_table = df_var_table(spss_data)

  #we got a data frame?
  expect_s3_class(spss_data_table, "data.frame")

  #test specific data features
  expect_true(any(spss_data_table$miss_prop > 0))
  expect_true(any(spss_data_table$classes == "numeric"))
  expect_true(any(is.na(spss_data_table$label)))
})



# df_winsorise ------------------------------------------------------------

test_that("df_winsorrise", {
  #add extreme outlier
  iris2 = iris
  iris2[1, 1] = 100

  expect_lt(df_winsorise(iris2, z = 2)[1, 1], 100)
  expect_lt(df_winsorise(iris2, rz = 2)[1, 1], 100)
  expect_lt(df_winsorise(iris2, centile = .99)[1, 1], 100)

  #robust to NA
  iris2[2, 1] = NA
  expect_lt(df_winsorise(iris2, z = 2)[1, 1], 100)
  expect_lt(df_winsorise(iris2, rz = 2)[1, 1], 100)
  expect_lt(df_winsorise(iris2, centile = .99)[1, 1], 100)

  #varied data input
  expect_s3_class(df_winsorise(mpg, centile = .99), "data.frame")

})

# df_add_affix ------------------------------------------------------------

test_that("df_add_affix", {
  expect_equivalent(
    df_add_affix(iris, prefix = "P_") |> names(),
    c("P_Sepal.Length", "P_Sepal.Width", "P_Petal.Length", "P_Petal.Width",
      "P_Species")
  )

  expect_equivalent(
    df_add_affix(iris, suffix = "_S") |> names(),
    df_add_affix(iris, suffix = "_S") |> names()
  )


  expect_equivalent(
    df_add_affix(iris, suffix = "_X", exclude = "Species") |> names(),
    c("Sepal.Length_X", "Sepal.Width_X", "Petal.Length_X", "Petal.Width_X",
      "Species")
  )

})
