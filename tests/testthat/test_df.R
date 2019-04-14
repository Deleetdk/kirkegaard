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

  expect_equivalent(iris %>% `[`(2, ) %>% df_round(digits = 1, simple = T),
                    data.frame(Sepal.Length = 4.9,
                               Sepal.Width = 3.0,
                               Petal.Length = 1.4,
                               Petal.Width = 0.2,
                               Species = "setosa")
  )


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


# df_t ---------------------------------------------------------------

test_that("df_t", {
  expect_equivalent(df_t(iris), iris %>% t %>% as.data.frame %>% set_colnames(rownames(iris)) %>% set_rownames(colnames(iris)))
})


# df_colFunc --------------------------------------------------------------
#apply functions selectively to columns of a data.frame
t1 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Length"))
#using inverse regex
t2 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Species", pattern_inverse = T))
#using integer indices
t3 = head(df_colFunc(iris, func = function(x) return(x * 2), indices = 2:3))
#using logical indices
t4 = head(df_colFunc(iris, func = function(x) return(x * 2), indices = c(T, F, T, F, F)))
#removing unselected columns
t5 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Length", keep_unselected = F))
#select all by not providing any selector
t6 = head(df_colFunc(iris, func = as.character)) #all have been changed to chr

test_that("df_colFunc", {
  expect_equivalent(t1[1, 1], iris[1, 1] * 2)
  expect_true(!t1[1, 2] == iris[1, 1] * 2)
  expect_true(t2[1, 1] == iris[1, 1] * 2)
  expect_true(t2[1, 2] == iris[1, 2] * 2)
  expect_true(!t3[1, 1] == iris[1, 1] * 2)
  expect_true(t3[1, 2] == iris[1, 2] * 2)
  expect_true(t4[1, 1] == iris[1, 1] * 2)
  expect_true(!t4[1, 2] == iris[1, 2] * 2)
  expect_true(ncol(t5) == 2)
  expect_true(t5[1, 1] == iris[1, 1] * 2)
  expect_true(purrr::map_lgl(t6, is.character) %>% all)
})

# df_subset_by_pattern -------------------------------------------------------
#subset using regex for column names


test_that("df_subset_by_pattern", {
  # length columns
  expect_equivalent(df_subset_by_pattern(iris, "Length"), iris[c(1, 3)])

  # non-length columns
  expect_equivalent(df_subset_by_pattern(iris, "Length", T), iris[-c(1, 3)])

  # species, 1 col
  expect_equivalent(df_subset_by_pattern(iris, "Species"), iris[5])
})


# df_add_id ------------------------------------------------------------------
x = iris; x["ID"] = "A"

test_that("df_add_id", {
  expect_equivalent(df_add_id(iris, "A"), x)
})


# df_rename -----------------------------------------------------

test_that("df_rename", {
  expect_equivalent(names(df_rename(iris, "Species", "SPECIES")),
                    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "SPECIES"))
  expect_equivalent(names(df_rename(iris, c("Sepal.Length", "Species"), c("SEPAL_LENGTH", "SPECIES"))),
                    c("SEPAL_LENGTH", "Sepal.Width", "Petal.Length", "Petal.Width", "SPECIES"))
})


# df_remove ----------------------------------------------------------

test_that("df_remove", {
  #rm one
  expect_equivalent(df_remove(iris, "Species"), iris[-5])

  #two
  expect_equivalent(names(df_remove(iris, c("Sepal.Length", "Species"))), names(iris[-c(1, 5)]))

  #all
  expect_equivalent(df_remove(iris, names(iris)), iris[-c(1:5)])

  #attempt remove same twice
  expect_warning(df_remove(iris, c("Species", "Species")))
})


# df_residualize ----------------------------------------------------------

set.seed(1)
df = data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5))
weightsvar = runif(5)

test_that("df_residualize", {
  #test basic function
  expect_true(all(df_residualize(df, resid.vars = "c", print.models = F) %>% `[`(c("a", "b")) != df[c("a", "b")]))

  #test suffix + message off
  expect_true(all(df_residualize(df, resid.vars = "c", suffix = "_r", print.models = F) %>% colnames() != colnames(df)))

  #test exclusion vector
  expect_equivalent(df_residualize(df, resid.vars = "c", exclude_vars = "b", print.models = F) %>% `[`("b"), df["b"])

  #test return
  expect_true(all(df_residualize(df, resid.vars = "c", return.resid.vars = F, print.models = F) %>% colnames != "c"))

  #with weights
  expect_true(all(df_residualize(df, resid.vars = "c", weights = weightsvar, print.models = F) %>% get_dims() == c(5, 3)))
})



# df_merge_cols -----------------------------------------------------------


test_that("df_merge_cols", {
  expect_equivalent(tibble(
    a = c(1, NA, NA),
    b = c(-1, 2, NA),
    c = c(-5, -9, 3)
  ) %>% df_merge_cols(letters[1:3]), 1:3)
})


# df_fct_split -----------------------------------------------------------

test_that("df_fct_split", {
  #basic uses
  expect_equivalent(names(df_fct_split(iris, "Species")), c(names(iris), levels(iris$Species)))
  expect_equivalent(names(df_fct_split(iris, "Species", rm_old = T)), c(names(iris)[-5], levels(iris$Species)))
  expect_equivalent(names(df_fct_split(iris, "Species", prefix = "%v_")), c(names(iris), "Species_" + levels(iris$Species)))
  expect_equivalent(names(df_fct_split(iris, "Species", prefix = "%v_")), c(names(iris), "Species_" + levels(iris$Species)))

  #type output
  expect_equivalent(purrr::map_chr(df_fct_split(iris, "Species"), class), c(rep("numeric", 4), "factor", rep("logical", 3)))
  expect_equivalent(purrr::map_chr(df_fct_split(iris, "Species", type = "f"), class), c(rep("numeric", 4), "factor", rep("factor", 3)))
  expect_equivalent(purrr::map_chr(df_fct_split(iris, "Species", type = "n"), class), c(rep("numeric", 4), "factor", rep("numeric", 3)))
  expect_equivalent(purrr::map_chr(df_fct_split(iris, "Species", type = "i"), class), c(rep("numeric", 4), "factor", rep("integer", 3)))

  #errors
  expect_error(df_fct_split(iris, "Sepal.Length"))
  expect_error(df_fct_split(iris, "wrong"))
})


# df_merge_rows -----------------------------------------------------------
#performs row-wise merging

#some objects
t = data.frame(id = c("a", "a", "b", "b", "c"), value = 1:5)
t_true = data.frame(id = c("a", "b", "c"), value = c(3, 7, 5))
t_true2 = data.frame(id = c("a", "b", "c"), value = c(1.5, 3.5, 5))

t1 = data.frame(X = c(1, 2, 3, NA), Y = c(1, 2, NA, 3));rownames(t1) = LETTERS[1:4]
t1_cor = data.frame(X = c(1, 2, 3), Y = c(1, 2, 3));rownames(t1_cor) = LETTERS[1:3]

test_that("df_merge_rows", {
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

iris2 = iris
iris2$list = list(1)
iris2$df = list(iris)

test_that("df_no_list_cols", {
  expect_equivalent(datasets::iris, df_no_list_cols(iris2))
})


# df_legalize_names -------------------------------------------------------
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

test_that("df_legalize_names", {
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
#read some SPSS data

spss_data = haven::read_sav(system.file("extdata", "survey.sav", package = "kirkegaard"))
spss_data_table = df_var_table(spss_data)

test_that("df_var_table", {
  #we got a data frame?
  expect_s3_class(spss_data_table, "data.frame")

  #test specific data features
  expect_true(any(spss_data_table$miss_prop > 0))
  expect_true(any(spss_data_table$classes == "numeric"))
  expect_true(any(is.na(spss_data_table$label)))
})
