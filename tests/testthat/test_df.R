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


# smart_join tests --------------------------------------------------------

context("smart_join")

test_that("smart_join fills NA in data1 from data2, but never overwrites non-NA", {
  data1 <- tibble(
    id = 1:4,
    x  = c(1, NA, 3, NA),
    y  = c("a", "b", NA, "d")
  )

  data2 <- tibble(
    id = 1:4,
    x  = c(10, 20, NA, 40),
    y  = c("A", "B", "C", "D")
  )

  res <- smart_join(data1, data2, id_col = id)

  expect_equal(res$x, c(1, 20, 3, 40))    # NA in data1 filled from data2
  expect_equal(res$y, c("a", "b", "C", "d"))  # non-NA in data1 kept; NA filled
})

test_that("smart_join includes columns present only in one data frame", {
  data1 <- tibble(
    id = 1:3,
    a  = 1:3
  )

  data2 <- tibble(
    id = 2:4,
    b  = 10:12   # length 3 to match id
  )

  res <- smart_join(data1, data2, id_col = "id")

  # Ensure deterministic order for checking
  res <- dplyr::arrange(res, id)

  # full join behaviour: ids 1,2,3,4
  expect_equal(res$id, 1:4)

  # a only from data1
  expect_equal(res$a, c(1, 2, 3, NA_integer_))

  # b only from data2
  expect_equal(res$b, c(NA_integer_, 10L, 11L, 12L))
})


test_that("smart_join accepts id_col as bare name and as string", {
  data1 <- tibble(
    id = 1:3,
    x  = c(1, NA, 3)
  )
  data2 <- tibble(
    id = 1:3,
    x  = c(10, 20, 30)
  )

  res_sym  <- smart_join(data1, data2, id_col = id)
  res_str  <- smart_join(data1, data2, id_col = "id")

  expect_equal(res_sym$x, res_str$x)
  expect_equal(res_sym$id, res_str$id)
})

test_that("smart_join complains if id_col missing in data1 or data2", {
  data1 <- tibble(
    id = 1:3,
    x  = 1:3
  )
  data2 <- tibble(
    other_id = 1:3,
    x        = 1:3
  )

  expect_error(
    smart_join(data1, data2, id_col = "other_id"),
    "not found in data1"
  )

  expect_error(
    smart_join(data1, data2, id_col = "id_missing"),
    "not found in data1"
  )
})

test_that("smart_join uses rownames when id_col = '.rownames'", {
  data1 <- data.frame(
    x = c(1, NA),
    row.names = c("r1", "r2")
  )
  data2 <- data.frame(
    x = c(10, 20),
    row.names = c("r2", "r3")
  )

  res <- smart_join(data1, data2, id_col = ".rownames")

  # Expect .rownames column with union of rownames
  expect_equal(rownames(res), c("r1", "r2", "r3"))

  # For r1: only in data1 -> 1
  # For r2: NA in data1, filled from data2 -> 10
  # For r3: only in data2 -> 20
  expect_equal(res$x, c(1, 10, 20))
})


test_that("smart_join errors if rownames are not present when id_col = '.rownames'", {
  data1 <- data.frame(x = 1:2)
  data2 <- data.frame(x = 3:4)

  # Make rownames invalid for data1: include a blank name
  rownames(data1) <- c("", "r2")
  # data2 rownames are fine, but that's okay: data1 already invalid
  rownames(data2) <- c("r3", "r4")

  expect_error(
    smart_join(data1, data2, id_col = ".rownames"),
    "does not have valid rownames"
  )
})


test_that("smart_join messages about fillings", {
  data1 <- tibble(
    id = 1:2,
    x  = c(NA, 2)
  )
  data2 <- tibble(
    id = 1:2,
    x  = c(10, NA)
  )

  expect_message(
    res <- smart_join(data1, data2, id_col = "id"),
    "Filled missing values"
  )
  expect_equal(res$x, c(10, 2))
})


test_that("smart_join works with sf objects (if sf is installed)", {
  skip_if_not_installed("sf")

  library(sf)

  data1 <- st_as_sf(
    tibble(
      id = 1:2,
      x  = c(NA_real_, 2),
      lon = c(0, 1),
      lat = c(0, 1)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  data2 <- st_as_sf(
    tibble(
      id = 1:2,
      x  = c(10, NA_real_),
      lon = c(0, 1),
      lat = c(0, 1)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  res <- smart_join(data1, data2, id_col = "id")

  expect_s3_class(res, "sf")
  # x should be filled just like with non-sf tibbles
  expect_equal(res$x, c(10, 2))
})

test_that("smart_join errors on duplicate IDs", {
  data1 <- tibble(id = c(1, 1), x = c(1, 2))
  data2 <- tibble(id = 1:2,       y = c(10, 20))

  expect_error(
    smart_join(data1, data2, id_col = "id"),
    "duplicate values in data1"
  )
})


test_that("smart_join errors on incompatible column types when filling is needed", {
  data1 <- tibble(
    id = 1:2,
    x  = c(NA_real_, 2)
  )
  data2 <- tibble(
    id = 1:2,
    x  = c("a", "b")  # character
  )

  expect_error(
    smart_join(data1, data2, id_col = "id"),
    "incompatible types"
  )
})

test_that("smart_join with sf objects also errors on incompatible types when filling", {
  skip_if_not_installed("sf")
  library(sf)

  data1 <- st_as_sf(
    tibble(
      id  = 1:2,
      val = c(NA_real_, 2),
      lon = c(0, 1),
      lat = c(0, 1)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  data2 <- st_as_sf(
    tibble(
      id  = 1:2,
      val = c("10", "20"),   # character
      lon = c(0, 1),
      lat = c(0, 1)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  expect_error(
    smart_join(data1, data2, id_col = "id"),
    "incompatible types"
  )
})


test_that("smart_join ignores type mismatch if no values are filled from data2", {
  data1 <- tibble(
    id = 1:2,
    x  = c(1, 2)
  )
  data2 <- tibble(
    id = 1:2,
    x  = c("10", "20")
  )

  expect_silent(
    res <- smart_join(data1, data2, id_col = "id")
  )
  expect_equal(res$x, c(1, 2))
})


test_that("smart_join errors on incompatible types when filling is needed (numeric vs character)", {
  data1 <- tibble(
    id = 1:2,
    x  = c(NA_real_, 2)  # numeric with an NA
  )
  data2 <- tibble(
    id = 1:2,
    x  = c("10", "20")   # character
  )

  expect_error(
    smart_join(data1, data2, id_col = "id"),
    "incompatible types"
  )
})

test_that("smart_join ignores incompatible types if no filling is needed", {
  data1 <- tibble(
    id = 1:2,
    x  = c(1, 2)         # numeric, no NA
  )
  data2 <- tibble(
    id = 1:2,
    x  = c("10", "20")   # character
  )

  expect_silent(
    res <- smart_join(data1, data2, id_col = "id")
  )

  # data1 values should be kept as-is
  expect_equal(res$x, c(1, 2))
  expect_true(is.numeric(res$x))
})

test_that("smart_join merges factor levels when filling from data2", {
  data1 <- tibble(
    id = 1:3,
    f  = factor(c("A", NA, "B"), levels = c("A", "B"))
  )

  data2 <- tibble(
    id = 1:3,
    f  = factor(c("B", "C", "C"), levels = c("B", "C"))
  )

  res <- smart_join(data1, data2, id_col = "id")

  # For id 1: data1 = "A" (kept)
  # For id 2: data1 = NA, data2 = "C" (filled)
  # For id 3: data1 = "B" (kept)
  expect_equal(as.character(res$f), c("A", "C", "B"))

  # Levels should be union of both
  expect_setequal(levels(res$f), c("A", "B", "C"))
})

test_that("smart_join accepts factor type mismatch when no filling is needed", {
  data1 <- tibble(
    id = 1:2,
    f  = factor(c("A", "B"), levels = c("A", "B"))
  )

  data2 <- tibble(
    id = 1:2,
    f  = factor(c("X", "Y"), levels = c("X", "Y"))
  )

  # No NAs in data1$f, so no actual filling
  expect_silent(
    res <- smart_join(data1, data2, id_col = "id")
  )

  expect_equal(as.character(res$f), c("A", "B"))
  expect_setequal(levels(res$f), c("A", "B"))
})

test_that("smart_join errors when filling factor from character", {
  data1 <- tibble(
    id = 1:2,
    f  = factor(c(NA, "B"), levels = c("A", "B"))
  )

  data2 <- tibble(
    id = 1:2,
    f  = c("A", "B")  # character
  )

  expect_error(
    smart_join(data1, data2, id_col = "id"),
    "incompatible types"
  )
})

test_that("smart_join update modes differ as expected on the same data", {
  data1 <- tibble(
    id = 1:4,
    x  = c(1, NA, 3, 4)
  )

  data2 <- tibble(
    id = 1:4,
    x  = c(10, 20, NA, NA)
  )

  # missing: only fill NA in data1 from non-NA in data2
  res_missing <- smart_join(data1, data2, id_col = "id", update = "missing") %>%
    arrange(id)
  expect_equal(res_missing$x, c(1, 20, 3, 4))

  # values: any non-NA in data2 overwrites
  res_values <- smart_join(data1, data2, id_col = "id", update = "values") %>%
    arrange(id)
  expect_equal(res_values$x, c(10, 20, 3, 4))

  # all: data2 wins completely (including NA)
  res_all <- smart_join(data1, data2, id_col = "id", update = "all") %>%
    arrange(id)
  expect_equal(res_all$x, c(10, 20, NA, NA))
})

test_that("smart_join update = 'all' can introduce NA from data2", {
  data1 <- tibble(
    id = 1:3,
    x  = c(1, 2, 3)
  )

  data2 <- tibble(
    id = 1:3,
    x  = c(NA, 20, NA)
  )

  res <- smart_join(data1, data2, id_col = "id", update = "all") %>%
    arrange(id)

  # data2 wins for all shared IDs (including NA)
  expect_equal(res$x, c(NA, 20, NA))
})

test_that("smart_join update = 'values' never introduces NA from data2", {
  data1 <- tibble(
    id = 1:3,
    x  = c(1, 2, 3)
  )

  data2 <- tibble(
    id = 1:3,
    x  = c(NA, 20, NA)
  )

  res <- smart_join(data1, data2, id_col = "id", update = "values") %>%
    arrange(id)

  # only non-NA in data2 (id = 2) overwrites
  expect_equal(res$x, c(1, 20, 3))
})


test_that("smart_join update modes handle rows unique to each data correctly", {
  data1 <- tibble(
    id = 1:3,
    x  = c(1, NA, 3)
  )

  data2 <- tibble(
    id = 2:4,
    x  = c(20, 30, 40)
  )

  # Unique rows:
  # - id 1: only data1
  # - id 4: only data2
  # Shared rows:
  # - id 2, id 3

  res_missing <- smart_join(data1, data2, id_col = "id", update = "missing") %>%
    arrange(id)
  res_values  <- smart_join(data1, data2, id_col = "id", update = "values")  %>%
    arrange(id)
  res_all     <- smart_join(data1, data2, id_col = "id", update = "all")     %>%
    arrange(id)

  # id sequence is same
  expect_equal(res_missing$id, 1:4)
  expect_equal(res_values$id,  1:4)
  expect_equal(res_all$id,     1:4)

  # id 1: only data1
  expect_equal(res_missing$x[1], 1)
  expect_equal(res_values$x[1],  1)
  expect_equal(res_all$x[1],     1)

  # id 2: data1 NA, data2 = 20
  expect_equal(res_missing$x[2], 20)  # filled (missing)
  expect_equal(res_values$x[2],  20)  # overwritten (non-NA)
  expect_equal(res_all$x[2],     20)  # from data2

  # id 3: data1 = 3, data2 = 30
  expect_equal(res_missing$x[3], 3)   # unchanged (not NA)
  expect_equal(res_values$x[3],  30)  # overwritten (non-NA)
  expect_equal(res_all$x[3],     30)  # from data2

  # id 4: only data2, x = 40
  expect_equal(res_missing$x[4], 40)
  expect_equal(res_values$x[4],  40)
  expect_equal(res_all$x[4],     40)
})



test_that("smart_join update modes all error on incompatible types when data2 is used", {
  data1_missing <- tibble(
    id = 1:2,
    x  = c(NA_real_, 2)
  )
  data2_char <- tibble(
    id = 1:2,
    x  = c("10", "20")
  )

  # missing: will try to fill x[1]
  expect_error(
    smart_join(data1_missing, data2_char, id_col = "id", update = "missing"),
    "incompatible types"
  )

  data1_values <- tibble(
    id = 1:2,
    x  = c(1, 2)
  )
  # values: will try to overwrite both with non-NA
  expect_error(
    smart_join(data1_values, data2_char, id_col = "id", update = "values"),
    "incompatible types"
  )

  # all: will fully replace, also should error
  expect_error(
    smart_join(data1_values, data2_char, id_col = "id", update = "all"),
    "incompatible types"
  )
})
