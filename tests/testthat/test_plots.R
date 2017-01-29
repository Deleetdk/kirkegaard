### tests for df_ functions

context("GG_")


# GG_denhist --------------------------------------------------------------
#just run the plots

test_that("denhist", {
  expect_s3_class(GG_denhist(iris, "Sepal.Length"), "ggplot")
  expect_s3_class(GG_denhist(iris, "Sepal.Length", vline = median), "ggplot")
  expect_s3_class(GG_denhist(iris, "Sepal.Length", group = "Species"), "ggplot")
  expect_s3_class(iris$Sepal.Length %>% GG_denhist, "ggplot")
  expect_s3_class(silence(GG_denhist(iris[1])), "ggplot")
  expect_s3_class(silence(data.frame(x = c(1, 2, NA), y = c(1, 2, 3)) %>% GG_denhist("x", "y")), "ggplot")
  expect_s3_class(silence(data.frame(x = c(1, 2, 3), y = c(1, 2, NA)) %>% GG_denhist("x", "y")), "ggplot")

  #expect warnings
  expect_warning(GG_denhist(iris[1]))
  expect_warning(data.frame(x = c(1, 2, NA), y = c(1, 2, 3)) %>% GG_denhist("x", "y"))
  expect_warning(data.frame(x = c(1, 2, 3), y = c(1, 2, NA)) %>% GG_denhist("x", "y"))
})



# GG_scatter --------------------------------------------------------------


test_that("scatter", {
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names_vector = rep("A", 150)), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names_vector = "Species"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150), "ggplot")
})
