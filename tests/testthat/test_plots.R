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


