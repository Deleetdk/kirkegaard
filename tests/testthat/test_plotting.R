### tests for df_ functions

context("GG_")


# GG_text -----------------------------------------------------------------
#convenience function for adding text to ggplots
#hard to do formally

base_plot = ggplot(data_frame(x = 0, y = 0), aes(x, y)) +
  geom_point()

test_that("text", {
  #blank
  expect_s3_class({base_plot + GG_text("red test")}, "ggplot")

  #positions
  expect_s3_class({base_plot + GG_text("red test", text_pos = "tl")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "tm")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "tr")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "ml")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "mm")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "mr")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "bl")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "bl")}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test", text_pos = "bl")}, "ggplot")

  #color
  expect_s3_class({base_plot + GG_text("red test", font_color = "red")}, "ggplot")

  #size
  expect_s3_class({base_plot + GG_text("red test", font_size = 99)}, "ggplot")

  #custom position
  expect_s3_class({base_plot + GG_text("red test",
                                       text_pos = "manual",
                                       x = .25,
                                       y = .75)}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test",
                                       text_pos = "manual",
                                       x = .25,
                                       y = .75,
                                       hjust = 1)}, "ggplot")
  expect_s3_class({base_plot + GG_text("red test",
                                       text_pos = "manual",
                                       x = .25,
                                       y = .75,
                                       vjust = 1)}, "ggplot")
  #custom gpar
  #TODO: make a test for this (seems hard)

})


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

  #expect error
  expect_error(GG_denhist(iris, "Sepal.Length", vline = T))
})



# GG_scatter --------------------------------------------------------------


test_that("scatter", {
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = rep("A", 150)), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", repel_names = T), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", alpha = .1), "ggplot")
})


# GG_group_means -----------------------------------------------------------

iris_na = miss_add_random(iris)

#does it respect factor levels order?
iris_reorder = iris
iris_reorder$Species = factor(x = iris_reorder$Species, levels = levels(iris$Species) %>% rev())
gg = GG_group_means(iris_reorder, "Sepal.Length", "Species")

#subgroup
iris2 = iris
iris2$type = sample(c("A", "B"), size = 150, replace = T)

#this the plot means function
l_t = list(GG_group_means(iris, "Sepal.Length", "Species"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "point"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "points"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = .999999),
           GG_group_means(iris_na, "Sepal.Length", "Species", msg_NA = F),
           "order" = GG_group_means(iris_reorder, "Sepal.Length", "Species"),

           #some more parameters tried
           GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type"),
           GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type", type = "point"),
           GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type", type = "points")
           )

test_that("GG_group_means", {
  #all types
  expect_true(all(map_lgl(l_t, function(x) "ggplot" %in% class(x))))

  #missing data, but don't ignore it
  expect_error(GG_group_means(iris_na, 'Sepal.Length', 'Species', na.rm = F))

  #reversed levels
  expect_true(all(levels(l_t$order$data$group1) == rev(levels(iris$Species))))
})


