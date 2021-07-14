### tests for df_ functions

context("GG_")



# GG_matrix ---------------------------------------------------------------

test_that("GG_matrix", {
  expect_s3_class(iris %>% miss_add_random() %>% GG_matrix(), "ggplot")
})


# GG_text -----------------------------------------------------------------
#convenience function for adding text to ggplots
#hard to do formally

test_that("text", {
  #base
  base_plot = ggplot(tibble(x = 0, y = 0), aes(x, y)) +
    geom_point()

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
  #data prep
  iris$labelled = haven::labelled(iris$Sepal.Length, labels = NULL)

  expect_s3_class(GG_denhist(iris, "Sepal.Length"), "ggplot")
  expect_s3_class(GG_denhist(iris, "Sepal.Length", vline = median), "ggplot")
  expect_s3_class(GG_denhist(iris, "Sepal.Length", group = "Species"), "ggplot")
  expect_s3_class(iris$Sepal.Length %>% GG_denhist, "ggplot")
  expect_s3_class(silence(GG_denhist(iris[1])), "ggplot")
  expect_s3_class(silence(data.frame(x = c(1, 2, NA), y = c(1, 2, 3)) %>% GG_denhist("x", "y")), "ggplot")
  expect_s3_class(silence(data.frame(x = c(1, 2, 3), y = c(1, 2, NA)) %>% GG_denhist("x", "y")), "ggplot")

  #advanced atomic input
  expect_s3_class(GG_denhist(iris, "labelled"), "ggplot")
  #bugged unit test
  # expect_s3_class(GG_denhist(iris$labelled), "ggplot")

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
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", case_names_color = "purple"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", repel_names = T), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", alpha = .1), "ggplot")

  #test cleaning of color groups
  iris$Species2 = iris$Species + "_X"
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species2"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species2", clean_names = F), "ggplot")
})


# GG_group_means -----------------------------------------------------------



test_that("GG_group_means", {
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
             GG_group_means(iris, "Sepal.Length", "Species", type = "boxplot"),

             #some more parameters tried
             GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type"),
             GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type", type = "point"),
             GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type", type = "points"),
             GG_group_means(mpg, "displ", "manufacturer", subgroupvar = "drv", type = "boxplot")
  )

  #add more groups to iris
  iris$group2 = sample(letters[1:3], size = 150, replace = T)

  #all types
  expect_true(all(map_lgl(l_t, function(x) "ggplot" %in% class(x))))

  #missing data, but don't ignore it
  expect_error(GG_group_means(iris_na, 'Sepal.Length', 'Species', na.rm = F))

  #reversed levels
  expect_true(all(levels(l_t$order$data$group1) == rev(levels(iris$Species))))

  #error because removed all groups
  expect_error(GG_group_means(iris, "Sepal.Length", groupvar = "Species", min_n = 51), regexp = "No groups left after filtering to sample size requirement")
  expect_error(GG_group_means(iris, "Sepal.Length", groupvar = "Species", subgroupvar = "group2", min_n = 51), regexp = "No groups left after filtering to sample size requirement")
})



# GG_heatmap --------------------------------------------------------------

test_that("GG_heatmap", {
  #save plots to list
  heatmaps = list(
    #various options
    default = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(),
    no_reorder = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(reorder_vars = F),
    no_values = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(add_values = F),
    many_digits = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(digits = 5),
    small_text = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(font_size = 2),
    move_legend = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(legend_position = c(.5, .75)),
    short_x_labels = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(short_x_labels = T),
    axis_labels_clean_func = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(axis_labels_clean_func = NULL)
  )

  #check that plots work
  walk(heatmaps, ~expect_s3_class(., class = "ggplot"))

  #check for non-identity
  #cant think of an easy smart way to do this
  expect_true(!identical(heatmaps$default, heatmaps$no_reorder))
  expect_true(!identical(heatmaps$default, heatmaps$no_values))
  expect_true(!identical(heatmaps$default, heatmaps$many_digits))
  expect_true(!identical(heatmaps$no_reorder, heatmaps$no_values))
  expect_true(!identical(heatmaps$no_reorder, heatmaps$many_digits))
  expect_true(!identical(heatmaps$no_values, heatmaps$many_digits))
})



# GG_save -----------------------------------------------------------------

test_that("GG_save", {
  #make a plot
  plot = ggplot(datasets::quakes, aes(mag)) +
    geom_histogram()

  #save it
  GG_save(filename = "tmp.png")

  #assert exists
  expect_true(file.exists("tmp.png"))

  #delete the file
  file.remove("tmp.png")

  #test ggtern if exists
  if (is_inst("ggtern")) {
    #make data
    tern_data = matrix(runif(60), nrow = 20) %>%
      apply(MARGIN = 1, FUN = function(row) {
        row/sum(row)
      }) %>%
      t() %>%
      set_colnames(letters[1:3]) %>%
      as_tibble()

    plot = ggtern::ggtern(tern_data, aes(x = a, y = b, z = c)) +
      geom_point()

    #assert class
    expect_true(plot$coordinates %>% is(class2 = "CoordTern"))

    #save it
    GG_save(filename = "tmp.png")

    #assert exists
    expect_true(file.exists("tmp.png"))

    #delete the file
    file.remove("tmp.png")
  }
})
