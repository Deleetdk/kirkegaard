### tests for df_ functions

context("plotting")





test_that("GG_matrix", {
  expect_s3_class(iris %>% miss_add_random() %>% GG_matrix(), "ggplot")
})



#convenience function for adding text to ggplots
#hard to do formally

test_that("GG_text", {
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





#just run the plots
test_that("GG_denhist", {
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






test_that("GG_scatter", {
  iris$runif = runif(nrow(iris))

  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = rep("A", 150)), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", case_names_color = "purple"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", repel_names = T), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", case_names = "Species", repel_names = T, text_size = 5), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", text_pos = "br"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", CI = .99), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", clean_names = F), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", weights = 1:150), "ggplot")

  #color scales
  #discrete
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species") + scale_color_discrete(), "ggplot")
  #continuous
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "runif") + scale_color_gradient2(low = "blue", high = "red", mid = "green"), "ggplot")

  #alpha
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", alpha = .1), "ggplot")

  #test cleaning of color groups
  iris$Species2 = iris$Species + "_X"
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species2"), "ggplot")
  expect_s3_class(GG_scatter(iris, "Sepal.Length", "Sepal.Width", color = "Species2", clean_names = F), "ggplot")

  #fails
  expect_error(GG_scatter(iris[numeric(), ], "Sepal.Length", "Sepal.Width"), "no cases")
  expect_error(GG_scatter(tibble(x = c(1, 2, NA, NA), y = c(NA, NA, 3, 4)), "x", "y"), "no complete cases")
  expect_error(GG_scatter(tibble(x = c(1, 1, 1, 1), y = c(2, 2, 2, 2)), "x", "y"), "Correlation could not be computed")
})






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
             GG_group_means(mpg, "displ", "manufacturer", subgroupvar = "drv", type = "boxplot"),
             GG_group_means(iris, "Sepal.Length", "Species", type = "points", add_sample_sizes_to_labels = T)
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

  #empty levels bug!
  iris4 = bind_rows(tibble(Species = "empty"), iris) %>%
    mutate(group2 = rep(1:2, length.out = 151))

  GG_group_means(iris, "Sepal.Length", "Species") -> gg1
  GG_group_means(iris4, "Sepal.Length", "Species") -> gg2
  GG_group_means(iris4, "Sepal.Length", "Species", subgroupvar = "group2") -> gg3
  GG_group_means(iris4, "Sepal.Length", "Species", subgroupvar = "group2", type = "points") -> gg4

  #check factor level lengths
  expect_true(length(levels(gg1$data$group1)) == 3)
  expect_true(length(levels(gg2$data$group1)) == 3)
  expect_true(length(levels(gg3$data$groupvar)) == 3)
  expect_true(length(levels(gg4$data$groupvar)) == 3)

  #almost empty level
  iris5 = bind_rows(tibble(Species = "size1", Sepal.Length = 6), iris) %>%
    mutate(group2 = rep(1:2, length.out = 151))

  GG_group_means(iris5, "Sepal.Length", "Species") -> gg1
  GG_group_means(iris5, "Sepal.Length", "Species", min_n = 2) -> gg2
  GG_group_means(iris5, "Sepal.Length", "Species", min_n = 2, type = "points") -> gg3
  GG_group_means(iris5, "Sepal.Length", "Species", subgroupvar = "group2", min_n = 2, type = "points") -> gg4
  GG_group_means(iris5, "Sepal.Length", "Species", subgroupvar = "group2", type = "points") -> gg5

  expect_true(length(levels(gg1$data$group1)) == 4)
  expect_true(length(levels(gg2$data$group1)) == 3)
  expect_true(length(levels(gg3$data$group1)) == 3)
  expect_true(length(levels(gg4$data$groupvar)) == 3)
  expect_true(length(levels(gg5$data$groupvar)) == 4)
})



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
    axis_labels_clean_func = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(axis_labels_clean_func = NULL),
    nonsig_p = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(cross_out_nonsig = T),
    rm_diag = mtcars[, c(1,3,4,5,6,7)] %>% GG_heatmap(remove_diag = T)
  )

  #check that plots work
  walk(heatmaps, ~expect_s3_class(., class = "ggplot"))
})

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
      apply(MARGIN = 1, function(row) {
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



test_that("GG_proportions", {
  #plot the proportions of cylinders by year
  GG_proportions(mpg$year, mpg$cyl) %>%
  expect_s3_class("ggplot")

  #remove the 0%'s
  GG_proportions(mpg$year, mpg$cyl, drop_empty = T) %>%
  expect_s3_class("ggplot")
})



test_that("save_plot_to_file", {
  save_plot_to_file(plot(1:3), filename = "test.png")
  expect_true(file.exists("test.png"))
  file.remove("test.png")
})

test_that("GG_plot_models", {
  #get some models
  iris_model_coefs = compare_predictors(iris, names(iris)[1], names(iris)[-1])
  mpg_model_coefs = compare_predictors(mpg, names(mpg)[3], names(mpg)[-3])

  #make plots
  iris_plot = GG_plot_models(iris_model_coefs)
  mpg_plot = GG_plot_models(mpg_model_coefs)

  #check type
  expect_s3_class(iris_plot, "ggplot")
  expect_s3_class(mpg_plot, "ggplot")

})


test_that("GG_BMA", {
  #make up some data
  set.seed(1)
  ex_data = tibble(
    y = rnorm(100),
    num = rnorm(100),
    factor = sample(letters[1:3], size = 100, replace = T),
    logical = sample(c(T, F), size = 100, replace = T),
  )

  #fit BMA models
  sink(nullfile())
  on.exit(sink())
  bma_fit = BMA::bic.glm(y ~ ., data = ex_data, glm.family = "gaussian")
  bas_fit = BAS::bas.lm(y ~ ., data = ex_data)
  pdf(file = NULL) #prevent plotting
  bms_fit = BMS::bms(iris[-5])
  dev.off()

  #make plots
  bma_plot = GG_BMA(bma_fit)
  bas_plot = GG_BMA(coef(bas_fit))
  bms_plot = GG_BMA(bms_fit)

  #check type
  expect_s3_class(bma_plot, "ggplot")
  expect_s3_class(bas_plot, "ggplot")
  expect_s3_class(bms_plot, "ggplot")
})


test_that("GG_ordinals", {

  #make some ordinal data
  set.seed(1)
  xx = tibble(
    ord_1 = cut(rnorm(200), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
    ord_2 = cut(rnorm(200, mean = 1), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
    ord_3 = cut(rnorm(200, mean = -1), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D")),
    ord_4 = cut(rnorm(200, mean = 0), breaks = c(-Inf, -1, 0, 1, Inf), labels = c("A", "B", "C", "D"))
  )

  #xx long form
  xx_long = xx %>%
    pivot_longer(everything())

  #add some missing data to 4th
  xx$ord_4[sample(1:200, 20)] = NA

  p1 = xx %>%
    GG_ordinal()

  p2 = xx %>%
    GG_ordinal(order = "negative")

  p3 = xx %>%
    GG_ordinal(percentages = F)

  p4 = xx %>%
    GG_ordinal(clean_factor_levels = F)

  p5 = xx %>%
    GG_ordinal(add_values = F)

  p6 = xx %>%
    GG_ordinal(font_size = 3)

  p7 = xx %>%
    GG_ordinal(exclude_values_below = .05)

  p8 = xx %>%
    GG_ordinal(reverse_factor_levels = T)

  #long form
  p9 = xx_long %>%
    GG_ordinal(vars = "value", group = "name")

  #check that the plots are different
  plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9)

  for (i in 1:length(plot_list)) {
    expect_s3_class(plot_list[[i]], "ggplot")
  }

  #throws warnings, not sure why, results are correct
  # combn(length(plot_list), 2, function(idx) {
  #   expect_true(!are_equal(plot_list[[idx[1]]], plot_list[[idx[2]]]))
  # })
})


test_that("GG_lines", {
  #test it with time series data
  p1 = tidyr::population %>%
    filter(country %in% (.env$population$country %>% unique() %>% str_subset(pattern = "^A") %>% head(10))) %>%
    GG_lines("year", "population", "country") +
    scale_y_log10()

  p2 = tidyr::population %>%
    filter(country %in% (.env$population$country %>% unique() %>% str_subset(pattern = "^A") %>% head(10))) %>%
    GG_lines("year", "population", "country", right_margin = 200) +
    scale_y_log10()

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})
