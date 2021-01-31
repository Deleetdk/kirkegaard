context("stereotypes")


#simulate some data we will use
#criterion values
set.seed(1)
sim_criterion_values = rnorm(20)
bias_values = rnorm(20)
sim_n = 1000

suppressMessages({
  sim_ratings = map_dfc(1:sim_n, function(i) {
    tibble(
      a = sim_criterion_values*.5 + rnorm(20, mean = rnorm(1), sd = runif(1, min = 0, max = 2))
    )
  }) %>% t() %>% set_colnames("E" + seq_along(sim_criterion_values)) %>% as_tibble()
})



# accuracy scoring --------------------------------------------------

test_that("score_accuracy", {
  #score it
  scored = score_accuracy(sim_ratings, criterion = sim_criterion_values)
  scored_all = score_accuracy(sim_ratings, criterion = sim_criterion_values, methods = "all")

  #data type and size
  expect_s3_class(scored, "data.frame")
  expect_equal(c(1000, 4), dim(scored))
  expect_true(!anyNA(scored))

  expect_s3_class(scored_all, "data.frame")
  expect_equal(c(1000, 9), dim(scored_all))
  expect_true(!anyNA(scored_all))
})

# bias scoring functions --------------------------------------------------

test_that("score_bias_metrics", {
  #score it
  scored = score_bias_metrics(sim_ratings, criterion = sim_criterion_values, bias_var = bias_values)

  #data type and size
  expect_s3_class(scored, "data.frame")
  expect_equal(c(1000, 6), dim(scored))
  expect_true(!anyNA(scored))
})


test_that("score_by", {
  test_data = tibble(
    a = c(1:4),
    b = c(5:8)
  )

  #chr input
  score_by(test_data, moderator = c("m", "m", "f", "f")) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "5c2c228d2f")

  #specific levels
  score_by(test_data, moderator = c("m", "m", "f", "f") %>% factor(levels = c("m", "f"))) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "bd9fcb4550")

  #unused levels
  score_by(test_data, moderator = c("m", "m", "f", "f") %>% factor(levels = c("m", "f", "unused"))) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "bd9fcb4550")

  score_by(test_data, moderator = c("m", "m", "f", "f") %>% factor(levels = c("m", "f", "unused")), drop_unused_levels = F) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "bf900e529f")

  #numerical grouping
  score_by(test_data, moderator = c(1:4)) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "b91bd92d89")

  score_by(test_data, moderator = seq(0, 1, length.out = 4)) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "5a7e38ec23")

  score_by(test_data, moderator = seq(.2, .8, length.out = 4), extrapolate_to = c(0, 1)) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "3f8ace5820")

  #missing data
  test_data_NA = tibble(
    a = c(1:4, NA),
    b = c(5:8, NA)
  )

  score_by(test_data_NA, moderator = c("m", "m", "f", "f", "f")) %>%
    expect_s3_class("data.frame") %>%
    expect_known_hash(hash = "5c2c228d2f")
})
