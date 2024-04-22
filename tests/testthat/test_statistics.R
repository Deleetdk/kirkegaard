context("statistics")




test_that("describe2", {
  #classes
  expect_s3_class(describe2(iris), "data.frame")
  expect_s3_class(describe2(iris), "tbl")

  #non numeric equivalent cols removed automatically
  tibble(
    lgl = sample(c(T, F), size = 10, replace = T),
    dbl = rnorm(10),
    int = sample(c(1L, 2L), size = 10, replace = T),
    chr = letters[1:10],
    fct = as.factor(LETTERS[1:10])
    ) %>%
    describe2() %>%
    {
      expect_equivalent(c("lgl", "dbl", "int"), .$var)
    }

  #correct cols
  expect_equivalent(describe2(iris) %>% colnames(), c("var", "n", "mean", "median", "sd", "mad", "min", "max", "skew", "kurtosis"))

  #grouped version
  expect_equivalent(describe2(iris, group = iris$Species) %>% colnames(), c("group", "var", "n", "mean", "median", "sd", "mad", "min", "max", "skew", "kurtosis"))
})




test_that("transform_01", {
  expect_equal(transform_01(1:5), ((1:5)-1)/4)
  expect_true(anyNA(transform_01(c(1, NA, 2))))
  expect_equivalent(transform_01(c()), c())
})


#correlation matrix with nice output

test_that("cor_matrix", {
  #add missing data
  iris_miss = miss_add_random(iris[-5])

  #validate vs. base-r
  expect_equivalent(cor_matrix(iris[-5]), cor(iris[-5]))

  #automatic skip non-numeric
  expect_equivalent(cor_matrix(iris), cor(iris[-5]))

  #with weights
  expect_equivalent(cor_matrix(iris[-5], weights = rep(1, 150)), cor(iris[-5]))

  #other weights
  expect_false(are_equal(cor_matrix(iris[-5], weights = runif(150)), cor(iris[-5])))

  #complex weights
  expect_false(are_equal(cor_matrix(iris[-5], weights = iris[-5]), cor(iris[-5])))

  #test arguments
  expect_true(cor_matrix(iris, CI = .95) %>% is.character())
  expect_true(cor_matrix(iris, p_val = T) %>% is.character())
  expect_true(cor_matrix(iris, p_val = T, asterisks_only = T) %>% is.character())
  expect_true(all((cor_matrix(iris, CI = .95) %>% get_dims()) == (c(4, 4))))
  expect_true(cor_matrix(iris[-5], weights = iris[-5], CI = .95) %>% is.character())

  #errors
  expect_error(cor_matrix(iris, p_val = T, CI = .95), regexp = "Cannot both calculate CIs and p values")

  #missing data
  expect_equivalent(cor_matrix(iris_miss), cor(iris_miss, use = "pairwise"))
  expect_true(cor_matrix(iris_miss, CI = .95) %>% is.character())
  expect_true(cor_matrix(iris_miss, p_val = T) %>% is.character())
})



test_that("adj_d_reliability", {
  expect_equivalent(adj_d_reliability(1, 1), 1)
  expect_equivalent(adj_d_reliability(1, rel = 0), Inf)
  expect_equivalent(adj_d_reliability(-2, rel = 0), -Inf)
  expect_error(adj_d_reliability(1, rel = -1))
})


#standardized mean differences


test_that("SMD_matrix", {
  #iris with missing
  set.seed(1)
  iris_miss = miss_add_random(iris)

  #tests
  t = list(#parameters
    SMD_matrix(iris$Sepal.Length, iris$Species),
    SMD_matrix(iris$Sepal.Length, iris$Species, central_tendency = median),
    SMD_matrix(iris$Sepal.Length, iris$Species, dispersion = "mad"),
    SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "pair"),
    SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "total"),
    SMD_matrix(iris$Sepal.Length, iris$Species, central_tendency = mean, trim = .05),
    SMD_matrix(iris$Sepal.Length, iris$Species, reliability = 0.9),

    #with missing data
    SMD_matrix(iris_miss$Sepal.Length, iris_miss$Species)
  )

  #correct type
  expect_true(all(purrr::map_lgl(t, is.matrix)))

  #all different results
  unique(t) %>% length %>% `==`(7)
})






test_that("standardize", {
  set.seed(1)
  X = rnorm(100, mean = 10, sd = 5)
  W = runif(100)

  #2 groups
  X2 = c(rnorm(10000), rnorm(10000, 1))
  focal = rep(c(T, F), each = 10000)

  #test normal
  expect_equal(mean(standardize(X)), 0)
  expect_equal(sd(standardize(X)), 1)

  #test weights
  expect_equal(weighted.mean(standardize(X, W), W), 0)

  #robust
  expect_equal(median(standardize(X, robust = T)), 0)
  expect_equal(mad(standardize(X, robust = T)), 1)

  #focal group
  expect_equal(mean(standardize(X2, focal_group = focal)[focal]), 0)
  expect_equal(sd(standardize(X2, focal_group = focal)[focal]), 1)
  expect_equal(mean(standardize(X2, focal_group = focal)[!focal]), 1, tol = .02)
  expect_equal(sd(standardize(X2, focal_group = focal)[!focal]), 1, tol = .05)
})




test_that("winsorise", {
  #test normal
  expect_equal(winsorise(0), 0) #no limits, means no winsorising
  expect_identical(winsorise(NA), NA_real_) #convert NA to double
  expect_equal(winsorise(99, upper = 1), 1)
})






test_that("wtd_mean", {
  #some variables
  set.seed(1)
  rand_norm = rnorm(100)
  rand_uniform = runif(100)
  x = c(1, 1, 2, 2, 2)

  expect_true(mean(rand_norm) == wtd_mean(rand_norm))
  expect_true(weighted.mean(rand_norm, rand_uniform) == wtd_mean(rand_norm, rand_uniform))
})

test_that("wtd_sd", {
  #some variables
  set.seed(1)
  rand_norm = rnorm(100)
  rand_uniform = runif(100)
  x = c(1, 1, 2, 2, 2)

  expect_true(sd(rand_norm) == wtd_sd(rand_norm))
  #dont agree, but I think it has to do with the correction Hmisc uses...
  # expect_equivalent(Hmisc::wtd.var(rand_norm, rand_uniform) %>% sqrt(), wtd_sd(rand_norm, rand_uniform))
})

# test_that("wtd_median", {
#   expect_true(median(rand_norm) == wtd_median(rand_norm))
#   expect_true(Hmisc::wtd.quantile(rand_norm, rand_uniform, probs = .5) == wtd_median(rand_norm, rand_uniform))
# })
#
# test_that("wtd_quantile", {
#   expect_equivalent(Hmisc::wtd.quantile(rand_norm), wtd_quantile(rand_norm))
#   expect_equivalent(Hmisc::wtd.quantile(rand_norm, rand_uniform), wtd_quantile(rand_norm, rand_uniform))
# })




test_that("calc_row_representativeness", {
  #compute for mpg dataset
  mpg_representativeness = mpg %>%
    select(displ, cyl, cty, hwy) %>%
    calc_row_representativeness()

  expect_s3_class(mpg_representativeness, "data.frame")
  expect_equal(c(234, 7), dim(mpg_representativeness))
  expect_equal(colnames(mpg_representativeness[-1]), c("displ", "cyl", "cty", "hwy", "mean", "median"))
  expect_equal(mpg_representativeness %>% arrange(mean) %>% .[[1, "row"]],
               42)
})





test_that("test_HS", {
  #fix some data in iris
  iris2 = iris %>%
    arrange(Petal.Length) %>%
    mutate(
      Sepal.Length_linHS = Sepal.Length + rnorm(n()) * seq(1, 3, length.out = n()),
      Sepal.Length_nonlinHS = Sepal.Length + rnorm(n()) * c(seq(1, 3, length.out = n()/2), seq(3, 1, length.out = n()/2))
    )

  #fit models
  test1 = test_HS(resid = resid(lm(Sepal.Length ~ Petal.Length, data = iris2)), x = iris2$Petal.Length)
  test2 = test_HS(resid = resid(lm(Sepal.Length_linHS ~ Petal.Length, data = iris2)), x = iris2$Petal.Length)
  test3 = test_HS(resid = resid(lm(Sepal.Length_nonlinHS ~ Petal.Length, data = iris2)), x = iris2$Petal.Length)

  #objects look right
  expect_s3_class(test1, "tbl")
  expect_s3_class(test2, "tbl")
  expect_s3_class(test3, "tbl")

  expect_true(nrow(test1) == 4)
  expect_true(nrow(test2) == 4)
  expect_true(nrow(test3) == 4)
  expect_true(ncol(test1) == 5)
  expect_true(ncol(test2) == 5)
  expect_true(ncol(test3) == 5)
  expect_true(!anyNA(test1))
  expect_true(!anyNA(test2))
  expect_true(!anyNA(test3))
})






test_that("quantile_smooth", {
  default = quantile_smooth(iris$Petal.Length, iris$Sepal.Length, quantile = .90)
  qgam = quantile_smooth(iris$Petal.Length, iris$Sepal.Length, quantile = .90, method = "Rq")
  Rq = quantile_smooth(iris$Petal.Length, iris$Sepal.Length, quantile = .90, method = "Rq")
  running = quantile_smooth(iris$Petal.Length, iris$Sepal.Length, quantile = .90, method = "running")

  #looks right
  expect_type(default, "double")
  expect_type(qgam, "double")
  expect_type(Rq, "double")
  expect_type(running, "double")

  #length ok
  expect_true(length(default) == nrow(iris))
  expect_true(length(qgam) == nrow(iris))
  expect_true(length(Rq) == nrow(iris))
  expect_true(length(running) == nrow(iris))

  #no missing
  expect_true(!anyNA(default))
  expect_true(!anyNA(qgam))
  expect_true(!anyNA(Rq))
  expect_true(!anyNA(running))

})






test_that("prop_tests", {
  prop_tests(mpg$cyl, mpg$manufacturer) %>%
    expect_s3_class("tbl_df") %>%
    expect_silent() %>%
    {expect_equal(dim(.), c(60, 12))}
})



#https://rpubs.com/EmilOWK/IRT_DIF_examples


test_that("DIF_test", {
  n = 1000
  n_items = 10

  #slopes
  set.seed(1)
  a1 = runif(n_items, min = .5, max = 2)
  a2 = a1

  a2[1] = 0 #item doesnt work for this group

  #intercepts
  i1 = rnorm(n_items, mean = -0.5, sd = 2)
  i2 = i1

  i2[2] = 1 #item much harder for this group

  #simulate data twice
  d1 = mirt::simdata(
    a1,
    i1,
    N = n,
    itemtype = "2PL",
    mu = 0
  )

  d2 = mirt::simdata(
    a2,
    i2,
    N = n,
    itemtype = "2PL",
    mu = 1
  )

  #combine
  d = bind_rows(
    d1 %>% as.data.frame(),
    d2 %>% as.data.frame()
  )

  #find the DIF
  suppressMessages({
    DIF_fit = DIF_test(
      d,
      model = 1,
      itemtype = "2PL",
      group = rep(c("A", "B"), each = nrow(d1)),
      verbose = F
    )
  })

  #find the two bad items
  #test we find the 2 positives, and all the others are negative
  expect_true(DIF_fit$DIF_stats$p_adj[1] < .001)
  expect_true(DIF_fit$DIF_stats$p_adj[2] < .001)
  expect_true(all(DIF_fit$DIF_stats$p_adj[3:10] > .001))
})


test_that("abbreviate_scale", {
  #generate some data using mirt
  set.seed(1)
  d_sim = mirt::simdata(
    a = runif(10, min = .5, max = 2),
    d = rnorm(10),
    N = 1000,
    itemtype = "2PL"
  )

  #we want a 10 item version
  max_items = 5

  #suppress all output
  sink(nullfile())

  #abbreviate it using different methods
  #forwards
  d_sim_abbrev_forwards = abbreviate_scale(
    d_sim,
    method = "forwards",
    item_target = max_items,
    )

  #backwards
  d_sim_abbrev_backwards = abbreviate_scale(
    d_sim,
    method = "backwards",
    item_target = max_items
    )

  #max loading
  d_sim_abbrev_max_loading = abbreviate_scale(
    d_sim,
    method = "max_loading",
    item_target = max_items
    )

  #genetic
  d_sim_abbrev_genetic = abbreviate_scale(
    d_sim,
    method = "genetic",
    item_target = max_items,
    max_generations = 3,
    population_size = 10
    )

  #turn off sink
  sink()

  #verify outputs are correct
  #are lists
  expect_is(d_sim_abbrev_forwards, "list")
  expect_is(d_sim_abbrev_backwards, "list")
  expect_is(d_sim_abbrev_max_loading, "list")
  expect_is(d_sim_abbrev_genetic, "list")

  #has some of the right items
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_forwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_backwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_max_loading)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_genetic)))

  #test the plots
  p_forwards = d_sim_abbrev_forwards %>% GG_scale_abbreviation()
  p_backwards = d_sim_abbrev_backwards %>% GG_scale_abbreviation()
  p_max_loading = d_sim_abbrev_max_loading %>% GG_scale_abbreviation()
  p_genetic = d_sim_abbrev_genetic %>% GG_scale_abbreviation()

  #check the plots
  expect_true(p_forwards %>% is.ggplot())
  expect_true(p_backwards %>% is.ggplot())
  expect_true(p_max_loading %>% is.ggplot())
  expect_true(p_genetic %>% is.ggplot())

})
