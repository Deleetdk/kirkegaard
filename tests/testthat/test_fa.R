context("fa_")

# fa_all_methods --------------------------------------------------------
# fa_am = silence(fa_all_methods(iris[-5], skip_methods = "pa", messages = F, warnings = F))
# # error in psych causes a lot of unsilence-able printed text
#
# test_that("fa_all_methods", {
#   expect_equal(names(fa_am), c("scores", "loadings"))
#   expect_true(all(map_lgl(fa_am, inherits, "data.frame")))
# })



# fa_congruence_matrix ----------------------------------------------------

test_that("fa_congruence_matrix",{
  mpg_numeric = mpg %>% select(where(is.numeric))

  fa_mpg4 = list(
    fa(mpg_numeric, fm = "ml"),
    fa(mpg_numeric, fm = "minres"),
    fa(mpg_numeric, fm = "wls"),
    fa(mpg_numeric, fm = "pa")
  )
  fa_iris4_congru = fa_congruence_matrix(fa_mpg4)

  expect_is(fa_iris4_congru, "matrix")
  expect_equal(dim(fa_iris4_congru), c(4, 4))
})


# fa_Jensens_method -------------------------------------------------------------
#this extract GFP and checks whether the gender difference is GFP-loaded


test_that("fa_Jensens_method",{
  fa_bfi = fa(bfi[1:25])
  set.seed(1)
  tmp_num_only = matrix(rnorm(10000), ncol = 10) %>% as.data.frame()

  #latents
  expect_is(fa_Jensens_method(fa_bfi, bfi, criterion = "gender"), "ggplot")
  expect_is(fa_Jensens_method(fa_bfi, bfi, criterion = "gender", reverse_factor = T), "ggplot")
  expect_is(fa_Jensens_method(fa_bfi, bfi, criterion = "gender", loading_reversing = F), "ggplot")

  #pearson
  expect_is(fa_Jensens_method(fa(tmp_num_only[, -1]), tmp_num_only, "V1"), "ggplot")
})


# fa_residuals ------------------------------------------------------------

test_that("fa_Jensens_method",{
  fa_resids = fa_residuals(swiss)

  expect_is(fa_resids, "data.frame")
  expect_equal(dim(fa_resids), c(47, 6))
})

# fa_MAR ------------------------------------------------------------------


test_that("fa_MAR", {
  fa_mar = fa_MAR(swiss, scores = "Bartlett")

  expect_equal(dim(fa_mar), c(47, 1))
  expect_is(fa_mar, "data.frame")
})


# fa_mixedness ------------------------------------------------------------


test_that("fa_mixedness", {
  fa_mix = fa_mixedness(swiss)

  expect_equal(dim(fa_mix), c(47, 5))
  expect_is(fa_mix, "data.frame")
})


# fa_splitsample_repeat ---------------------------------------------------------------------


test_that("fa_splitsample_repeat", {
  fa_splitsam = fa_splitsample_repeat(psychTools::ability, runs = 5, messages = F, progress = F)

  expect_equal(dim(fa_splitsam), c(5, 1))
  expect_is(fa_splitsam, "data.frame")
})


# fa_plot_loadings fa_rank_fa -----------------------------------------------------


test_that("fa_plot_loadings", {
  fa_iris3 = list(part1 = fa(iris[1:50, -5]),
                  part2 = fa(iris[51:100, -5]),
                  part3 = fa(iris[101:150, -5]))
  fa_iris3b = list(part1 = fa(iris[1:50, -c(1, 5)], fm = "ml"),
                   part2 = fa(iris[51:100, -c(2, 5)], fm = "ml"),
                   part3 = fa(iris[101:150, -c(3, 5)], fm = "ml"))

  #mono analysis
  expect_is(fa_plot_loadings(fa_iris3[[1]]), "ggplot")

  #multi analysis
  expect_is(fa_plot_loadings(fa_iris3), "ggplot")
  expect_is(fa_plot_loadings(fa_iris3, reorder = 1), "ggplot")
  expect_is(fa_plot_loadings(fa_iris3, reorder = 2), "ggplot")
  expect_is(fa_plot_loadings(fa_iris3, reorder = 3), "ggplot")

  #nonperfect overlap
  expect_is(fa_plot_loadings(fa_iris3b), "ggplot")
})


# fa_loadings ----------------------------------------------------


test_that("fa_loadings", {
  fa_bfi = fa(bfi[1:25])
  fa_bfi2 = fa(bfi[1:25], 2)

  #1 factor
  expect_is(fa_loadings(fa_bfi), "data.frame")
  expect_equal(dim(fa_loadings(fa_bfi)), c(25, 1))

  #2 factors
  expect_is(fa_loadings(fa_bfi2), "data.frame")
  expect_equal(dim(fa_loadings(fa_bfi2)), c(25, 2))

  #long form
  expect_equal(dim(fa_loadings(fa_bfi2, long_form = T)), c(50, 3))

  #threshold
  expect_true(anyNA(fa_loadings(fa_bfi2, threshold = .2)))
  expect_true(!anyNA(fa_loadings(fa_bfi2, threshold = .2, long_form = T)))
  expect_equal(dim(fa_loadings(fa_bfi2, threshold = .2, long_form = T)), c(25, 3))
})


# fa_nfactors -------------------------------------------------------------

test_that("fa_nfactors", {
  fa_iris4 = list(
    fa(iris[-5], fm = "ml"),
    fa(iris[-5], fm = "ml"),
    fa(iris[-5], fm = "ml"),
    fa(iris[-5], fm = "ml")
  )

  expect_equal(fa_nfactors(fa_iris4), rep(1, 4))
})

