# some libs ---------------------------------------------------------------
library(pacman)
p_load(kirkegaard, psych, plyr, stringr, MASS, assertthat)

#otherwise get error
options("expressions" = 10000)

# merge_datasets + merge_datasets2 ----------------------------------------------------------
#some data to merge
d1 = iris[1:75, ] #split in two
d2 = iris[76:150, ]
set.seed(1);d2_na = df_addNA(d2)
t = merge_datasets(d1, d2) #merge into one
t2 = merge_datasets(d1, d2, join = "left")
t3 = merge_datasets(d1, d2, join = "right")
t4 = merge_datasets(iris[1], iris[2:5])

stopifnot({
  t == iris #because everything went back to original position
  t2 == d1 #because nothing was joined
  t3 == d2 #because nothing was joined
  t4 == iris #if not, likely that drop=F is needed!
})

#multi version
stopifnot({
  iris == merge_datasets_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ])
})

#merge_datasets2
t = merge_datasets2(d1, d2) #merge into one
t2 = merge_datasets2(d1, d2, join = "left")
t3 = merge_datasets2(d1, d2, join = "right")
t4 = merge_datasets2(iris[1], iris[2:5])
t5 = merge_datasets2(d2, d2_na) #test overwriting of NAs

stopifnot({
  t == iris #because everything went back to original position
  t2 == d1 #because nothing was joined
  t3 == d2 #because nothing was joined
  t4 == iris #if not, likely that drop=F is needed!
  t5 == d2   #because NAs should not be overwritten on top of values
})

#multi version
stopifnot({
  iris == merge_datasets2_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ])
})

# FA_all_methods & FA_congruence_mat --------------------------------------------------------
t = FA_all_methods(iris[-5], skip_methods = "pa", messages = F)

# FA_congruence_mat -------------------------------------------------------
stopifnot({
  dim(cor(t$scores))==c(12, 12)
  t2 = list(fa(iris[-5]), fa(iris[-5]), fa(iris[-5]), fa(iris[-5]))
  t = FA_congruence_matrix(t$loadings)
  t2 = FA_congruence_matrix(t2)
  class(t) == "matrix"
  class(t2) == "matrix"
  dim(t) == c(3, 3)
  dim(t2) == c(4, 4)
})


# std_df ------------------------------------------------------------------
#should print: "Skipped Species because it is a factor."
stopifnot({
  t = std_df(iris, messages = F)
  round(t[5, 1], 4) == -1.0184
  t[10, 5] == "setosa"
})


# rank_df -----------------------------------------------------------------
t = data.frame(letters = letters[1:10],
               norm = rnorm(10),
               unif = runif(10))
stopifnot({
  class(t) == "data.frame"
  dim(t) == c(10, 3)
})


# write_clipboard ---------------------------------------------------------
write_clipboard(iris, 0)
stopifnot({
  read.delim("clipboard")[38, 1] == 5
  read.delim("clipboard")[66, 5] == "versicolor"
})

#test arguments
write_clipboard(iris[1:5, ], digits = 5)
write_clipboard(iris[1:5, ], clean_names = T)
write_clipboard(iris[1:5, ], clean_names = T, clean_what = "Q")
write_clipboard(iris[1:5, ], print = T)


# lm_best -----------------------------------------------------------------
#fit some models
t = list(lm(Sepal.Length ~ Sepal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, iris))
stopifnot(lm_best(t) == 4)



# lm_beta_matrix df_addNA ----------------------------------------------------------
t = lm_beta_matrix("Petal.Width", colnames(iris)[1:3], data = iris, standardized = T, messages = F)
stopifnot({
  length(t) == 2
  class(t[[2]]) == "lm"
  class(t[[1]]) == "data.frame"
  nrow(t[[1]]) == 7
})

#with missing data
t = lm_beta_matrix("Petal.Width", colnames(iris)[1:3], data = df_addNA(iris), standardized = T, messages = F)
stopifnot({
  length(t) == 2
  class(t[[2]]) == "lm"
  class(t[[1]]) == "data.frame"
  nrow(t[[1]]) == 7
})

# round_df ----------------------------------------------------------------
stopifnot({
  set.seed(1)
  round_df(matrix(rnorm(30), ncol=3))[6, 1] == -0.820
  set.seed(1)
  round_df(matrix(rnorm(30), ncol=3), 0)[6, 1] == -1
})


# semi_par_serial ---------------------------------------------------------
t = semi_par_serial(df = airquality, dependent = "Ozone", primary = "Solar.R", secondaries = colnames(airquality)[3:6])
t = round(t, 2)
stopifnot({
  t[2, 1] == .70
  dim(t) == c(4, 2)
})


# MOD_repeat_glmnet_cv ----------------------------------------------------
#using the iris dataset
t = MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5, messages = F)
stopifnot({
  dim(t) == c(5, 4)
  class(t) == "data.frame"
})

#weights
t_w = MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5, weights_ = runif(nrow(iris)), messages = F)


# MOD_summarize_models ----------------------------------------------------
t2 = MOD_summarize_models(t)
stopifnot({
  class(t2) == "data.frame"
  dim(t2) == c(5, 3)
})


# as_num_matrix -----------------------------------------------------------
t = data.frame(a = 1:3, b = letters[10:12],
              c = seq(as.Date("2004-01-01"), by = "week", len = 3),
              stringsAsFactors = TRUE)
t = as_num_matrix(t)
stopifnot({
  class(t) == "matrix"
  dim(t) == c(3, 3)
})


# Jensen_plot -------------------------------------------------------------
library(psych)
#this extract GFP and checks whether the gender difference is GFP-loaded
t = fa(bfi[1:25])
t2 = cor(bfi, use = "p")
stopifnot({
  g = Jensen_plot(as.numeric(t$loadings), t2[26, 1:25], reverse = T)
  class(g) == c("gg", "ggplot")
  g = Jensen_plot(as.numeric(t$loadings), t2[26, 1:25], reverse = F)
  class(g) == c("gg", "ggplot")
  g = Jensen_plot(as.numeric(t$loadings), t2[26, 1:25], reverse = F, var_names = F)
  class(g) == c("gg", "ggplot")
  g = Jensen_plot(as.numeric(t$loadings), t2[26, 1:25], reverse = F, check_overlap = F)
  class(g) == c("gg", "ggplot")
})



# FA_residuals ------------------------------------------------------------
t = FA_residuals(swiss)
stopifnot({
  dim(t) == c(47, 6)
  class(t) == "data.frame"
})

# FA_MAR ------------------------------------------------------------------
t = FA_MAR(swiss, scores = "Bartlett")
t2 = FA_MAR(swiss)
stopifnot({
  dim(t) == c(47, 1)
  class(t) == "data.frame"
  t != t2
})


# FA_mixedness ------------------------------------------------------------
t = FA_mixedness(swiss)
stopifnot({
  dim(t) == c(47, 4)
  class(t) == "data.frame"
})



# FA_splitsample_repeat ---------------------------------------------------------------------
library(psych)
t = FA_splitsample_repeat(ability, runs = 5, messages = F)
stopifnot({
  class(t) == "data.frame"
  dim(t) == c(5, 1)
})


# GG_scatter --------------------------------------------------------------
#easy scatterplots with ggplot2
library(ggplot2)
mpg_na = df_addNA(mpg) #missing data

l_t = list(t = GG_scatter(mpg, "hwy", "cty"), #test default
           t2 = GG_scatter(mpg_na, "hwy", "cty"), #test with missing data
           t3 = GG_scatter(mpg, "hwy", "cty", case_names = F), #test no case names
           t4 = GG_scatter(mpg, "hwy", "cty", CI = .99), #test CI
           t5 = GG_scatter(mpg, "hwy", "cty", text_pos = "br")) #test position

stopifnot({
  sapply(l_t, function(x) {
    class(x) == c("gg", "ggplot")
  })
})


# Jensens_method ----------------------------------------------------------
library(psych)
t = fa(bfi[1:25])
t2 = Jensens_method(t, bfi, "gender");t2
stopifnot({
  class(t2) == c("gg", "ggplot")
})



# get_spherical_dists -----------------------------------------------------
set.seed(1)
t = data.frame(lat = runif(5, 0, 90),
               lon = runif(5, -180, 180))
t1 = get_spherical_dists(t)
t2 = get_spherical_dists(t, output = "matrix")
stopifnot({
  #test vector
  class(t1) == "numeric"
  length(t1) == 10

  #test matrix
  class(t2) == "matrix"
  dim(t2) == c(5, 5)
})



# get_euclidean_dists -----------------------------------------------------
set.seed(1)
t = data.frame(x = runif(5, 1, 100),
               y = runif(5, 1, 100))
t1 = get_euclidean_dists(t)
t2 = get_euclidean_dists(t, output = "matrix")
stopifnot({
  #test vector
  class(t1) == "numeric"
  length(t1) == 10

  #test matrix
  class(t2) == "matrix"
  dim(t2) == c(5, 5)
})


# get_pairwise_means ------------------------------------------------------
t = get_pairwise_means(1:3)
t2 = get_pairwise_means(1:3, weight_method = "arith")
t3 = get_pairwise_means(1:3, weight_method = "geo")

stopifnot({
  3 == length(t)
  3 == length(t2)
  3 == length(t3)
  t[2] == 1.5
  t2[2] == 2
  t3[2] == sqrt(3)
})


# get_distances -----------------------------------------------------------
library(maps)

#with spherical variables
d = as.data.frame(maps::ozone)
t = get_distances(d, distance_method = "spherical")
t_m = get_distances_mat(df=d, distance_method = "spherical")

stopifnot({
  nrow(t) == 820
  ncol(t) == 3
  colnames(t)[3] == "spatial"
  class(t_m) == "list"
  "spatial" %in% names(t_m)
})

#with euclidean variables
d = data.frame(x = runif(5, 1, 100),
               y = runif(5, 1, 100))
t = get_distances(d)
t_m = get_distances_mat(d)

stopifnot({
  #test vector
  class(t) == "data.frame"
  nrow(t) == 10
  class(t_m) == "list"
  "spatial" %in% names(t_m)
})

#no spatial variables at all
d = data.frame(abc = runif(5, 1, 100),
               def = runif(5, 1, 100))
t = get_distances(d)
t_m = get_distances_mat(d)

stopifnot({
  #test vector
  class(t) == "data.frame"
  nrow(t) == 10
  ncol(t) == 2
  class(t_m) == "list"
  !"spatial" %in% names(t_m)
})


# cor_matrix_weights ------------------------------------------------------
library(datasets)
library(weights)

t = cor_matrix_weights(as.data.frame(state.x77), weight_var = "Population")
stopifnot({
  dim(t) == c(7, 7)
  is.na(diag(t))
  t[upper.tri(t)] != t[lower.tri(t)]
})


# find_neighbors ----------------------------------------------------------
n=50
set.seed(1)
t = data.frame(x = runif(n, 1, 100),
               y = runif(n, 1, 100))
t = find_neighbors(t)
stopifnot({
  length(t) == 50
  class(t) == "list"
  all(sapply(t, function(x) length(x)==3)) #all elements 3 long?
})

n=50
set.seed(1)
t = data.frame(lat = runif(n, -90, 90),
               lon = runif(n, -180, 180))
t = find_neighbors(t)
stopifnot({
  length(t) == 50
  class(t) == "list"
  all(sapply(t, function(x) length(x)==3)) #all elements 3 long?
})


# add_SAC & Morans_I & Morans_I_multi & SAC_knsn_reg & SAC_measures ----------------------------
n=10
set.seed(1)
t0 = data.frame(x = runif(n, 1, 100),
                y = runif(n, 1, 100),
                outcome = rnorm(n),
                test = rep(1:2, n/2))
t0$weightVar = sample(n, n)
t1 = add_SAC(t0, iter = 10, vars = c("outcome", "test"))
t1$weightVar = sample(n, n)

stopifnot({
  class(t0) == class(t1)
  dim(t0) == dim(t1)
})

I0 = get_Morans_I(t0, "outcome")
I1 = get_Morans_I(t1, "outcome")

stopifnot({
  I1$observed > I0$observed
  class(I0) == class(I1)
})

I0 = get_Morans_I_multi(t0, "outcome")
I1 = get_Morans_I_multi(t1, "outcome")

stopifnot({
  I1 > I0
  class(I0) == class(I1)
})

#test correlations
knsn_3_0 = SAC_knsn_reg(t0, "outcome", output = "cor")
knsn_3_1 = SAC_knsn_reg(t1, "outcome", output = "cor")

stopifnot({
  knsn_3_0 < knsn_3_1
})

#test scores
knsn_3_0 = SAC_knsn_reg(t0, "outcome", output = "scores")
knsn_3_1 = SAC_knsn_reg(t1, "outcome", output = "scores")

stopifnot({
  nrow(knsn_3_0) == nrow(knsn_3_0)
  nrow(knsn_3_0) == nrow(t0)
  class(knsn_3_0) == "numeric"
})

#test resids
knsn_3_0 = SAC_knsn_reg(t0, "outcome", output = "resids")
knsn_3_1 = SAC_knsn_reg(t1, "outcome", output = "resids")

stopifnot({
  class(knsn_3_0) == "numeric"
  class(knsn_3_1) == "numeric"
  length(knsn_3_0) == nrow(t0)
  length(knsn_3_1) == nrow(t0)
})

#test resids_cor
knsn_3_0 = SAC_knsn_reg(t0, "outcome", predictor = "test", output = "resids_cor")
knsn_3_1 = SAC_knsn_reg(t1, "outcome", predictor = "test", output = "resids_cor")

stopifnot({
  class(knsn_3_0) == "numeric"
  class(knsn_3_1) == "numeric"
})

#sac measures
t = SAC_measures(df = t1, vars = c("outcome", "test"), k = 3:5)
#with random weights, this is painfully slow!
t_w = SAC_measures(df = t1, vars = c("outcome", "test"), k = 3:5, weights_var = "weightVar")

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(2, 6)
  class(t_w) == "data.frame"
  dim(t_w) == c(2, 6)
})

##test using user-inputted dists
set.seed(1)
dists_y = dist(t1$y) %>% as.matrix
dists_x = dist(t1$x) %>% as.matrix
t_x = SAC_measures(t1, dists = dists_x, vars = c("outcome", "test"), k = 3:5)
t_y = SAC_measures(t1, dists = dists_y, vars = c("outcome", "test"), k = 3:5)

stopifnot({
  class(t_x) == "data.frame"
  dim(t_x) == c(2, 6)
  !all(t_x == t_y, na.rm = T) # they should not be identical
})

#test outcome options
t = SAC_knsn_reg(t1, "outcome", dists=dists_y, output = "scores")
t_cor = SAC_knsn_reg(t1, "outcome", dists=dists_y, output = "cor")
t_resids = SAC_knsn_reg(t1, "outcome", dists=dists_y, output = "resids")

#test other functions
t_xy = find_neighbors(df = t0)
t_x = find_neighbors(dists = dists_x)

t1_xy = add_SAC(t0, vars = "outcome")
t1_x = add_SAC(t0, vars = "outcome", dists = dists_x)



# SAC_slr -----------------------------------------------------------------
#example from "Some methods for analyzing and correcting for spatial autocorrelation"
r_num = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3)
r_vec = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3, output = "vector")

#without the case itself
r_num_self = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3, include_self = T)
r_num2_self = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3, include_self = T)
r_vec_self = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3, output = "vector", include_self = T)
r_vec2_self = SAC_slr(df=t1, dependent = "outcome", predictors = "test", k = 3, output = "vector", include_self = T)


# remove_redundant_vars & remove_redundant_vars2 ----------------------------------------------
t = remove_redundant_vars(longley, 3)

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(16, 4)
})

t = remove_redundant_vars2(longley, .9, messages = F)
e = throws_error("remove_redundant_vars2(longley, .9, messages = 123)")
e2 = throws_error("remove_redundant_vars2(longley, threshold = 5)")

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(16, 3)
  e
  e2
})


# GG_scatter &  Jensens_method --------------------------------------------------------------
library(psych)

g = GG_scatter(longley, "Unemployed", "Armed.Forces");g
g = GG_scatter(longley, "Unemployed", "GNP");g

fa = fa(swiss[-c(3, 5)])
Jensens_method(fa, swiss, "Examination", reverse_factor = T)
Jensens_method(fa, swiss, "Examination", reverse_factor = F)


# MAT_ --------------------------------------------------------------------
#tests whether the original size of the matrix can be correctly determined
stopifnot({
  #without diags
  MAT_find_size(0) == 1
  MAT_find_size(1) == 2
  MAT_find_size(3) == 3
  MAT_find_size(6) == 4
  MAT_find_size(10) == 5

  #with diags
  MAT_find_size(1, T) == 1
  MAT_find_size(3, T) == 2
  MAT_find_size(6, T) == 3
  MAT_find_size(10, T) == 4
  MAT_find_size(15, T) == 5
})

## reconstruct a matrix
#debug matrix
# t = expand.grid(letters[1:5], 1:5) %>% apply(1, function(x) {
#   str_c(x[1], "_", x[2])
# }) %>% matrix(nrow = 5)

#make symmetric matrix for testing
t = dist(1:5, diag = T, upper = T) %>% as.matrix

t_l = t[lower.tri(t)]     #extract halves with and without diagonals
t_l2 = t[lower.tri(t, T)]
t_u = t[upper.tri(t)]
t_u2 = t[upper.tri(t, T)]

#check if it works
stopifnot({
  all(MAT_vector2full(t_l) == t)             #lower without diagonals
  all(MAT_vector2full(t_l2, diag = T) == t)  #lower with diagonals
  all(MAT_vector2full(t_u, byrow = T) == t)            #upper without diagonals
  all(MAT_vector2full(t_u2, diag = T, byrow = T) == t) #upper with diagonals
})

#gets half a matrix and makes it into a full again and back, then compares with the full
stopifnot({
  MAT_vector2full(1:3) %>% MAT_get_half() %>% MAT_vector2full() == MAT_vector2full(1:3)
})


# GG_denhist --------------------------------------------------------------
library(MASS)
library(ggplot2)
library(magrittr)

Sepal_Length = iris$Sepal.Length
g = list(GG_denhist(iris, "Sepal.Length"),
         GG_denhist(Sepal_Length),
         GG_denhist(Sepal_Length, vline = "mean"),
         GG_denhist(Sepal_Length, vline = "median"),
         GG_denhist(Sepal_Length, vline = NULL))

stopifnot({
  sapply(g, function(x) class(x)) %in% c("gg", "ggplot")
})


# plot_loadings_multi FA_rank_fa -----------------------------------------------------
fa_list = list(part1 = fa(iris[1:50, -5]),
               part2 = fa(iris[51:100, -5]),
               part3 = fa(iris[101:150, -5]))
g = plot_loadings_multi(fa_list);g
g_1 = plot_loadings_multi(fa_list, reorder = 1);g_1
g_2 = plot_loadings_multi(fa_list, reorder = 2);g_2
g_3 = plot_loadings_multi(fa_list, reorder = 3);g_3


stopifnot({
  sapply(list(g, g_1, g_2, g_3), function(x) "gg" %in% class(x))
})


# SAC_control -------------------------------------------------
t = SAC_control(df=t1, dependent = "outcome", predictors = "test", knsn_k = 5, slr_k = 3)

#with mr
t3 = SAC_control(df=t1, dependent = "outcome", predictors = "test", knsn_k = 5, slr_k = 3, control_approach = c("partial", "mr"))

#multiple predictors no mr
t5 = SAC_control(df=t1, dependent = "outcome", predictors = c("test", "weightVar"), knsn_k = 5, slr_k = 3, control_approach = c("partial"))
#this is just to see if it returns an error

#multiple predictors + mr
t5 = SAC_control(df=t1, dependent = "outcome", predictors = c("test", "weightVar"), knsn_k = 5, slr_k = 3, control_approach = c("partial", "mr"))


stopifnot({
  class(t) == "data.frame" #check classes
  !is.null(names(t)) #check names are present
  class(t3) == "data.frame" #check classes
})


# MOD_partial -------------------------------------------------------------
#this is a partial correlation function
t = MOD_partial(iris, "Sepal.Length", "Sepal.Width", "Petal.Length")
t2 = MOD_partial(iris, "Sepal.Width", "Sepal.Length", "Petal.Length")

stopifnot({
  all.equal(t, t2)
})


# as_num_df ---------------------------------------------------------------
#converts string vectors in a data.frame to numeric ones if possible
#make iris into a df with strings
iris_chr = lapply(iris, as.character) %>% as.data.frame(stringsAsFactors = F)
#change it back to numerics
t = as_num_df(iris_chr)
#check that back to factor works
t2 = as_num_df(iris_chr, stringsAsFactors = T)
#check that skip factors works
t3 = as_num_df(iris, skip_factors = F)
#check that it handles NAs
iris_chr_NA = iris_chr;iris_chr_NA[1, 1] = NA
t4 = as_num_df(iris_chr_NA)

stopifnot({
  all(sapply(t, class) == c("numeric", "numeric", "numeric", "numeric", "character"))
  all(sapply(t2, class) == c("numeric", "numeric", "numeric", "numeric", "factor"))
  all(sapply(t3, class) == c("numeric", "numeric", "numeric", "numeric", "numeric"))
  all(sapply(t4, class) == c("numeric", "numeric", "numeric", "numeric", "character"))
})



# df_add_delta ------------------------------------------------------------
#this function adds delta (difference) variables to a df, in a semi-intelligent fashion

#these should work
t_list = list(df_add_delta(iris, primary_var = 1),
              df_add_delta(iris, primary_var = "Sepal.Length"),
              df_add_delta(iris, primary_var = pi),
              df_add_delta(iris, primary_var = 1, standardize = T))


#errors
e_list = list(try({df_add_delta(iris, primary_var = 1, secondary_vars = 1:4)}, T),
              try({df_add_delta(iris, primary_var = 0)}, T),
              try({df_add_delta(iris, primary_var = 1, secondary_vars = 99)}, T),
              try({df_add_delta(iris, primary_var = 1, secondary_vars = -1:1)}, T))

stopifnot({
  sapply(t_list, class) == rep("data.frame", length(t_list))
  sapply(e_list, class) == rep("try-error", length(e_list))
})



# df_func ------------------------------------------------------------------------

#tests
t_list = list(
  df_func(iris[1:4]),
  df_func(iris[1], iris[2], iris[3], iris[4]),
  df_func(iris[1:4], func = median),
  df_func(iris[1:4], standardize = T),
  df_func(iris[1:4], standardize = T, func = median),
  df_func(iris, pattern = "al"),
  df_func(iris, pattern = "al", func = median, standardize = T)
)

#errors
e_list = list(
  try({df_func(iris)}, T),
  try({df_func(iris, pattern = "sadaiasd")}, T)
)

#check
stopifnot({
  sapply(t_list, class) == rep("data.frame", length(t_list))
  sapply(e_list, class) == rep("try-error", length(e_list))
})


# sort_df ---------------------------------------------------------------
t1 = sort_df(iris, "Sepal.Length")
t2 = sort_df(iris, "Sepal.Length", decreasing = T)
t3 = sort_df(iris, 1)
t4 = sort_df(iris, 2)
t5 = sort_df(iris, 5)

stopifnot({
  cor(t1$Sepal.Length, t2$Sepal.Length) < -.9
})


# score_items -------------------------------------------------------------
#scores test items
library(psych) #data in here

t = score_items(iqitems, c(4,4,4, 6,  6,3,4,4,   5,2,2,4,   3,2,6,7))

stopifnot({
  dim(t) == c(1525, 16)
  class(t) == "data.frame"
  (cor(t, use = "p") > 0) %>% all #all cors positive
})



# GG_group_means -----------------------------------------------------------
#this the plot means function
l_t = list(GG_group_means(iris, "Sepal.Length", "Species"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "point"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "points"),
           GG_group_means(iris, "Sepal.Length", "Species", type = "points", CI = .999999))

stopifnot({
  sapply(l_t, function(x) "ggplot" %in% class(x))
})



# percent_cutoff ----------------------------------------------------------
#test cutoff function

stopifnot({
  percent_cutoff(iris$Sepal.Length, cutoffs = c(4, 8)) == c(1, 0)
  percent_cutoff(iris$Sepal.Length, cutoffs = 5, digits = 2) == .79
})


# score_accuracy -------------------------------------------------
library(stringr)
#simulate some data
n_cases = 10
n_countries = 100

#random guesses
set.seed(2)
{
d_randomguesses = matrix(runif(n_cases * n_countries, 0, 100), ncol = n_countries) %>% as.data.frame()
v_randomguesses = runif(n_countries, 0, 100)
}

#random true values
set.seed(1)
v_criteria = runif(n_countries, 0, 100)

#with missing values
d_randomguesses_na = df_addNA(d_randomguesses)

#score
l_t = list(t = score_accuracy(d_randomguesses, v_criteria, methods = "all"),
           t2 = score_accuracy(d_randomguesses_na, v_criteria, methods = "all"),
           t3 = score_accuracy(d_randomguesses, v_criteria, methods = "all", aggregate = T),
           t4 = score_accuracy(v_randomguesses, v_criteria, methods = "all"))

#tests
stopifnot({
  #test class
  sapply(l_t, function(x) class(x) == "data.frame")
  #test not the same
  !all(cor(l_t$t) == cor(l_t$t, use = "p"))
})


# simple is_ --------------------------------------------------------
#various small helpful functions

#is_simple_vector
stopifnot({
  #try list
  is.vector(list(1:3))
  is.list(list(1:3))
  !is_simple_vector(list(1:3))

  #test vector
  is.vector(1:3)
  !is.list(1:3)
  is_simple_vector(1:3)
})

#is_whole_number
stopifnot({
  is_whole_number(seq(0, 2, .5)) == c(T, F, T, F, T)
})

#is_negative, is_positive
stopifnot({
  is_negative(-2:2) == c(T, T, F, F, F)
  is_positive(-2:2) == c(F, F, F, T, T)
})


# is_error, throws_error() ------------------------------------------------
#test error functions

trial = try({1/"1"}, silent = T)

stopifnot({
  #first
  is_error(trial)
  !is_error(15)

  #second
  !throws_error("1==1")
  !throws_error("1 > 2")
  throws_error("!1!")
  throws_error("1><>2")
  throws_error("7///7")
})


# conditional_change ------------------------------------------------------
#a complement to plyr's mapvalues.

stopifnot({
  #tests
  conditional_change(1:10, func_str = "<5", new_value = NA) %>% count_NA == 4
  conditional_change(1:10, func_str = "> 9", new_value = NA) %>% count_NA == 1
  conditional_change(data.frame(1:10), func_str = "> 9", new_value = NA) %>% count_NA == 1
  conditional_change(list(1:10, 1:10), func_str = "> 9", new_value = NA) %>% sapply(is.na) %>% sum == 2
  conditional_change(matrix(1:9, nrow = 3), func_str = "> 5", new_value = NA) %>% count_NA == 4
  conditional_change(c(1:10, NA), func_str = "<5", new_value = NA) %>% count_NA == 5

  #errors
  throws_error('conditional_change(1:10, func_str = "!1!", new_value = NA)')
  throws_error('conditional_change(is.array, func_str = "<0", new_value = NA)')
})


# math_to_function --------------------------------------------------------
#converts a string with math to a function
stopifnot({
  #tests
  sapply(-1:1, math_to_function("<0")) == c(T, F, F)
  sapply(-1:1, math_to_function(">0")) == c(F, F, T)
  sapply(-1:1, math_to_function("=0")) == c(F, T, F)
  sapply(-1:1, math_to_function("==0")) == c(F, T, F)
  sapply(-1:1, math_to_function("=5")) == rep(FALSE, 3)

  #errors
  throws_error("math_to_function('!4!4!4')(1)")
})


# missing data handling ---------------------------------------------------

stopifnot({
  count_NA(c(1:10, rep(NA, 5), 1:10)) == 5
})


# str_replace_multi, str_clean --------------------------------------------
stopifnot({
  str_clean("Long_Test.String.") == "Long Test String"
  str_replace_multi("Long_Test.String", c("_", "\\."), " ") == "Long Test String"
})



# silence ---------------------------------------------------------------
#not sure how to test these, but they should not give errors

suppressor(log(-1))
suppressor(warning("test"))
suppressor(warning("test"), warnings = T)
suppressor(message("test"))
suppressor(message("test"), messages = T)



# get_dims --------------------------------------------------------------
#a better version of dim() from base-r

stopifnot({
  get_dims(1:2) == 2
  get_dims(list(1, 2)) == 2
  get_dims(matrix(1:4, nrow=2)) == c(2, 2)
  matrix(1:4, nrow=2) %>% as.data.frame %>% get_dims == c(2, 2)
  array(1:16, dim = c(2, 2, 2)) %>% dim == c(2, 2, 2)
})


# copy_names --------------------------------------------------------------
#copy names from one object to another, partially if possible

#make two 3x3 mats and copy from one to the other
t = matrix(1:9, nrow=3)
t2 = t
rownames(t) = LETTERS[1:3]; colnames(t) = letters[1:3]
copy_names(t, t2)

stopifnot({
  rownames(t) == rownames(t2)
  colnames(t) == colnames(t2)
})


#partiamatching test, copy from 3x2 to 3x3
t = matrix(1:6, nrow=3)
t2 = matrix(1:9, nrow=3)
rownames(t) = LETTERS[1:3]; colnames(t) = letters[1:2]

copy_names(t, t2)

stopifnot({
  rownames(t) == rownames(t2)
})


#test error, try to do perfect match copy on non-perfect match
t2 = matrix(1:9, nrow=3)
t = matrix(1:6, nrow=3)
rownames(t) = LETTERS[1:3]; colnames(t) = letters[1:2]

stopifnot({
  throws_error("copy_names(t, t2, F)")
})



# is_between --------------------------------------------------------------
#tests whether values are between two limits.

stopifnot({
  #scalars
  is_between(5, 0, 10)
  is_between(5, 5, 5)
  !is_between(1, 5, 5)

  #vectors
  is_between(0:10, 0, 10)
  !is_between(-10:-1, 0, 10)

  #test parameters
  !is_between(1, 1, 2, include_lower = F)
  !is_between(2, 1, 2, include_upper = F)
  !is_between(1, 1, 1, include_upper = F, include_lower = F)
})



# fill_in -----------------------------------------------------------------
#fills in values so that a vector reaches a desired length.

stopifnot({
  fill_in(1:5, 10) %>% count_NA == 5
  fill_in(1:5, 5) %>% count_NA == 0
  fill_in(1:5, 10, value = -1) == c(1:5, rep(-1, 5))
})


# named_vectors_to_df -----------------------------------------------------------------------
#inputs a list of vectors with names, outputs a data.frame

l = list(A = c(a = 1, b = 2, c = 3), B = c(a = 3, b = 2, c = 1))
l2 = list(A = c(a = 1, b = 2, c = 3), B = c(a = 3, b = 2))

stopifnot({
  named_vectors_to_df(l) %>% dim == c(3, 4)
  named_vectors_to_df(l2) %>% dim == c(3, 4)
})


# get_salient_loadings ----------------------------------------------------
#returns either a list or data.frame with the salient loadings of each factor from a factor analysis.

library("psych")
library("magrittr")
fa_iris1 = fa(iris[-5])
fa_iris2 = fa(iris[-5], 2)

stopifnot({
  get_salient_loadings(fa_iris1) %>% dim == c(4, 2)
  get_salient_loadings(fa_iris2) %>% dim == c(3, 4)
})



# split_every_k -----------------------------------------------------------
#split a vector every k element

stopifnot({
  all.equal(split_every_k(1:12, 4), list(1:4, 5:8, 9:12), check.names = F)
  all.equal(split_every_k(1:11, 4), list(1:4, 5:8, 9:11), check.names = F)
  all.equal(split_every_k(1:4, 4), list(1:4), check.names = F)
  throws_error("split_every_k(1:11, 4, uneven = F)")
})

# stack_into_n_columns ----------------------------------------------------
#useful function for stacking data that is too wide for the document or screen.

df = split_every_k(1:12, 2) %>% as.data.frame

stopifnot({
  stack_into_n_columns(df, 2) %>% dim == c(11, 2)
  stack_into_n_columns(df, 3) %>% dim == c(7, 3)
  silence(stack_into_n_columns(df, 4) %>% dim == c(7, 4))
})



# alternate ---------------------------------------------------------------
#intertwine vectors

stopifnot({
  alternate(list(1:3, letters[1:3])) == c("1", "a", "2", "b", "3", "c")
  alternate(list(1:3, letters[1:3], LETTERS[1:3])) == c("1", "a", "A", "2", "b", "B", "3", "c", "C")
  throws_error("alternate(list(1:2, letters[1:3]))")
  throws_error("alternate(c(1:2, 2:1))")
})


# df_residualize ----------------------------------------------------------

library("magrittr")

set.seed(1)
df = data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5))

stopifnot({
  #test basic function
  df_residualize(df, resid.vars = "c", print.models = F) %>% extract(c("a", "b")) != df[c("a", "b")]
  #test suffix + message off
  df_residualize(df, resid.vars = "c", suffix = "_r", print.models = F) %>% colnames() != colnames(df)
  #test exclusion vector
  df_residualize(df, resid.vars = "c", exclude_vars = "b", print.models = F) %>% extract("b") == df$b
  #test return
  df_residualize(df, resid.vars = "c", return.resid.vars = F, print.models = F) %>% colnames != "c"
})


# filter_by_missing_values ------------------------------------------------
#filters data by number of missing values per case

df = data.frame(1:10, letters[1:10])
set.seed(1)
df = df_addNA(df)

stopifnot({
  filter_by_missing_values(df) %>% nrow %>% equals(8)
})



# extract_num_vars --------------------------------------------------------

stopifnot({
  extract_num_vars(iris) == iris[-5]
  extract_nonnum_vars(iris) == iris[5]
})


# copy_columns ------------------------------------------------------------
#copy cols from one df to another
library(magrittr)

stopifnot({
  #copy all using default
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150))) %>% dim %>% equals(c(150, 6))

  #copy all using pattern
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150)), pattern = ".") %>% dim %>% equals(c(150, 6))

  #copy those with Length in name (2)
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150)), pattern = "Length") %>% dim %>% equals(c(150, 3))

  #copy one specific column
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150)), columns = "Species") %>% dim %>% equals(c(150, 2))

  #copy two specific cols
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150)), columns = c("Species", "Sepal.Length")) %>% dim %>% equals(c(150, 3))

  #copy using numbers
  copy_columns(from = iris, to = data.frame(rnorm = rnorm(150)), columns = 1:3) %>% dim %>% equals(c(150, 4))
})

# is_numeric is_numeric_by_col --------------------------------------------------------------

stopifnot({
  #
  is_numeric_by_col(iris)

  #
  is_numeric(1:3)
  !is_numeric("123")
  !is_numeric(iris)
  is_numeric(iris[-5])
  !is_numeric(iris[-5], F)
  !is_numeric(T)
  is_numeric(array(1:8, dim = rep(2, 3)))
  is_numeric(list(1, 2, 3))
  is_numeric(list(1, 2, 3), F)
  is_numeric(list(list(1, 2, 3), 5, list(1, 2, 3), 4, list(list(10))))
})



# format_digits -----------------------------------------------------------
#useful for outputting data

stopifnot({
  format_digits(c(.1), 2) == "0.10"
  format_digits(c(.1), 5) == "0.10000"
  format_digits(c(.12345), 2) == "0.12"
  format_digits(c(.15555), 2) == "0.16"
})


# cor_matrix --------------------------------------------------------------
#correlation matrix with nice output
library(weights);library(magrittr)

stopifnot({
  are_equal(cor_matrix(iris[-5]), cor(iris[-5])) #validate vs. base-r
  are_equal(cor_matrix(iris), cor(iris[-5])) #automatic skip non-numeric
  are_equal(cor_matrix(iris[-5], weights = rep(1, 150)), cor(iris[-5])) #with weights
  !are_equal(cor_matrix(iris[-5], weights = runif(150)), cor(iris[-5])) #other weights
  !are_equal(cor_matrix(iris[-5], weights = iris[-5]), cor(iris[-5])) #complex weights
  cor_matrix(iris, CI = .95) %>% is.character #with CI
  cor_matrix(iris, CI = .95) %>% get_dims %>% equals(c(4, 4)) #verify dims
  cor_matrix(iris[-5], weights = iris[-5], CI = .95) %>% is.character #complex weights + CI
})


# SMD_matrix & pooled_sd --------------------------------------------------------------
#standardized mean differences
library(magrittr)

#test parameters
t = list(SMD_matrix(iris$Sepal.Length, iris$Species),
         SMD_matrix(iris$Sepal.Length, iris$Species, central_tendency = median),
         SMD_matrix(iris$Sepal.Length, iris$Species, dispersion = "mad"),
         SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "pair"),
         SMD_matrix(iris$Sepal.Length, iris$Species, dispersion_method = "total"))

stopifnot({
  sapply(t, is.matrix) #all matrices
  unique(t) %>% length %>% equals(5) #all different
})



# merge_rows, merge_rows_by_name --------------------------------------------------------------
#performs row-wise merging

t = data.frame(id = c("a", "a", "b", "b", "c"), value = 1:5)
t_true = data.frame(id = c("a", "b", "c"), value = c(3, 7, 5))
t_true2 = data.frame(id = c("a", "b", "c"), value = c(1.5, 3.5, 5))

stopifnot({
  merge_rows(t, "id") == t_true #test string input
  merge_rows(t, "id", func = mean) == t_true2 #test another function
  throws_error("merge_rows(t, 'id', numeric = FALSE)") #test error
})

#do it by name
t1 = data.frame(X = c(1, 2, 3, NA), Y = c(1, 2, NA, 3));rownames(t1) = LETTERS[1:4]
t1_cor = data.frame(X = c(1, 2, 3), Y = c(1, 2, 3));rownames(t1_cor) = LETTERS[1:3]

stopifnot({
  merge_rows_by_name(df = t1, names = c("C", "D"), func = mean) == t1_cor
  #another new_name
  merge_rows_by_name(df = t1, names = c("C", "D"), func = mean, new_name = "D") %>% rownames %>% equals(c("A", "B", "D"))
  #sum instead (no difference in this case)
  merge_rows_by_name(df = t1, names = c("C", "D"), func = sum) == t1_cor
})


# extract_last ------------------------------------------------------------

stopifnot({
  extract_last(iris, 1) == tail(iris, 1)
  extract_last(iris, 10:1) == tail(iris, 10)
  extract_last(iris, seq(10, 2, by = -2)) == iris[(nrow(iris)+1) - seq(10, 2, by = -2), ]
  extract_last(iris, c(20, 15, 5, 1)) == iris[(nrow(iris)+1) - c(20, 15, 5, 1), ]
  extract_last(iris, , 1) == iris[5]
  extract_last(iris, , 2:1) == iris[4:5]
  extract_last(iris, 10:1, 1) == iris[141:150, 5, drop = FALSE]
  extract_last(letters, 1) == rev(letters)[1]
  extract_last(letters, 5:1) == rev(letters)[5:1]
})


# subset_by_pattern -------------------------------------------------------
#subset using regex for column names

stopifnot({
  subset_by_pattern(iris, "Length") == iris[c(1, 3)] # length columns
  subset_by_pattern(iris, "Length", T)  == iris[-c(1, 3)] # non-length columns
  subset_by_pattern(iris, "Species")  == iris[5] # species, 1 col
})


# merge_vectors -----------------------------------------------------------

t1 = c(NA, 2, 3, NA)
t2 = c(1, NA, NA, 4)
t1_n = c(NA, 2, 3, NA);names(t1_n) = letters[1:4]
t2_n = c(1, NA, NA, 4);names(t2_n) = letters[1:4]
t3_n = c(1, NA, NA, 4, 5);names(t3_n) = letters[1:5]


stopifnot({
  merge_vectors(t1, t2) == 1:4
  throws_error("merge_vectors(t1, t2, byname = T)")
  merge_vectors(t1_n, t2_n, byname = T) == 1:4
  are_equal(merge_vectors(t1_n, t2_n, byname = T, overwrite_NA = T), c(1, NA, NA, 4), check.names = F)
  merge_vectors(t1_n, t3_n, byname = T) == 1:5
})


# discretize --------------------------------------------------------------

x1 = discretize(rnorm(100), 5)
x2 = discretize(rnorm(100), 5, equal_range = F)
x3 = discretize(rnorm(100), 5, labels = "intervals")

stopifnot({
  sapply(list(x1, x2, x3), function(x) length(unique(x)) == 5)
})


# mean_abs_diff -----------------------------------------------------------

stopifnot({
  (mean_abs_diff(iris[[1]]) - 0.9461924) < 1e-6
})



# exclude_missing ---------------------------------------------------------
#exclude a broad array of missing data types, with specificity

x = list(1, NA, 2, NULL, 3, NaN, 4, Inf)

stopifnot({
  are_equal(exclude_missing(x), list(1, 2, 3, 4))
  are_equal(exclude_missing(x, .NA = F), list(1, NA, 2, 3, 4))
  are_equal(exclude_missing(x, .NULL = F), list(1, 2, NULL, 3, 4))
  are_equal(exclude_missing(x, .NaN = F), list(1, 2, 3, NaN, 4))
  are_equal(exclude_missing(x, .Inf = F), list(1, 2, 3, 4, Inf))
})

# done --------------------------------------------------------------------

message("DONE! If you see this, there were no errors. Hopefully!")
