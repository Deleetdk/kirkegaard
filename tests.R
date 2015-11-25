#make datasets
source("kirkegaard/datasets.R")

#otherwise get error
options("expressions" = 10000)

# merge_datasets ----------------------------------------------------------
#some data to merge
d1 = iris[1:75,] #split in two
d2 = iris[76:150,]
t = merge_datasets(d1, d2) #merge into one
stopifnot(all(iris == t)) #they should be equal again


# FA_all_methods & FA_congruence_mat --------------------------------------------------------
t = FA_all_methods(iris[-5], skip_methods = "pa")

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
  t = std_df(iris)
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



# lm_best -----------------------------------------------------------------
#fit some models
t = list(lm(Sepal.Length ~ Sepal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, iris))
stopifnot(lm_best(t) == 4)



# lm_beta_matrix ----------------------------------------------------------
t = lm_beta_matrix("Petal.Width", colnames(iris)[1:3],data = iris,standardized = T)
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
t = MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5)
stopifnot({
  dim(t) == c(5, 4)
  class(t) == "data.frame"
})


# MOD_summarize_models ----------------------------------------------------
t2 = MOD_summarize_models(t)
stopifnot({
  class(t2) == "data.frame"
  dim(t2) == c(4, 3)
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
t = FA_splitsample_repeat(ability, runs = 5)
stopifnot({
  class(t) == "data.frame"
  dim(t) == c(5, 1)
})


# GG_scatter --------------------------------------------------------------
stopifnot({
  t = GG_scatter(mpg, "hwy", "cty")
  class(t) == c("gg", "ggplot")
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
library(fields)

#with spherical variables
d = as.data.frame(ozone)
t = get_distances(d, distance_method="spherical")
t_m = get_distances_mat(df=d, distance_method="spherical")

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
n=500
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
t_x = SAC_measures(t1, dists = dists_x, vars = c("outcome", "test"), k = 3:5);t_x
t_y = SAC_measures(t1, dists = dists_y, vars = c("outcome", "test"), k = 3:5);t_y

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
r_num = SAC_slr(df=d_ex5, dependent = "outcome", predictors = "predictor", k = 3)
r_num2 = SAC_slr(df=d_ex6, dependent = "outcome", predictors = "predictor", k = 3)
r_vec = SAC_slr(df=d_ex5, dependent = "outcome", predictors = "predictor", k = 3, output = "vector")
r_vec2 = SAC_slr(df=d_ex6, dependent = "outcome", predictors = "predictor", k = 3, output = "vector")

#without the case itself
r_num_self = SAC_slr(df=d_ex5, dependent = "outcome", predictors = "predictor", k = 3, include_self = T)
r_num2_self = SAC_slr(df=d_ex6, dependent = "outcome", predictors = "predictor", k = 3, include_self = T)
r_vec_self = SAC_slr(df=d_ex5, dependent = "outcome", predictors = "predictor", k = 3, output = "vector", include_self = T)
r_vec2_self = SAC_slr(df=d_ex6, dependent = "outcome", predictors = "predictor", k = 3, output = "vector", include_self = T)

stopifnot({
  r_num2 > r_num
  nrow(r_vec) == nrow(r_vec2)
  nrow(r_vec) == nrow(d_ex5)
})

# remove_redundant_vars & remove_redundant_vars2 ----------------------------------------------
t = remove_redundant_vars(longley, 3)

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(16, 4)
})

t = remove_redundant_vars2(longley, .9)

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(16, 3)
})


# # SAC_SEVM ----------------------------------------------------------------
# t = SAC_SEVM(d_ex5, c("predictor", "outcome"))


# GG_scatter &  Jensens_method --------------------------------------------------------------
g = GG_scatter(longley, "Unemployed", "Armed.Forces");g
g = GG_scatter(longley, "Unemployed", "GNP");g
library(psych)
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
set.seed(1)
t = dist(sample(100, 5)) %>% as.matrix #make a symmetric matrix
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
g = GG_denhist(Boston, "rm")

stopifnot({
  all(class(g) == c("gg", "ggplot"))
})


# plot_loadings_multi FA_rank_fa -----------------------------------------------------
fa_list = list(std = fa(iris[-5]),
               rank = FA_rank_fa(iris[-5]))
g = plot_loadings_multi(fa_list)

stopifnot({
  "gg" %in% class(g)
})

# SAC_control -------------------------------------------------
t = SAC_control(df=d_ex5, dependent = "outcome", predictors = "predictor", knsn_k = 10, slr_k = 3)
t2 = SAC_control(df=d_ex6, dependent = "outcome", predictors = "predictor", knsn_k = 10, slr_k = 3)

#with mr
t3 = SAC_control(df=d_ex5, dependent = "outcome", predictors = "predictor", knsn_k = 10, slr_k = 3, control_approach = c("partial", "mr"))
t4 = SAC_control(df=d_ex6, dependent = "outcome", predictors = "predictor", knsn_k = 10, slr_k = 3, control_approach = c("partial", "mr"))

#multiple predictors no mr
t5 = SAC_control(df=d_ex5, dependent = "outcome", predictors = c("predictor", "random"), knsn_k = 10, slr_k = 3, control_approach = c("partial"))
#this is just to see if it returns an error

#multiple predictors + mr
t5 = SAC_control(df=d_ex5, dependent = "outcome", predictors = c("predictor", "random"), knsn_k = 10, slr_k = 3, control_approach = c("partial", "mr"))
t6 = SAC_control(df=d_ex6, dependent = "outcome", predictors = c("predictor", "random"), knsn_k = 10, slr_k = 3, control_approach = c("partial", "mr"))


stopifnot({
  all(t2 > t) #those from 6 should be larger
  class(t) == "data.frame" #check classes
  class(t2) == "data.frame"
  !is.null(names(t)) #check names are present
  !is.null(names(t2))

  #with mr
  class(t3) == "data.frame" #check classes
  class(t4) == "data.frame"
  all(dim(t3) == dim(t4))
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

