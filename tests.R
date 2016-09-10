# some libs ---------------------------------------------------------------
library(pacman)
p_load(kirkegaard, psych, plyr, stringr, MASS, assertthat)

#otherwise get error
options("expressions" = 10000)

# merge_datasets + merge_datasets2 ----------------------------------------------------------
#some data to merge
d1 = iris[1:75, ] #split in two
d2 = iris[76:150, ]
set.seed(1);d2_na = miss_add_random(d2)
t = merge_datasets(d1, d2) #merge into one
t2 = silence(merge_datasets(d1, d2, join = "left"))
t3 = silence(merge_datasets(d1, d2, join = "right"))
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
t2 = silence(merge_datasets2(d1, d2, join = "left"))
t3 = silence(merge_datasets2(d1, d2, join = "right"))
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
set.seed(1)
l_t = list(t1 = std_df(iris, messages = F),
           t2 = std_df(iris, exclude_factors = F, messages = F),
           t3 = std_df(iris, w = runif(150), messages = F))

stopifnot({
  #inequalities
  head(l_t$t1[-5]) != head(iris)[-5]
  head(l_t$t2) != head(iris)
  l_t$t1[-5] != l_t$t3[-5]

  #numeric
  are_equal(sapply(l_t$t2, sd), rep(1, 5), check.names = F)
  !are_equal(sapply(l_t$t3[-5], sd), rep(1, 4), check.names = F)
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
#skip these tests on linux
if (!Sys.info()['sysname'] == "Linux") {
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

  #write NAs
  write_clipboard(miss_add_random(iris))

  stopifnot({
    read.delim("clipboard") %>% count_NA() != 0 #make sure there are NAs in the output too
  })
}



# MOD_k_fold_r2 --------------------------------------------------------------

fit = lm("Petal.Length ~ Species", data = iris)

stopifnot({
  round(MOD_k_fold_r2(fit), 5) - round(c(0.9413717, 0.9380666), 5) == 0
})


# lm_best MOD_summary -----------------------------------------------------------------
#fit some models
t = list(lm(Sepal.Length ~ Sepal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris),
         lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, iris))
stopifnot(lm_best(t) == 4)

#fit two models
fit1 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris)
fit2 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris %>% std_df(messages = F))

#weights
v_weights = runif(150, 1, 10)
fit3 = lm("Sepal.Length ~ Sepal.Width + Petal.Length", iris, weights = v_weights)
fit3_std = silence(lm("Sepal.Length ~ Sepal.Width + Petal.Length", std_df(iris), weights = v_weights))
fit4 = lm(formula = "Sepal.Length ~ Species + Sepal.Width + Petal.Width + Petal.Length", data = iris)
fit4_std = lm(formula = "Sepal.Length ~ Species + Sepal.Width + Petal.Width + Petal.Length", data = std_df(iris, messages = F))
fit5 = lm(formula = "Sepal.Length ~ Species + Sepal.Width + Petal.Width + Petal.Length", data = iris, weights = 1:150)
fit5_std = lm(formula = "Sepal.Length ~ Species + Sepal.Width + Petal.Width + Petal.Length", data = std_df(iris, messages = F), weights = 1:150)

#missing data
fit6 = lm("Petal.Length ~ Sepal.Width", iris) %>% MOD_summary()
fit6_miss = lm("Petal.Length ~ Sepal.Width", miss_add_random(iris)) %>% MOD_summary()
fit6_miss_wtd = lm("Petal.Length ~ Sepal.Width", miss_add_random(iris), weight = Sepal.Length) %>% MOD_summary()

#glm
fit7_c = glm("Sepal.Length ~ Sepal.Width + Species", iris, family = gaussian())
fit7_c_std = glm("Sepal.Length ~ Sepal.Width + Species", std_df(iris, messages = F), family = gaussian())

iris2 = mutate(iris, virginica = as.factor(Species == "virginica"))
fit7_d = glm("virginica ~ Sepal.Width + Sepal.Length", iris2, family = binomial())
fit7_d_std = glm("virginica ~ Sepal.Width + Sepal.Length", std_df(iris2, messages = F), family = binomial())

stopifnot({
  #then we test and make sure all the numbers are right
  MOD_summary(fit1, standardize = F)$coefs$Beta == round(fit1$coefficients[-1], 2) #unstd. data, don't std. betas
  MOD_summary(fit1, standardize = T)$coefs$Beta == c(.31, 1.01) #unstd. data, std. betas
  MOD_summary(fit2, standardize = F)$coefs$Beta == c(.31, 1.01) #std data., don't std. betas
  MOD_summary(fit2, standardize = T)$coefs$Beta == c(.31, 1.01) #std data., std. betas

  #weights
  MOD_summary(fit3)$coef == MOD_summary(fit3_std, standardize = F)$coef

  #factor variable
  MOD_summary(fit4)$coefs == MOD_summary(fit4_std, standardize = F)$coefs

  #factor variable and weights
  MOD_summary(fit5)$coefs == MOD_summary(fit5_std, standardize = F)$coefs

  #glm
  all(MOD_summary(fit7_c)$coefs == MOD_summary(fit7_c_std, standardize = F)$coefs, na.rm=T)
  all(MOD_summary(fit7_d)$coefs == MOD_summary(fit7_d_std, standardize = F)$coefs, na.rm=T)
})

#check old name
stopifnot(are_equal(lm_CI, MOD_summary))


# MOD_APSLM  ----------------------------------------------------------
t = silence(MOD_APSLM("Petal.Width", colnames(iris)[1:3], data = iris, standardized = T, messages = F))
stopifnot({
  length(t) == 2
  class(t[[2]]) == "lm"
  class(t[[1]]) == "data.frame"
  nrow(t[[1]]) == 7
})

#with missing data
t = silence(MOD_APSLM("Petal.Width", colnames(iris)[1:3], data = miss_add_random(iris), standardized = T, messages = F))
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
t = silence(semi_par_serial(df = airquality, dependent = "Ozone", primary = "Solar.R", secondaries = colnames(airquality)[3:6]))
t = round(t, 2)
stopifnot({
  t[2, 1] == .70
  dim(t) == c(4, 2)
})


# MOD_repeat_glmnet_cv ----------------------------------------------------
#using the iris dataset
t = silence(MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5, messages = F))
stopifnot({
  dim(t) == c(5, 4)
  class(t) == "data.frame"
})

#weights
t_w = silence(MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5, weights_ = runif(nrow(iris)), messages = F))


# MOD_summarize_models ----------------------------------------------------
t2 = MOD_summarize_models(t)
stopifnot({
  class(t2) == "data.frame"
  dim(t2) == c(5, 3)
})


# as_num_matrix -----------------------------------------------------------
t = data.frame(a = 1:3,
               b = letters[10:12],
               c = seq(as.Date("2004-01-01"), by = "week", len = 3),
               stringsAsFactors = TRUE)
t2 = as_num_matrix(t)

stopifnot({
  class(t2) == "matrix"
  dim(t2) == c(3, 4)
})


# Jensen_plot -------------------------------------------------------------
p_load(psych)
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
t = silence(FA_splitsample_repeat(ability, runs = 5, messages = F))
stopifnot({
  class(t) == "data.frame"
  dim(t) == c(5, 1)
})


# GG_scatter --------------------------------------------------------------
#easy scatterplots with ggplot2
library(ggplot2)
mpg_na = miss_add_random(mpg) #missing data

l_t = silence(list(t = GG_scatter(mpg, "hwy", "cty"), #test default
           t2 = GG_scatter(mpg_na, "hwy", "cty"), #test with missing data
           t3 = GG_scatter(mpg, "hwy", "cty", case_names = F), #test no case names
           t4 = GG_scatter(mpg, "hwy", "cty", CI = .99), #test CI
           t5 = GG_scatter(mpg, "hwy", "cty", text_pos = "br")) #test position
)

stopifnot({
  sapply(l_t, function(x) {
    class(x) == c("gg", "ggplot")
  })
})


# GG_contingency_Table ----------------------------------------------------
t = list(GG_contingency_table(mpg, "drv", "cyl"),
         GG_contingency_table(mpg, "drv", "cyl", margin = 1),
         GG_contingency_table(mpg, "drv", "cyl", margin = 2))

stopifnot({
  sapply(t, is.ggplot)
})



# Jensens_method ----------------------------------------------------------
p_load(psych)
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
p_load(maps)

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
p_load(datasets)
p_load(weights)

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
  dim(t) == c(2, 4)
  class(t_w) == "data.frame"
  dim(t_w) == c(2, 4)
})

##test using user-inputted dists
set.seed(1)
dists_y = dist(t1$y) %>% as.matrix
dists_x = dist(t1$x) %>% as.matrix
t_x = SAC_measures(t1, dists = dists_x, vars = c("outcome", "test"), k = 3:5)
t_y = SAC_measures(t1, dists = dists_y, vars = c("outcome", "test"), k = 3:5)

stopifnot({
  class(t_x) == "data.frame"
  dim(t_x) == c(2, 4)
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
p_load(psych)

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
p_load(MASS)
p_load(ggplot2)
p_load(magrittr)

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
library(psych)
fa_list = list(part1 = fa(iris[1:50, -5]),
               part2 = fa(iris[51:100, -5]),
               part3 = fa(iris[101:150, -5]))
#multianalysis plots, different orderings
g = plot_loadings_multi(fa_list);g
g_1 = plot_loadings_multi(fa_list, reorder = 1);g_1
g_2 = plot_loadings_multi(fa_list, reorder = 2);g_2
g_3 = plot_loadings_multi(fa_list, reorder = 3);g_3
#monoanalysis
g_4 = plot_loadings_multi(fa_list[[1]]);g_4

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


# df_as_num ---------------------------------------------------------------
#converts string vectors in a data.frame to numeric ones if possible
#make iris into a df with strings
iris_chr = lapply(iris, as.character) %>% as.data.frame(stringsAsFactors = F)
#change it back to numerics
t = df_as_num(iris_chr)
#check that back to factor works
t2 = df_as_num(iris_chr, stringsAsFactors = T)
#check that skip factors works
t3 = df_as_num(iris, always_skip_factors = F)
#check that it handles NAs
iris_chr_NA = iris_chr;iris_chr_NA[1, 1] = NA
t4 = df_as_num(iris_chr_NA)

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
p_load(psych) #data in here

t = score_items(iqitems, c(4,4,4, 6,  6,3,4,4,   5,2,2,4,   3,2,6,7))

stopifnot({
  dim(t) == c(1525, 16)
  class(t) == "data.frame"
  (cor(t, use = "p") > 0) %>% all #all cors positive
})



# GG_group_means -----------------------------------------------------------

iris_na = miss_add_random(iris)

#does it respect factor levels order?
iris_reorder = iris
iris_reorder$Species = factor(x = iris_reorder$Species, levels = levels(iris$Species) %>% rev())
levels(iris_reorder$Species)
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
           GG_group_means(df = iris2, var = "Petal.Length", groupvar = "Species", subgroupvar = "type", type = "points"))

stopifnot({
  sapply(l_t, function(x) "ggplot" %in% class(x))
  throws_error("GG_group_means(iris_na, 'Sepal.Length', 'Species', na.rm = F)")
  levels(l_t$order$data$group1) == rev(levels(iris$Species))
})




# percent_cutoff ----------------------------------------------------------
#test cutoff function

stopifnot({
  #test various settings
  percent_cutoff(iris$Sepal.Length, cutoffs = c(4, 8)) == c(1, 0)
  percent_cutoff(iris$Sepal.Length, cutoffs = 5, digits = 2, inclusive = F) == .79
  percent_cutoff(iris$Sepal.Length, cutoffs = 5, digits = 2) == .85
  percent_cutoff(iris$Sepal.Length, cutoffs = 5, digits = 2, below = T) == .21
  percent_cutoff(iris$Sepal.Length, cutoffs = 5, digits = 2, below = T, inclusive = F) == .15

  #test if NA cause problems
  count_NA(percent_cutoff(c(1:3, NA, NaN, 4:6), cutoffs = 3)) == 0
})


# score_accuracy -------------------------------------------------
p_load(stringr)
#simulate some data
n_cases = 10
n_countries = 100

#random guesses
set.seed(2)
{
d_randomguesses = matrix(runif(n_cases * n_countries, 0, 100), ncol = n_countries) %>% as.data.frame()
v_randomguesses = runif(n_countries, 0, 100)
rownames(d_randomguesses) = sample(rownames(d_randomguesses)) #change order of rownames
}

#random true values
set.seed(1)
v_criteria = runif(n_countries, 0, 100)

#with missing values
d_randomguesses_na = miss_add_random(d_randomguesses)

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
  #check rownames
  rownames(l_t$t) == rownames(d_randomguesses)
})


# is_ --------------------------------------------------------
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

# is_
stopifnot({
  is_(iris, class = "data.frame") #check for one class
  is_(iris, class = c("data.frame", "logical", "matrix")) #can check for multiple classes
  is_(iris, class = "data.frame", size = c(150, 5)) #check for one class and size
  is_(iris, size = c(150, 5)) #check for size
  !is_(iris, size = 1) #check for wrong size
  is_(iris, type = "list") #check for type
  !is_(iris, type = "factor") #check for wrong type
  throws_error('is_(iris, class = "list", error_on_false = T)')  #check for one class, error
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


# missing data counting ---------------------------------------------------

stopifnot({
  count_NA(c(1:10, rep(NA, 5), 1:10)) == 5
  count_NA(c(1:10, rep(NA, 5), 1:10), reverse = T) == 20
})


# str_replace_multi, str_clean, str_detect2 --------------------------------------------
stopifnot({
  str_clean("Long_Test.String.") == "Long Test String"
  str_replace_multi("Long_Test.String", c("_", "\\."), " ") == "Long Test String"
  str_clean("A_B") == "A B" #underscores to spaces
  str_clean("A..B") == "A.B" #multiple dots in a row

  str_detect2(letters[1:10], pattern = "[acbde]") == c(rep(T, 5), rep(F, 5))
  str_detect2(letters[1:10], pattern = "[acbde]", value = T) == letters[1:5]
})



# silence ---------------------------------------------------------------
#make stuff shut up

#unload maps if its loaded
p_unload("maps")

stopifnot({
  #minimal silent stuff
  are_equal(capture.output(silence(warning("test"), messages = T, startupmessages = T)), character())
  are_equal(capture.output(silence(message("test"), warnings = T, startupmessages = T)), character())
  are_equal(capture.output(silence(p_load(maps), warnings = T, messages = T)), character())

})




# get_dims total_cells --------------------------------------------------------------
#a better version of dim() from base-r

stopifnot({
  get_dims(1:2) == 2
  get_dims(list(1, 2)) == 2
  get_dims(matrix(1:4, nrow=2)) == c(2, 2)
  matrix(1:4, nrow=2) %>% as.data.frame %>% get_dims == c(2, 2)
  array(1:16, dim = c(2, 2, 2)) %>% dim == c(2, 2, 2)

  #total cells, easy application
  total_cells(iris) == 750 #2d
  total_cells(1:3) == 3 #1d
  total_cells(array(1:27, dim = c(3, 3, 3))) == 27 #3d
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

p_load("psych")
p_load("magrittr")
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

# stack_into_n_columns split_into_n_columns -----------------------------------------
#useful function for stacking data that is too wide for the document or screen.

df = split_every_k(1:12, 2) %>% as.data.frame
df2 = data.frame(small = letters[1:6], big = LETTERS[1:6], stringsAsFactors = F)

stopifnot({
  stack_into_n_columns(df, 2) %>% dim == c(11, 2)
  stack_into_n_columns(df, 3) %>% dim == c(7, 3)
  silence(stack_into_n_columns(df, 4) %>% dim == c(7, 4))
})

#reverse function

stopifnot({
  #check wrong input
  throws_error("split_into_n_columns(data = df2, columns = 5, pad_rows = F)")

  #test some values
  split_into_n_columns(df2, split_times = 4) %>% get_dims() == c(3, 12)
  split_into_n_columns(df2, split_times = 3) %>% get_dims() == c(3, 9)
  split_into_n_columns(df2, split_times = 2) %>% get_dims() == c(4, 6)

  #without rownames
  split_into_n_columns(df2, split_times = 2, include_rownames = F) %>% get_dims() == c(4, 4)

  #without colnames
  split_into_n_columns(df2, split_times = 2, include_colnames = F) %>% get_dims() == c(3, 6)

  #without doing anything
  are_equal(split_into_n_columns(df2, split_times = 1, include_rownames = F, include_colnames = F) %>% as.data.frame(), df2, check.names = F)

  #test the colname
  split_into_n_columns(df2, split_times = 2, rownames_var =  "testname")[1, 1] %>% unlist() == "testname"
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

p_load("magrittr")

set.seed(1)
df = data.frame(a = rnorm(5), b = rnorm(5), c = rnorm(5))
weightsvar = runif(5)

stopifnot({
  #test basic function
  df_residualize(df, resid.vars = "c", print.models = F) %>% extract(c("a", "b")) != df[c("a", "b")]
  #test suffix + message off
  df_residualize(df, resid.vars = "c", suffix = "_r", print.models = F) %>% colnames() != colnames(df)
  #test exclusion vector
  df_residualize(df, resid.vars = "c", exclude_vars = "b", print.models = F) %>% extract("b") == df$b
  #test return
  df_residualize(df, resid.vars = "c", return.resid.vars = F, print.models = F) %>% colnames != "c"
  #with weights
  df_residualize(df, resid.vars = "c", weights = weightsvar, print.models = F) %>% get_dims() == c(5, 3) #with weights
})


# filter_by_missing_values ------------------------------------------------
#filters data by number of missing values per case

df = data.frame(1:10, letters[1:10])
set.seed(1)
df = miss_add_random(df)

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
p_load(magrittr)

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
p_load(weights);p_load(magrittr)

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


# SMD_matrix & pool_sd --------------------------------------------------------------
#standardized mean differences
p_load(magrittr)

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
         SMD_matrix(iris$Sepal.Length, iris$Species, trim = .05),

         #with missing data
         SMD_matrix(iris_miss$Sepal.Length, iris_miss$Species)
         )

stopifnot({
  sapply(t, is.matrix) #all matrices
  unique(t) %>% length %>% equals(7) #all different
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
x2 = list(1, NA, 2, NULL, 3, NaN, 4, Inf, list(1)) #a list with a list
x3 = list(NULL, NULL) #list that becomes empty when values are excluded
#this led to difficult to solve bugs before
x4 = list() #empty

stopifnot({
  #list of scalars
  are_equal(exclude_missing(x), list(1, 2, 3, 4))
  are_equal(exclude_missing(x, .NA = F), list(1, NA, 2, 3, 4))
  are_equal(exclude_missing(x, .NULL = F), list(1, 2, NULL, 3, 4))
  are_equal(exclude_missing(x, .NaN = F), list(1, 2, 3, NaN, 4))
  are_equal(exclude_missing(x, .Inf = F), list(1, 2, 3, 4, Inf))
  are_equal(exclude_missing(x3), list())
  are_equal(exclude_missing(x4), list())

  #complex list
  are_equal(exclude_missing(x2), list(1, 2, 3, 4, list(1)))
})


# split unsplit functions -----------------------------------------------------------------
library(magrittr)

#complex example: same as above but delete some columns
iris_set = iris[iris$Species == "setosa", -c(1, 5)] #create 3 lists with partly missing columns
iris_ver = iris[iris$Species == "versicolor", -c(2, 5)]
iris_vir = iris[iris$Species == "virginica", -c(3, 5)]
iris_list = list("setosa" = iris_set, "versicolor" = iris_ver, "virginica" = iris_vir) #a combined list
#then merge to one data.frame


#df_to_v
stopifnot({
  #length
  df_to_v(iris) %>% length() == 750

  #type convert
  df_to_v(iris) %>% class() == "character"

  #dont convert factors
  df_to_v(iris, fact_to_chr = F) %>% class() == "numeric"

  #with matrix input
  df_to_v(as.matrix(iris)) %>% length() == 750
})

#df_to_ldf
stopifnot({
  df_to_ldf(iris, iris$Species, remove_by = T) %>% length() == 3
  df_to_ldf(iris, "Species") %>% length() == 3
})

#ldf_to_df
stopifnot({
  #remove by, add back
  df_to_ldf(iris, "Species", remove_by = T) %>% ldf_to_df() %>% equals(iris)

  #dont remove by, dont add
  df_to_ldf(iris, "Species", remove_by = F) %>% ldf_to_df(add_by = F) %>% equals(iris)

  #reorder cars, split by cyl, keep by
  df_to_ldf(mtcars[order(mtcars$cyl), ], "cyl", remove_by = F) %>%
    #then combine back, set rownames to a var, and dont add another by
    ldf_to_df(rownames_to_var = T, rownames_name = "Car_name", add_by = F) %>%
    #then extract car names
    extract("Car_name") %>%
    #then compare to the orig
    equals(rownames(mtcars[order(mtcars$cyl), ]))

  #complex example
  is.data.frame(ldf_to_df(iris_list))
})



# add_id ------------------------------------------------------------------
x = iris; x["ID"] = "A"
stopifnot({
  add_id(iris, "A") == x
})



# list-arrays -------------------------------------------------------------
#easy way to make list-arrays

t = list(#vector input
         make_list_array(1:2, letters[1:2], LETTERS[1:2]),
         #mixed input
         make_list_array(1:2, letters[1:2], 2),
         #using only scalars as input
         make_list_array(1, 1, 1),
         #what about 10 dimensions with length 2?
         do.call("make_list_array", args = as.list(rep(2, 10))))

stopifnot({
  #check types
  sapply(t, is.list)
  #check all dims
  are_equal(sapply(t, dim), list(c(2, 2, 2), c(2, 2, 2), c(1, 1, 1), rep(2, 10)))
})



# homogeneity -------------------------------------------------------------

stopifnot({
  #test normal and reverse
  homogeneity(iris$Species) == 1/3
  are_equal(homogeneity(iris$Species, reverse = T), 2/3)

  #use summary statistics
  homogeneity(c(.7, .2, .1), summary = T) + homogeneity(c(.7, .2, .1), summary = T, reverse = T) == 1
  homogeneity(c(80, 15, 5), summary = T) + homogeneity(c(80, 15, 5), summary = T, reverse = T) == 1

  #test error
  throws_error("homogeneity(c(80, 15, 99), summary = T)")
})



# df_rename_vars -----------------------------------------------------

stopifnot({
  (df_rename_vars(iris, current_names = "Sepal.Length", new_names = "Sepal_length") %>% colnames()) == c("Sepal_length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  (df_rename_vars(iris, current_names = colnames(iris) %>% rev(), new_names = letters[1:5]) %>% colnames()) == letters[1:5]
})


# df_remove_vars ----------------------------------------------------------

stopifnot({
  df_remove_vars(iris, "Species") == iris[-5]
})


# check_ -----------------------------------------------------------
#check if arguments are missing

test_func = function(y) {
  check_missing("y")
  T
}

stopifnot({
  #check_missing
  throws_error("test_func(y = )")
  test_func(y = "k")

  #check_if_in
  are_equal(check_if_in("a", letters[1:10]), NULL)
  throws_error("check_if_in('a', letters[2])")
})



# wtd_sd wtd_mean wtd_sum standardize ------------------------------------------------------

set.seed(1)
X = rnorm(100, mean = 10, sd = 5)
set.seed(1)
W = runif(100)

stopifnot({
  #test normal
  are_equal(mean(standardize(X)), 0)
  are_equal(sd(standardize(X)), 1)

  #test weights
  are_equal(weighted.mean(standardize(X, W), W), 0)
  are_equal(wtd_sd(standardize(X, W), W), 1)

  #robust
  are_equal(median(standardize(X, robust = T)), 0)
  are_equal(mad(standardize(X, robust = T)), 1)

  #means
  are_equal(wtd_mean(X), mean(X))
  are_equal(wtd_mean(X, W), weighted.mean(X, W))

  #sums
  are_equal(wtd_sum(X), sum(X))
  are_equal(wtd_sum(X, W), weighted.mean(X, W) * 100)

  #errors
  throws_error("wtd_mean(NA, NA)")
  !throws_error("wtd_mean(c(1, NA), c(1, NA))")
})




# missing data functions --------------------------------------------------

t1 = miss_analyze(miss_add_random(iris))
set.seed(1)
t2 = rnorm(10e3) %>% matrix(nrow = 1000) %>% as.data.frame() %>% miss_add_random() %>% miss_analyze()

stopifnot({
  #analyze missing data relationships
  get_dims(t1) == c(5, 5)
  get_dims(t2) == c(10, 10)
  (t2 < .8) %>% sum(na.rm = T) == 90 #all are small effects

})



# transpose ---------------------------------------------------------------

stopifnot({
  are_equal(t_df(iris),
            iris %>% t %>% as.data.frame %>% set_colnames(rownames(iris)) %>% set_rownames(colnames(iris)))
})



# last_value --------------------------------------------------------------

stopifnot({
  last_value(1:3) == 3
last_value(c(1:3, NA)) == 3
is.na(last_value(c(1:3, NA), na.rm=F))
is.na(last_value(rep(NA, 3)))
})


# all_the_same lengths_match ------------------------------------------------------------

stopifnot({
  #check if same
  all_the_same(rep(1, 100))
  !all_the_same(1:100)

  #lengths
  lengths_match(1:4, 5:8) #same lengths
  lengths_match(iris, iris[1:2]) #same nrow
  !lengths_match(iris, iris[1:2], dimension = 2) #different ncol

  #errors
  throws_error("lengths_match(1:4, iris, dimension = 2)", silent_try = T)
})



# find_cutoff -------------------------------------------------------------
# using a simple model where there is a normal distribution of a trait, and everybody is selected for a special group
# above a cutoff. If we know what the population mean is above the cutoff, then we can estimate what the cutoff is.

stopifnot({
  are_equal(find_cutoff(115), 104.1)
  are_equal(find_cutoff(130), 123.7)
})


# df_colFunc --------------------------------------------------------------
#apply functions selectively to columns of a data.frame
t1 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Length"))
#using inverse regex
t2 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Species", pattern_inverse = T))
#using integer indices
t3 = head(df_colFunc(iris, func = function(x) return(x * 2), indices = 2:3))
#using logical indices
t4 = head(df_colFunc(iris, func = function(x) return(x * 2), indices = c(T, F, T, F, F)))
#removing unselected columns
t5 = head(df_colFunc(iris, func = function(x) return(x * 2), pattern = "Length", keep_unselected = F))
#select all by not providing any selector
t6 = head(df_colFunc(iris, func = as.character)) #all have been changed to chr

stopifnot({
  t1[1, 1] == iris[1, 1] * 2
  !t1[1, 2] == iris[1, 1] * 2

  t2[1, 1] == iris[1, 1] * 2
  t2[1, 2] == iris[1, 2] * 2

  !t3[1, 1] == iris[1, 1] * 2
  t3[1, 2] == iris[1, 2] * 2

  t4[1, 1] == iris[1, 1] * 2
  !t4[1, 2] == iris[1, 2] * 2

  ncol(t5) == 2
  t5[1, 1] == iris[1, 1] * 2

  sapply(t6, is.character)
})


# proportion_true ---------------------------------------------------------

#sample some logical data
set.seed(1)
{
  x = sample(c(T, F), size = 100, replace = T)
  x2 = sample(c(T, F, NA), size = 100, replace = T)
  x3 = sample(c(1, 0), size = 100, replace = T)
  x4 = sample(c(1L, 0L), size = 100, replace = T)
}


stopifnot({
  proportion_true(x) == .52 #standard
  proportion_true(x2) %>% round(2) %>% equals(.42) #no errors or NA output
  proportion_true(x3) == .62 #numeric/integer
  proportion_true(x4) == .57
  #throws errors if it gets a nonsensical input
  throws_error('"str" %>% proportion_true()')
  throws_error("data.frame() %>% proportion_true()")
  throws_error("list() %>% proportion_true()")
})


# df_gather_by_pattern ----------------------------------------------------
#some example data
d_test = readRDS("kirkegaard/S_misc_data.RDS")

#runs
d_test_1 = df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", id_col = "region")
d_test_2 = df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", id_col = "region", method = "custom")

#check that the two methods give the same result, which is a decent control
silence(stopifnot({
  are_equal(df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year"),
            df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", method = "custom"))
  "region" %in% names(d_test_1)
  "region" %in% names(d_test_2)
}))



# done --------------------------------------------------------------------

message("DONE! If you see this, there were no errors. Hopefully!")
