# some libs ---------------------------------------------------------------
library(pacman)
p_load(kirkegaard, psych, plyr, MASS, assertthat)

#otherwise get error
options("expressions" = 10000)


# write_clipboard ---------------------------------------------------------
#skip these tests on linux
#only run manually because these are annoying
if (F) {
  if (Sys.info()['sysname'] %in% c("Windows")) {
    write_clipboard(iris, 0, .rownames = F)
    Sys.sleep(.5)

    stopifnot({
      read.delim("clipboard")[38, 1] == 5
      read.delim("clipboard")[66, 5] == "versicolor"
    })

    #test arguments
    write_clipboard(iris[1:5, ], digits = 5)
    write_clipboard(iris[1:5, ], clean_names = T)
    write_clipboard(iris[1:5, ], clean_names = T, clean_what = "Q")
    write_clipboard(iris[1:5, ])

    #write NAs
    write_clipboard(miss_add_random(iris))
    Sys.sleep(.5)

    stopifnot({
      read.delim("clipboard") %>% miss_count() != 0 #make sure there are NAs in the output too
    })
  }
}




# MOD_k_fold_r2 --------------------------------------------------------------

fit = lm("Petal.Length ~ Species", data = iris)

stopifnot({
  round(MOD_k_fold_r2(fit, progress = F), 5) - round(c(0.9413717, 0.9380666), 5) == 0
})



# MOD_APSLM  ----------------------------------------------------------
t = silence(MOD_APSLM("Petal.Width", colnames(iris)[1:3], data = iris, standardized = T, messages = F, cv_runs = 2))
stopifnot({
  length(t) == 2
  class(t[[2]]) == "lm"
  class(t[[1]]) == "data.frame"
  nrow(t[[1]]) == 7
})

#with missing data
t = silence(MOD_APSLM("Petal.Width", colnames(iris)[1:3], data = miss_add_random(iris), standardized = T, messages = F, cv_runs = 2))
stopifnot({
  length(t) == 2
  class(t[[2]]) == "lm"
  class(t[[1]]) == "data.frame"
  nrow(t[[1]]) == 7
})


# semi_par_serial ---------------------------------------------------------
t = silence(semi_par_serial(df = airquality, dependent = "Ozone", primary = "Solar.R", secondaries = colnames(airquality)[3:6]))
t = round(t, 2)
stopifnot({
  t[2, 1] == .70
  dim(t) == c(4, 2)
})


# MOD_LASSO ----------------------------------------------------
#using the iris dataset
t = silence(MOD_LASSO(iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 2, messages = F))
stopifnot({
  dim(t) == c(5, 4)
  class(t) == "data.frame"
})

#weights
t_w = silence(MOD_LASSO(iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 5, weights_ = runif(nrow(iris)), messages = F))


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



# GG_contingency_Table ----------------------------------------------------
t = list(GG_contingency_table(mpg, "drv", "cyl"),
         GG_contingency_table(mpg, "drv", "cyl", margin = 1),
         GG_contingency_table(mpg, "drv", "cyl", margin = 2))

stopifnot({
  sapply(t, is.ggplot)
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


# add_SAC & Morans_I & Morans_I_multi & SAC_knsnr & SAC_measures ----------------------------
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
knsn_3_0 = SAC_knsnr(t0, "outcome", output = "cor")
knsn_3_1 = SAC_knsnr(t1, "outcome", output = "cor")

stopifnot({
  knsn_3_0 < knsn_3_1
})

#test scores
knsn_3_0 = SAC_knsnr(t0, "outcome", output = "scores")
knsn_3_1 = SAC_knsnr(t1, "outcome", output = "scores")

stopifnot({
  nrow(knsn_3_0) == nrow(knsn_3_0)
  nrow(knsn_3_0) == nrow(t0)
  class(knsn_3_0) == "numeric"
})

#test resids
knsn_3_0 = SAC_knsnr(t0, "outcome", output = "resids")
knsn_3_1 = SAC_knsnr(t1, "outcome", output = "resids")

stopifnot({
  class(knsn_3_0) == "numeric"
  class(knsn_3_1) == "numeric"
  length(knsn_3_0) == nrow(t0)
  length(knsn_3_1) == nrow(t0)
})

#test resids_cor
knsn_3_0 = SAC_knsnr(t0, "outcome", predictor = "test", output = "resids_cor")
knsn_3_1 = SAC_knsnr(t1, "outcome", predictor = "test", output = "resids_cor")

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
t = SAC_knsnr(t1, "outcome", dists=dists_y, output = "scores")
t_cor = SAC_knsnr(t1, "outcome", dists=dists_y, output = "cor")
t_resids = SAC_knsnr(t1, "outcome", dists=dists_y, output = "resids")

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


# remove_redundant_vars ----------------------------------------------

t = remove_redundant_vars(longley, .9, messages = F)
e = throws_error(remove_redundant_vars(longley, .9, messages = 123))
e2 = throws_error(remove_redundant_vars(longley, threshold = 5))

stopifnot({
  class(t) == "data.frame"
  dim(t) == c(16, 3)
  e
  e2
})



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
  MAT_vector2full(1:3) %>% MAT_half() %>% MAT_vector2full() %>% are_equal(MAT_vector2full(1:3))
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


# score_items -------------------------------------------------------------
#scores test items

t = score_items(iqitems, c(4,4,4, 6,  6,3,4,4,   5,2,2,4,   3,2,6,7))

stopifnot({
  dim(t) == c(1525, 16)
  class(t) == "data.frame"
  (cor(t, use = "p") > 0) %>% all #all cors positive
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
  miss_count(percent_cutoff(c(1:3, NA, NaN, 4:6), cutoffs = 3)) == 0
})


# score_accuracy -------------------------------------------------
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


# conditional_change ------------------------------------------------------
#a complement to plyr's mapvalues.

stopifnot({
  #tests
  conditional_change(1:10, func_str = "<5", new_value = NA) %>% miss_count == 4
  conditional_change(1:10, func_str = "> 9", new_value = NA) %>% miss_count == 1
  conditional_change(data.frame(1:10), func_str = "> 9", new_value = NA) %>% miss_count == 1
  conditional_change(list(1:10, 1:10), func_str = "> 9", new_value = NA) %>% sapply(is.na) %>% sum == 2
  conditional_change(matrix(1:9, nrow = 3), func_str = "> 5", new_value = NA) %>% miss_count == 4
  conditional_change(c(1:10, NA), func_str = "<5", new_value = NA) %>% miss_count == 5

  #errors
  throws_error(conditional_change(1:10, func_str = "!1!", new_value = NA))
  throws_error(conditional_change(is.array, func_str = "<0", new_value = NA))
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
  throws_error(math_to_function('!4!4!4')(1))
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
  throws_error(copy_names(t, t2, F))
})



# fill_in -----------------------------------------------------------------
#fills in values so that a vector reaches a desired length.

stopifnot({
  fill_in(1:5, 10) %>% miss_count == 5
  fill_in(1:5, 5) %>% miss_count == 0
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


# split_every_k -----------------------------------------------------------
#split a vector every k element

stopifnot({
  all.equal(split_every_k(1:12, 4), list(1:4, 5:8, 9:12), check.names = F)
  all.equal(split_every_k(1:11, 4), list(1:4, 5:8, 9:11), check.names = F)
  all.equal(split_every_k(1:4, 4), list(1:4), check.names = F)
  throws_error(split_every_k(1:11, 4, uneven = F))
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
  throws_error(split_into_n_columns(data = df2, columns = 5, pad_rows = F))

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
  throws_error(alternate(list(1:2, letters[1:3])))
  throws_error(alternate(c(1:2, 2:1)))
})


# extract_num_vars --------------------------------------------------------

stopifnot({
  extract_num_vars(iris) == iris[-5]
  extract_nonnum_vars(iris) == iris[5]
})


# format_digits -----------------------------------------------------------
#useful for outputting data

stopifnot({
  format_digits(c(.1), 2) == "0.10"
  format_digits(c(.1), 5) == "0.10000"
  format_digits(c(.12345), 2) == "0.12"
  format_digits(c(.15555), 2) == "0.16"
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


# merge_vectors -----------------------------------------------------------

t1 = c(NA, 2, 3, NA)
t2 = c(1, NA, NA, 4)
t1_n = c(NA, 2, 3, NA); names(t1_n) = letters[1:4]
t2_n = c(1, NA, NA, 4); names(t2_n) = letters[1:4]
t3_n = c(1, NA, NA, 4, 5); names(t3_n) = letters[1:5]


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
  df_to_ldf(iris, "Species", remove_by = T) %>% ldf_to_df() %>% `==`(iris)

  #dont remove by, dont add
  df_to_ldf(iris, "Species", remove_by = F) %>% ldf_to_df(add_by = F) %>% `==`(iris)

  #reorder cars, split by cyl, keep by
  df_to_ldf(mtcars[order(mtcars$cyl), ], "cyl", remove_by = F) %>%
    #then combine back, set rownames to a var, and dont add another by
    ldf_to_df(rownames_to_var = T, rownames_name = "Car_name", add_by = F) %>%
    #then extract car names
    extract("Car_name") %>%
    #then compare to the orig
    `==`(rownames(mtcars[order(mtcars$cyl), ]))

  #complex example
  is.data.frame(ldf_to_df(iris_list))
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
         do.call("make_list_array", args = as.list(rep(2, 10)))
         )

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

  #robust
  are_equal(median(standardize(X, robust = T)), 0)
  are_equal(mad(standardize(X, robust = T)), 1)
})




# find_cutoff -------------------------------------------------------------
# using a simple model where there is a normal distribution of a trait, and everybody is selected for a special group
# above a cutoff. If we know what the population mean is above the cutoff, then we can estimate what the cutoff is.

stopifnot({
  are_equal(find_cutoff(115), 104.1)
  are_equal(find_cutoff(130), 123.7)
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
  proportion_true(x2) %>% round(2) %>% `==`(.42) #no errors or NA output
  proportion_true(x3) == .62 #numeric/integer
  proportion_true(x4) == .57
  #throws errors if it gets a nonsensical input
  throws_error('"str" %>% proportion_true()')
  throws_error("data.frame() %>% proportion_true()")
  throws_error("list() %>% proportion_true()")
})


# df_gather_by_pattern ----------------------------------------------------
#some example data
d_test = readRDS("S_misc_data.RDS")

#runs
silence({d_test_1 = df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", id_col = "region")})
d_test_2 = df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", id_col = "region", method = "custom")

#check that the two methods give the same result, which is a decent control
silence(stopifnot({
  are_equal(df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year"),
            df_gather_by_pattern(d_test, pattern = "\\.(\\d\\d\\d\\d)$", key_col = "year", method = "custom"))
  "region" %in% names(d_test_1)
  "region" %in% names(d_test_2)
}))


# rescale -----------------------------------------------------------------
#tests without checking, just to see if function errors

stopifnot({
  round(kirkegaard::rescale(1:10, new_min = 0, new_max = 1), 2) == round(seq(0, 1, length.out = 10), 2)
  round(kirkegaard::rescale(1:10, new_min = 0, new_max = 5), 2) == round(seq(0, 5, length.out = 10), 2)
  kirkegaard::rescale(c(.1, .5, 1), new_min = 10, new_max = 20, old_min = 0, old_max = 1) == c(11, 15, 20)

  #input errors
  throws_error("kirkegaard::rescale('x')") # class failure
  throws_error("kirkegaard::rescale(1, new_min = '')") #class failure

  #errors on impossible min  max values
  throws_error("kirkegaard::rescale(1, old_max = 0, new_min = 0, new_max = 1)")
  throws_error("kirkegaard::rescale(1, old_min = 2, new_min = 0, new_max = 1)")
})


# pu_translate ------------------------------------------------------------
#lots of things to test
v_argentinian_provinces = c("Buenos Aires", "Buenos Aires City (DC)", "Catamarca", "Chaco",
                            "Chubut", "Córdoba", "Corrientes", "Entre Ríos", "Formosa", "Jujuy",
                            "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro",
                            "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
                            "Tierra del Fuego", "Tucumán")
v_argentina_abbrev = structure(c("ARG_B", "ARG_C", "ARG_K", "ARG_H", "ARG_U", "ARG_X",
                                 "ARG_W", "ARG_E", "ARG_P", "ARG_Y", "ARG_L", "ARG_F", "ARG_M",
                                 "ARG_N", "ARG_Q", "ARG_R", "ARG_A", "ARG_J", "ARG_D", "ARG_Z",
                                 "ARG_S", "ARG_G", "ARG_G", "ARG_T"), .Names = c("Buenos Aires",
                                                                                 "Buenos Aires City (DC)", "Catamarca", "Chaco", "Chubut", "Córdoba",
                                                                                 "Corrientes", "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja",
                                                                                 "Mendoza", "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan",
                                                                                 "San Luis", "Santa Cruz", "Santa Fe", "Santiago del Estero",
                                                                                 "Tierra del Fuego", "Tucumán"))
#translate back names
v_abbrevs = c("DNK", "THA", "USA", "GBR", "SWE", "NOR", "DEU", "VEN", "TUR", "NLD")
v_abbrevs_reversed = c("Denmark", "Thailand", "USA", "United Kingdom", "Sweden", "Norway", "Germany", "Venezuela", "Turkey", "Netherlands")

v_odd_names = c("Danmark", "Viet Nam", "Belgia", "Bolivia, Plurinational State of")
v_nonodd_names = c("Denmark", "Vietnam", "Belgium", "Bolivia")

v_notquiteright = c("USa", "Dnmark", "Viitnam", "Bolgium", "Boliviam")
v_quiteright = c("USA", "Denmark", "Vietnam", "Belgium", "Bolivia")

stopifnot({
  #test argentina results
  pu_translate(v_argentinian_provinces, superunit = "ARG", messages = 0) == v_argentina_abbrev

  #reverse
  pu_translate(v_abbrevs, messages = 0, reverse = T) == v_abbrevs_reversed

  #standardize odd names by exact match
  pu_translate(v_odd_names, messages = 0, standardize_name = T) == v_nonodd_names

  #fuzzy match
  pu_translate(v_notquiteright, messages = 0) == c("USA", "DNK", "VNM", "BEL", "BOL")
  pu_translate(v_notquiteright, messages = 0, standardize_name = T) == v_quiteright
})


# df_flexsubset -----------------------------------------------------------

stopifnot({
  #subset a single variable, no drop
  df_flexsubset(iris, c("Species")) == iris["Species"]
  #subset two variables
  df_flexsubset(iris, c("Species", "Sepal.Length")) == iris[c("Species", "Sepal.Length")]
  #subset two variables, one doesn't exist
  df_flexsubset(iris, c("Species", "test"), messages = F) == iris["Species"]
  silence(are_equal(df_flexsubset(iris, c("test")), as.data.frame(matrix(nrow=150, ncol=0)), check.attributes = F))
})


# done --------------------------------------------------------------------

message("DONE! If you see this, there were no errors. Hopefully!")
