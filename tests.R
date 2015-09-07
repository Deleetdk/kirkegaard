# merge_datasets ----------------------------------------------------------
#some data to merge
d1 = iris[1:75,] #split in two
d2 = iris[76:150,]
t = merge_datasets(d1, d2) #merge into one
stopifnot(all(iris == t)) #they should be equal again


# FA_all_methods ----------------------------------------------------------
t = cor(FA_all_methods(iris[-5], skip_methods = "pa")$scores)
stopifnot(dim(t)==c(12, 12))


# std_df ------------------------------------------------------------------
#should print: "Skipped Species because it is a factor."
stopifnot({
  t = std_df(iris)
  round(t[5, 1], 4) == -1.0184
  t[10, 5] == "setosa"
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
t = MOD_repeat_cv_glmnet(df = iris, dependent = "Sepal.Length", predictors = c("Sepal.Width", "Petal.Length", "Petal.Width"), runs = 20)
stopifnot({
  dim(t) == c(20, 4)
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
t = FA_splitsample_repeat(ability, runs = 10)
stopifnot({
  class(t) == "data.frame"
  dim(t) == c(10, 1)
})
