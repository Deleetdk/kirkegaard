
plot_loadings2 = function (fa, factor_labels = NA, reorder = "median")
{
  library("plotflow")
  library("stringr")
  library("ggplot2")
  library("reshape")
  library("gdata")
  library("plyr")

  #checks
  if (!"psych" %in% class(fa)) stop("fa is not an fa object from the psych package!")

  #get loadings
  d = get_loadings(fa)

  #set names
  if (is.na(factor_labels)) {
    factor_labels = colnames(fa$loadings)
  }

  #check length
  if (length(factor_labels) == ncol(d)) {
    colnames(d) = factor_labels
  } else {
    stop("Length of factor_labels does not match number of factors!")
  }

  #add id
  d$id = rownames(d) %>% as.factor

  #long form
  d2 = melt(d, id.vars = "id")

  #check if reorder is using too large an index
  if (is.numeric(reorder)) {
    if (reorder > ncol(d)) stop("reorder index larger than the number of factors!")
  }

  #reorder
  if (!is.na(reorder)) {
    if (reorder == "old") {
      suppressor({
        d2 = reorder_by(id, ~value, d2)
      })
    } else if (reorder == "mean") {
      d2_aggregate = ddply(d2, .(id), function(x) c(value = mean(x$value)))
      suppressor({
        d2 = reorder_by(id, ~aggregate, d2)
      })

      #reorder
      suppressor({
        d2_aggregate = reorder_by(id, ~value, d2_aggregate)
      })
      suppressor({
        d2$id = reorder.factor(d2$id, new.order = levels(d2_aggregate$id))
      })

    } else if (reorder == "median") {
      d2_aggregate = ddply(d2, .(id), function(x) c(value = median(x$value)))
      #reorder
      suppressor({
        d2_aggregate = reorder_by(id, ~value, d2_aggregate)
      })
      suppressor({
        d2$id = reorder.factor(d2$id, new.order = levels(d2_aggregate$id))
      })

    } else {
      #match
      if (is_whole_number(reorder)) { #match by index
        d2_sub = d2[d2$variable %>% as.numeric == reorder, ]
      } else if (is.character(reorder)) {
        d2_sub = d2[d2$variable == reorder, ]
      } else {
        stop("reorder was neither the default, a whole number or a string!")
      }

      #check
      if (nrow(d2_sub) == 0) {
        stop("Could not match the reorder parameter to a factor name!")
      }

      #reorder
      suppressor({
        d2_sub = reorder_by(id, ~value, d2_sub)
      })
      suppressor({
        # d2$id = reorder(d2$id, new.order = levels(d2_sub$id))
        d2$id = reorder.factor(d2$id, new.order = levels(d2_sub$id))
      })
    }
  }
  #plot
  #if only one factor, use simpler plot
  if (length(unique(d2$variable)) == 1) {
    g = ggplot(d2, aes(y = id, x = value)) +
      geom_point() +
      xlab("Loading") + ylab("Indicator")
  } else { #plot with fancy method
    g = ggplot(d2, aes(x = id, y = value, color = variable, group = variable)) +
      geom_point(position = position_dodge(width = 0.5)) +
      ylab("Loading") + xlab("Indicator") + scale_color_discrete(name = "Factor",
                                                                 labels = factor_labels) +
      coord_flip()
  }

  return(g)
}


# fa_iris1 = fa(iris[-5])
# fa_iris2 = fa(iris[-5], 2)
#
# plot_loadings2(fa_iris1)
# plot_loadings2(fa_iris2)

library(magrittr)
library(stringr)

v_max_k = 20
v_n = 1e3


make_categorical_data = function(k_max, func, n, ...) {
  lapply(2:k_max, function(k) {
    library(magrittr)

    #get poisson densities
    v_probs = do.call(func, list(1:k, ...))

    sample_letters_with_probs(v_probs, v_n)
  }) %>% as.data.frame %>% set_colnames(str_c("k", 2:k_max))
}


sample_letters_with_probs = function(probs, n) {
  #how many categories
  k = length(probs)

  #normalize
  probs / sum(probs)

  #generate data
  sample(letters[1:k], size = n, replace = T, prob = probs)
}


mean_abs_distance_equal = function(x, k = NA) {
  library(magrittr)

  #get prop table
  v_table = table(x) %>% as.vector() %>% divide_by(length(x))

  #how many groups are there?
  if (is.na(k)) v_k = unique(x) %>% length()
  #default to empirical value

  #get the abs distances to the equality scenario
  v_abs_dist_equal = v_table %>% subtract(1/v_k) %>% abs()

  #get mean value
  v_mean_abs_dist_equal = mean(v_abs_dist_equal)

  #return
  v_mean_abs_dist_equal
}

sd_probs = function(x) {
  #get prop table
  v_table = table(x) %>% as.vector() %>% divide_by(length(x))

  #sd
  sd(v_table)
}

categorical_dispersion_measures = function(x, k = NA) {
  c(MADE = mean_abs_distance_equal(x, k = k), SD_prob = sd_probs(x))
}

random_probs = function(k) {
  #get random values
  v_probs = runif(k)

  #normalize
  v_probs = v_probs / sum(v_probs)

  #return
  v_probs
}

# d_pois_1 = make_categorical_data(k_max = v_max_k, dpois, n = v_n, lambda = 1)
# d_norm = make_categorical_data(k_max = v_max_k, dnorm, n = v_n)
# d_random = make_categorical_data(k_max = v_max_k, random_probs, n = v_n)
#
# l_d = list(d_pois_1, d_norm, d_random)
#
# sapply(d_random, categorical_dispersion_measures) %>% t
#
# lapply(l_d, function(x) sapply(x, categorical_dispersion_measures) %>% t %>% cor)



# d = lapply(2:v_max_k, function(k) {
#   library(magrittr)
#
#   #get poisson densities
#   v_probs = dpois(1:k, lambda = 1)
#
#   sample_letters_with_probs(v_probs, v_n)
# }) %>% as.data.frame %>% set_colnames(str_c("k", 2:v_max_k))
#
# d2 = lapply(2:v_max_k, function(k) {
#   library(magrittr)
#
#   #get poisson densities
#   v_probs = dpois(1:k, lambda = 2)
#
#   sample_letters_with_probs(v_probs, v_n)
# }) %>% as.data.frame %>% set_colnames(str_c("k", 2:v_max_k))



# cor.test(iris[[1]], iris[[2]])
#
# wtd.cor(iris[1], iris[2])












# run_tests = function(..., test_name, error_msg) {
#   if (missing("test_name")) test_name = deparse(...)
#   start_msg = paste0("Starting test: ", test_name)
#   error_msg = "Test failed!"
#
#   #test
#   message(start_msg)
#   result = kirkegaard::throws_error("stopifnot(...)")
#   if (result) stop(error_msg)
# }

# run_tests(T)



