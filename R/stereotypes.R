#stereotype analysis functions


#' Score accuracy of estimates.
#'
#' Calculates accuracy measures from a data.frame of estimates using a vector of criteria values.
#' @param x (numeric data.frame) A data.frame with estimates. Rows must be cases. Alternatively, a vector of values. If given a vector, it will assume the user wants aggregate-level estimates.
#' @param criterion (numeric vector) A vector of criteria values to score estimates against.
#' @param methods (character vector) Which measures to return. Defaults to c("pearson_r", "mean_abs_error", "sd_error_abs", "mean_elevation_error_abs"). Use "all" to get all.
#' @param aggregate (boolean) Whether to use aggregated estimates.
#' @param aggregate_function (function) Which function to use for aggregation.
#' @param ... (named parameters) Additional parameters to pass to the aggregator function.
#' @export
score_accuracy = function(x, criterion, methods = c("pearson_r", "mean_abs_error", "sd_error_abs", "mean_error_abs"), aggregate = F, aggregate_function = wtd_mean, ...) {
  #other params given
  other_params = list(...)
  #is old param there? throw useful error
  if ("criteria" %in% names(other_params)) stop("`criteria` renamed to `criterion`, please adjust function call.")

  #save rownames
  v_rownames = rownames(x)

  #check
  x
  criterion
  if (!is.function(aggregate_function)) stop("`aggregate_function` isn't a function!")


  #aggregate?
  if (aggregate) { #if the user wants aggregated results

    #convert the x to aggregate estimates
    x = apply(x, 2, aggregate_function, ...) %>% t %>% data.frame
  }

  #detect aggregate
  if (is.vector(x)) { #if x is a vector, assume it is aggregate
    x = x %>% t %>% data.frame #convert to df form
    aggregate = T
  }

  #NAs
  if (any(is.na(x))) message("Note: some data were missing. This function uses pairwise complete cases.")

  #make df for results
  df = as.data.frame(x)
  criterion = unlist(criterion) %>% as.vector
  d_res = data.frame(matrix(nrow = nrow(df), ncol = 0))


  #Pearson r
  d_res$pearson_r = sapply(1:nrow(df), function(x) {
    cor(criterion, df[x, ] %>% unlist, use = "p")
  })


  #rank-order r
  d_res$rank_r = sapply(1:nrow(df), function(x) {
    cor(criterion, df[x, ] %>% unlist, method = "spearman", use = "p")
  })


  #delta/discrepancy/error
  d_deltas = (t(df) - criterion) %>% t %>% as.data.frame
  d_res$mean_abs_error = apply(d_deltas, 1, function(x) {
    mean(abs(x), na.rm = T)
  })


  #dispersion error
  d_res$sd = apply(df, 1, sd, na.rm = T) #sd of each persons estimates
  d_res$sd_error = d_res$sd - sd(criterion, na.rm = T)
  d_res$sd_error_abs = d_res$sd_error %>% abs


  #elevation error
  d_res$mean = apply(df, 1, mean, na.rm = T) #mean of each persons estimates
  d_res$mean_error = d_res$mean - mean(criterion, na.rm = T)
  d_res$mean_error_abs = abs(d_res$mean_error)

  #rownames
  if (!aggregate) { #dont set if we are using aggregate data
    rownames(d_res) = v_rownames
  }

  #subset and return
  if ("all" %in% methods) return(d_res)

  #check methods
  if (any(!methods %in% colnames(d_res))) stop(str_c("Some methods were not recognized!: "), setdiff(methods, colnames(d_res)))


  return(d_res[methods])
}



#' Score stereotype bias metrics
#'
#' @param estimates Data frame of estimates
#' @param criterion Vector of criterion values
#' @param bias_var Vector of values to bias metrics for
#'
#' @return A data frame of scored bias metrics
#' @export
#'
#' @examples
score_bias_metrics = function(estimates, criterion, bias_var) {
  #assert appropriate sizes
  assertthat::assert_that(length(criterion) == length(bias_var))
  assertthat::assert_that(ncol(estimates) == length(criterion))

  #loop rows
  plyr::adply(estimates, .margins = 1, function(r) {
    # browser()

    #temporary dataset
    dd = tibble(
      #extract estimates
      ests = r %>% unlist() %>% as.vector(),

      #standardize within person
      #this prevents scale effects
      ests_z = ests %>% standardize(),

      #get outcomes
      true_values = criterion,
      true_values_z = true_values %>% standardize(),

      #delta
      est_delta = ests - true_values,

      #bias vector values
      bias_var = bias_var
    )

    #regress to get residuals
    #fails when there is no variation in estimates
    #in which case we score as NA
    if (var(dd$ests) > 0) {
      #fit
      fit = lm(true_values_z ~ ests_z, data = dd)

      #add more vars
      dd %<>% mutate(
        #save resids, standardized
        resids_z = fit %>% resid() %>% standardize()
      )
    } else {
      dd %<>% mutate(
        #fill NAs
        resids_z = NA
      )
    }

    #return
    data.frame(
      #ratio scale metric
      bias_r = wtd.cors(dd$est_delta, dd$bias_var),
      bias_r_abs = wtd.cors(abs(dd$est_delta), dd$bias_var),

      #wmeans method
      bias_wmean = wtd_mean(dd$est_delta, dd$bias_var) - wtd_mean(dd$est_delta, 1-dd$bias_var),
      bias_wmean_abs = wtd_mean(abs(dd$est_delta), dd$bias_var) - wtd_mean(abs(dd$est_delta), 1-dd$bias_var),

      #resid z x bias vector
      resid_r = wtd.cors(dd$resids_z, dd$bias_var) %>% as.vector() %>% `*`(-1),

      #note that abs must be taken from the resids
      resid_r_abs = wtd.cors(abs(dd$resids_z), dd$bias_var) %>% as.vector() %>% `*`(-1)
    )

  }, .expand = F, .id = NULL)
}
