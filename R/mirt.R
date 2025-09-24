#### All mirt package related functions
#### And the helper functions and other item level analysis functions

#get factor loadings from mirt fit
get_loadings = function(x, clean_names = F) {

  #mirt
  if (any(class(x) %in% c("SingleGroupClass", "MultiGroupClass", "MixedGroupClass"))) {
    loads = mirt::summary(x, verbose = F)$rotF
  } else if (inherits(x, "fa")) {
    loads = x$loadings
  }

  if (clean_names) {
    loads = loads |> unname()
  }

  loads
}

get_difficulties = function(x) {
  mirt::coef(x, simplify = T)$items[, 2]
}

get_discriminations = function(x) {
  mirt::coef(x, simplify = T)$items[, 1]
}


#' Extract item parameters from `mirt()` fit
#'
#' @param x A `mirt` fit object
#'
#' @return A data frame of item parameters
#' @export
#'
#' @examples
#' library(mirt)
#' set.seed(1)
#' item_stats = tibble(slope = seq(0.2, 2, length.out = 5), diff = seq(-2, 2, length.out = 5))
#' data = simdata(item_stats$slope, item_stats$diff, 1000, itemtype = "2PL")
#' fit = mirt(data, 1)
#' get_mirt_stats(fit)
get_mirt_stats = function(x) {
  tibble(
    item_i = seq_along_rows(mirt::coef(x, simplify = T)$items),
    item = mirt::coef(x, simplify = T)$items %>% rownames(),
    loading = get_loadings(x) %>% as.vector(),
    difficulty = get_difficulties(x),
    discrimination = get_discriminations(x),
    mean = x@Data$data %>% colMeans(na.rm = T)
  )
}

#' Get reliability distribution of a `mirt` model
#'
#' @param x A `mirt` fit object
#'
#' @return A data frame of reliability values
#' @export
#'
#' @examples
#' library(mirt)
#' data = simdata(seq(0.2, 2, length.out = 5), seq(-2, 2, length.out = 5), 1000, itemtype = "2PL")
#' fit = mirt(data, 1)
#' get_reliabilities(fit)
get_reliabilities = function(x) {
  rxx_info = mirt::plot(x, type = "rxx")

  #which range has >.90?
  tibble::tibble(
    z = rxx_info$panel.args[[1]]$x,
    rel = rxx_info$panel.args[[1]]$y
  )
}

#internal function to fit mirt
fit_mirt = function(items, mirt_args, reliability_at) {

  local_mirt_fit = rlang::exec(
    .fn = mirt::mirt,
    data = items,
    !!!mirt_args
  )

  #get scores
  local_scores = mirt::fscores(local_mirt_fit, full.scores.SE = T)

  #return fit and scores
  list(
    fit = local_mirt_fit,
    scores = local_scores,
    reliability = get_reliability_at(local_mirt_fit, reliability_at)
  )
}

#make a list of items indexes with one removed per set
make_leave_one_out_sets = function(item_idx) {
  #make a list of items indexes with one removed per set
  purrr::map(
    seq(item_idx),
    function(i) {
      item_idx[-i]
    }
  )
}


#make forwards item selection sets
make_forward_sets = function(
    all_items,
    current_items = NULL,
    full_fit,
    min_items = 3,
    start_method = "highest_loading",
    IRT = T
) {
  #make a list of items indexes with one removed per set
  #if we have none, it's easy
  if (is.null(current_items)) {
    #if we have no items, we can either try all 3-way combinations, which is often not possible (explosion)
    #or we can begin with a best guess of the 3 items with highest loadings from full analysis (fast)
    if (start_method == "highest_loading") {
      #get highest loading items

      if (IRT) {
        item_stats = get_mirt_stats(full_fit)
      } else {
        item_stats = full_fit$item_stats
      }

      #pick items
      start_items = item_stats %>% arrange(loading) %>% tail(min_items) %>% pull(item)
      sets = list(start_items)
      message(str_glue("Starting with the {min_items} items with highest loadings: {str_c(start_items, collapse = ', ')}"))
    } else if (start_method == "combination") {
      #if we have no items, we can either try all 3-way combinations, which is often not possible (explosion)
      #or we can begin with a best guess of the 3 items with highest loadings from full analysis (fast)
      sets = combn(colnames(all_items), m = min_items, simplify = F)
    }

  } else {
    #if we have some, we need to add one different item to each set
    sets = purrr::map(
      setdiff(colnames(all_items), current_items),
      function(i) {
        c(current_items, i)
      }
    )
  }

  sets
}

get_reliability_at = function(
    fit,
    reliability_at
) {
  #if total, use total reliability
  if (length(reliability_at) == 1 && reliability_at == "total") {
    scores = mirt::fscores(fit, full.scores.SE = T)
    return(mirt::empirical_rxx(scores) %>% unname())
  }

  #if not total, is it a vector of values?
  assert_that(is.numeric(reliability_at), msg = "`reliability_at` must be either 'total' a numeric vector of target z scores")

  #compute all reliabilities
  fit_rels = get_reliabilities(fit)

  #get rels closest to target values
  fit_fels_targets = purrr::map_dfr(
    reliability_at,
    function(target) {
      fit_rels %>%
        mutate(diff = abs(z - target)) %>%
        arrange(diff) %>%
        slice_head(n = 1)
    }
  )

  #return average
  fit_fels_targets$rel %>%
    set_names(reliability_at)
}

#compute model stats
compute_fit_stats = function(
    fit,
    item_set,
    selection_method,
    criterion_vars,
    full_reliability,
    criterion_cors_full,
    save_fits = T,
    reliability_at
    ) {
  #get scores and their full set cors
  cors = wtd.cors(
    bind_cols(
      score = fit$scores[, 1],
      criterion_vars
    )
  )

  #mean fraction of criterion cors
  item_set = list(item_set)
  criterion_cors = cors[-1, 1]
  criterion_cors_frac = criterion_cors / criterion_cors_full[-1, 1]
  mean_criterion_cors_frac = mean(criterion_cors_frac)
  reliability_frac = (fit$reliability / full_reliability) %>% unname() %>% mean()

  #determine what to maximize
  criterion_value = switch(selection_method,
                           "rc" = mean(c(criterion_cors_frac, reliability_frac)),
                           "r" = reliability_frac,
                           "c" = mean(criterion_cors_frac)
  )

  y = tibble(
    items_in_scale = length(item_set[[1]]),
    item_set = item_set,
    criterion_cors_frac = criterion_cors_frac,
    mean_criterion_cors_frac = mean_criterion_cors_frac,
    reliability = fit$reliability,
    reliability_frac = reliability_frac,
    criterion_value = criterion_value
  )

  #add criterion cors
  for (i in seq_along(criterion_cors)) {
    y[[str_glue("r_{rownames(cors)[i+1]}")]] = criterion_cors[i]
  }

  #save fits and scores?
  if (save_fits) {
    y$fit = list(fit)
    y$scores = list(fit$scores)
  }

  y
}

#genetic algo item lists function
genetic_algo_item_sets = function(
    all_items,
    item_target,
    current_population = NULL,
    population_size = 1000,
    mutation_rate = 0.1,
    prob_exact = F,
    include_parents = T
) {

  #if no current poulation, then we start with random items
  if (is.null(current_population)) {
    new_pop = map(
      1:population_size,
      function(i) {
        sample(colnames(all_items), size = item_target)
      }
    )
  } else {

    #make repeats to match population size
    new_pop = rep(current_population, length.out = population_size)

    #mutate
    for (i in seq_along(new_pop)) {
      #mutate by partial resampling
      new_pop[[i]] = kirkegaard:::partially_resample(
        new_pop[[i]],
        possible_values = colnames(all_items),
        prob = mutation_rate,
        prob_exact = prob_exact
      )
    }

    #add parents as well? this makes the population size larger by the selection proportion
    #it prevents regression, and avoids inability to stop at a local minimum
    if (include_parents) {
      new_pop = c(new_pop, current_population)
    }

  }

  new_pop
}

#genetic algorithm
abbreviate_by_genetic_algo = function(
    all_items,
    item_target,
    current_population = NULL,
    criterion_vars,
    criterion_cors_full,
    selection_method,
    full_fit,
    full_reliability,
    mirt_args = NULL,
    save_fits = T,
    max_generations,
    population_size,
    mutation_rate,
    selection_ratio,
    stop_search_after_generations,
    include_parents,
    reliability_at,
    IRT = T
) {

  #main loop
  all_fits = tibble()
  for (iter in 1:max_generations) {
    #msg current iter
    message(str_glue("Starting iteration {iter} out of a maximum of {max_generations}"))

    #get new population
    new_pop = genetic_algo_item_sets(
      all_items = all_items,
      item_target = item_target,
      current_population = current_population,
      population_size = population_size,
      mutation_rate = mutation_rate,
      include_parents = include_parents
    )

    #fit models
    current_fits = furrr::future_map_dfr(
      new_pop,
      function(item_set) {

        if (IRT) {
          #fit mirt
          fit = fit_mirt(all_items[, item_set, drop = F], mirt_args = mirt_args, reliability_at = reliability_at)
        } else {
          fit = make_CTT_fit(all_items[, item_set, drop = F])
        }

        #get stats
        y = compute_fit_stats(
          fit = fit,
          item_set = item_set,
          selection_method = selection_method,
          criterion_vars = criterion_vars,
          full_reliability = full_reliability,
          criterion_cors_full = criterion_cors_full,
          save_fits = save_fits,
          reliability_at = reliability_at
        )

        y
      }
    )

    #add generation number
    current_fits$generation = iter

    #select top
    current_fits = current_fits %>%
      arrange(-criterion_value)

    #mark as selected
    current_fits$selected = F
    current_fits$selected[1:ceiling(population_size * selection_ratio)] = T

    #add fits to collection
    all_fits = bind_rows(
      all_fits,
      current_fits %>% filter(selected)
    )

    #check if we have improvement
    if (iter > 1) {

      #get best from last iteration
      best_last = all_fits %>%
        filter(generation == (iter - 1)) %>%
        arrange(-criterion_value) %>%
        pull(criterion_value) %>%
        max()

      #get improvement
      improvement = (max(current_fits$criterion_value) - best_last)

      #msg about improvement this generation
      message(str_glue("Improvement in iteration {iter} out of {max_generations}: {str_round(improvement, 5)}, current best: {str_round(max(current_fits$criterion_value), 5)}"))

      #if no improvement for X generations, stop search
      if (iter > stop_search_after_generations) {
        last_k_criterion_values = all_fits %>%
          group_by(generation) %>%
          arrange(-criterion_value) %>%
          slice_head(n = 1) %>%
          ungroup() %>%
          pull(criterion_value) %>%
          tail(stop_search_after_generations)

        if (all_the_same(last_k_criterion_values)) {
          message(str_glue("No improvement for {stop_search_after_generations} generations, stopping search"))
          break
        }
      }
    }

    #get new population
    current_population = current_fits %>% filter(selected) %>% pull(item_set)
  } #end loop

  #return results
  all_fits
}

#backwards drop function (drop 1 item)
backwards_drop = function(
    all_items,
    current_selection = NULL,
    criterion_vars,
    criterion_cors_full,
    selection_method,
    full_fit,
    full_reliability,
    mirt_args = NULL,
    save_fits = T,
    reliability_at = reliability_at,
    IRT = T
) {

  #if no current selection, select all
  if (is.null(current_selection)) {
    current_selection = colnames(all_items)
  }

  #main loop
  reductions = furrr::future_map_dfr(
    make_leave_one_out_sets(current_selection),
    function(item_set) {

      if (IRT) {
        #fit mirt
        fit = fit_mirt(all_items[, item_set, drop = F], mirt_args, reliability_at = reliability_at)
      } else {
        fit = make_CTT_fit(all_items[, item_set, drop = F])
      }


      #get stats
      y = compute_fit_stats(
        fit = fit,
        item_set = item_set,
        selection_method = selection_method,
        criterion_vars = criterion_vars,
        full_reliability = full_reliability,
        criterion_cors_full = criterion_cors_full,
        save_fits = save_fits,
        reliability_at = reliability_at
      )

      y
    }
  )

  #return results
  reductions
}

#forwards pick function
forwards_pick = function(
    items,
    current_items,
    criterion_vars,
    criterion_cors_full,
    selection_method,
    full_fit,
    full_reliability,
    mirt_args = NULL,
    save_fits = T,
    reliability_at = reliability_at,
    IRT = T
) {

  #main loop
  additions = furrr::future_map_dfr(
    make_forward_sets(items, current_items, full_fit = full_fit, IRT = IRT),
    function(item_set) {

      if (IRT) {
        #fit mirt
        fit = fit_mirt(items[, item_set, drop = F], mirt_args, reliability_at = reliability_at)
      } else {
        fit = make_CTT_fit(items[, item_set, drop = F])
      }

      #get stats
      y = compute_fit_stats(
        fit = fit,
        item_set = item_set,
        selection_method = selection_method,
        criterion_vars = criterion_vars,
        full_reliability = full_reliability,
        criterion_cors_full = criterion_cors_full,
        save_fits = save_fits,
        reliability_at = reliability_at
      )

      y
    }
  )

  #return results
  additions
}

#make loadings based item list
make_item_list_for_loadings_method = function(x, max_items) {
  #return a list with vectors of first 3 items, then first 4, then first 5, until max
  item_sets = purrr::map(
    3:max_items,
    function(last_item) {
      x[1:last_item]
    }
  )

  item_sets
}

#a complex function to balance items into groups but assign remainder items to middle positions
#made with GPT4 after 10 tries
get_balanced_items <- function(data, num_groups, num_items) {
  if (num_items > nrow(data)) {
    stop("Number of items to select is greater than available items.")
  }

  # Prepare the data by assigning groups based on difficulty
  data <- data %>%
    mutate(group = ntile(difficulty, num_groups)) %>%
    arrange(group, desc(loading))

  # Initial distribution
  base_items_per_group <- num_items %/% num_groups
  remainder = num_items %% num_groups
  group_counts = rep(base_items_per_group, num_groups)

  # Calculate middle positions and distribute remainders
  # Ensuring that the number of items always matches exactly
  middle_index = (num_groups + 1) / 2
  if (num_groups %% 2 == 0) {  # If even number of groups
    group_counts[ceiling(middle_index)] = group_counts[ceiling(middle_index)] + remainder %/% 2
    group_counts[floor(middle_index)] = group_counts[floor(middle_index)] + (remainder + 1) %/% 2
  } else {  # If odd number of groups
    group_counts[floor(middle_index)] = group_counts[floor(middle_index)] + remainder
  }

  # Select items based on the calculated distribution
  selected_items = vector("list", num_groups)
  for (i in seq_along(group_counts)) {
    selected_items[[i]] = head(data[data$group == i,], group_counts[i])
  }

  # Combine all selected items into a single dataframe
  selected_items = do.call(rbind, selected_items)
  return(selected_items)
}


#balanced difficulty groups
make_item_list_for_balanced_loadings_method = function(item_stats, max_items, difficulty_balance_groups) {
  #return a list with vectors of first 3 items, then first 4, then first 5, until max
  item_sets = purrr::map(
    3:max_items,
    function(item_count) {
      get_balanced_items(item_stats, num_groups = difficulty_balance_groups, num_items = item_count)$item
    }
  )

  item_sets
}

#use highest loadings
max_loading_method = function(
    items,
    max_items,
    criterion_vars,
    criterion_cors_full,
    full_fit,
    full_reliability,
    mirt_args = NULL,
    save_fits = T,
    selection_method,
    difficulty_balance_groups,
    residualize_loadings,
    reliability_at,
    IRT = T
) {
  #not non-NULL to both residualization and balancing
  if (!is.null(difficulty_balance_groups) & residualize_loadings) {
    stop("Cannot residualize loadings and balance by difficulty groups at the same time.", call. = F)
  }

  #get loadings
  if (IRT) {
    item_stats = get_mirt_stats(full_fit)
  } else {
    item_stats = make_CTT_fit(items)$item_stats
  }

  #residualize loadings?
  if (!residualize_loadings) {
    #just sort by loading as is
    item_stats = item_stats %>% arrange(-loading)
  } else {
    #fit linear model to predict loading from difficulty
    loading_model = lm(loading ~ difficulty, data = item_stats)
    #save residuals
    item_stats$loading_resid = residuals(loading_model)
    #sort by residual
    item_stats = item_stats %>% arrange(-loading_resid)
  }

  #determine item sets
  if (is.null(difficulty_balance_groups)) {
    item_sets = make_item_list_for_loadings_method(item_stats$item, max_items)
  } else {
    item_sets = make_item_list_for_balanced_loadings_method(item_stats, max_items, difficulty_balance_groups)
  }

  #fit models with items from 3 to max
  #main loop
  additions = furrr::future_map(
    item_sets,
    function(item_set) {

      if (IRT) {
        #fit mirt
        fit = fit_mirt(items[, item_set, drop = F], mirt_args, reliability_at = reliability_at)
      } else {
        fit = make_CTT_fit(items[, item_set, drop = F])
      }

      #get stats
      y = compute_fit_stats(
        fit = fit,
        item_set = item_set,
        selection_method = selection_method,
        criterion_vars = criterion_vars,
        full_reliability = full_reliability,
        criterion_cors_full = criterion_cors_full,
        save_fits = save_fits,
        reliability_at = reliability_at
      )

      y
    }
  )

  additions
}

simple_cronbach_alpha <- function(data) {
  #replace NA with 0
  data[is.na(data)] <- 0

  # Number of items
  N <- ncol(data)

  # Compute variance-covariance matrix
  cov_matrix <- cov(data)

  # Average item variance (diagonal of covariance matrix)
  v_bar <- mean(diag(cov_matrix))

  # Average inter-item covariance (off-diagonal elements)
  off_diag <- cov_matrix[lower.tri(cov_matrix)]
  c_bar <- mean(off_diag)

  # Cronbach's alpha formula
  alpha <- (N * c_bar) / (v_bar + (N - 1) * c_bar)

  return(alpha)
}

#classical test theory "fit"
make_CTT_fit = function(items) {

  y = list(
    fit = NULL,
    scores = rowSums(items, na.rm = T) %>% as.matrix(),
    reliability = simple_cronbach_alpha(items) %>% as.vector()
  )

  #add biserials as loadings
  y$loadings = purrr::map_dbl(
    1:ncol(items),
    function(i) {
      sink(tempfile())
      y = psych::biserial(y$scores[, 1], items[, i]) %>% as.vector()
      sink()
      y
    }
  )

  #item stats mimic mirt
  y$item_stats = tibble(
    item = items %>% colnames(),
    loading = y$loadings,
    difficulty = NA,
    discrimination = NA,
    pass_rate = colMeans(items, na.rm = T)
  ) %>% mutate(
    difficulty = pnorm(pass_rate, lower.tail = F),
    discrimination = loading_to_slope(loading)
  )

  y
}


#' Abbreviate a scale
#'
#' This function abbreviates a scale by iteratively adding or removing items that are the least useful for predicting a criterion variable. The function can use a max loading, backwards, forwards, or genetic algorithm method to select items.
#'
#' @param items A data frame or matrix of items
#' @param criterion_vars A data frame of criterion variables
#' @param item_target The number of items to retain
#' @param method The method to use for item selection. Options are "backwards", "forwards", "max_loading", or "genetic".
#' @param selection_method The method to use for selecting items. Options are "rc" (average of correlation with criterion variable(s) and reliability), "r" (reliability), or "c" (correlation with criterion variable(s)).
#' @param mirt_args A list of arguments to pass to the mirt function
#' @param save_fits Whether to save the fits and scores for each item set. This might be useful for further analysis, but it also takes up memory.
#' @param seed A seed to use for reproducibility. Default is 1.
#' @param max_generations The maximum number of generations to use for the genetic algorithm. Default is 100.
#' @param population_size The size of the population to use for the genetic algorithm. Default is 100.
#' @param mutation_rate The mutation rate to use for the genetic algorithm. Default is 0.1.
#' @param selection_ratio The ratio of the population to select for the next generation. Default is 0.20.
#' @param stop_search_after_generations The number of generations to wait for no improvement before stopping the search. Default is 10.
#' @param include_parents Whether to include the parents in the next generation. Default is TRUE.
#' @param difficulty_balance_groups The number of groups to balance difficulty across. Default is NULL.
#' @param residualize_loadings Whether to residualize loadings based on difficulty for selection purposes. Default is FALSE.
#' @param reliability_at The reliability to use for selection. Default is "total".
#' @param IRT Whether to use IRT or classical test theory. Default is TRUE. If false, reliability will be calculated as Cronbach's alpha, loadings will be calculated as biserial correlations, and the score will be calculated as the sum of the correct items. This makes it much faster to run but less accurate.
#'
#' @return A list of results. You probably want to call `GG_scale_abbreviation()` on these.
#' @export
#' @examples
#' library(mirt)
#' #simulate some mirt data 2PL
#' set.seed(1)
#' dat = mirt::simdata(N = 1e3, itemtype = "2PL", a = runif(20, 0.5, 2), d = rnorm(20, sd = 0.5))
#' #fit the model
#' fit = mirt::mirt(dat, 1)
#' #scale abbreviation
#' short_scale = abbreviate_scale(as.data.frame(dat), method = "max_loading", item_target = 10)
#' #plot
#' GG_scale_abbreviation(short_scale)
#' #using CTT statistics instead
#' short_scale = abbreviate_scale(as.data.frame(dat), method = "max_loading", item_target = 10, IRT = F)
abbreviate_scale = function(
    items,
    criterion_vars = NULL,
    item_target,
    method = "forwards",
    selection_method = "rc",
    mirt_args = NULL,
    save_fits = T,
    seed = 1,
    max_generations = 100,
    population_size = 100,
    mutation_rate = 0.1,
    selection_ratio = 0.20,
    stop_search_after_generations = 10,
    include_parents = T,
    difficulty_balance_groups = NULL,
    residualize_loadings = F,
    reliability_at = "total",
    IRT = T
) {

  #start timer
  tictoc::tic()

  #stop timer on exit
  on.exit({
    tictoc::toc()
  })

  #use seed
  set.seed(seed)

  #check arguments
  assert_that(is.data.frame(items) | is.matrix(items))
  items = as.matrix(items) #make sure its a matrix to avoid some dropping issues
  assert_that(method %in% c("backwards", "forwards", "max_loading", "genetic"))
  if (selection_method == "cr") selection_method = "rc"
  assert_that(selection_method %in% c("rc", "r", "c", "cr"))
  #not non-NULL to both residualization and balancing
  if (!is.null(difficulty_balance_groups) & residualize_loadings) {
    stop("Cannot residualize loadings and balance by difficulty groups at the same time.", call. = F)
  }
  assert_that(item_target >= 3, msg = "Item target must be at least 3")
  assert_that(item_target < ncol(items), msg = "Item target count must be less than the number of items")

  #messages
  message(str_glue("Abbreviating scale using {mapvalues(selection_method, from = c('c', 'rc', 'r'), to = c('correlation with criterion variable(s)', 'average of correlation with creiterion variable(s) and reliability', 'reliability'), warn_missing = F)} method"))
  message(str_glue("Using the {method} method"))

  #use default mirt args if none given
  if (is.null(mirt_args)) {
    mirt_args = list(
      model = 1,
      itemtype = "2PL",
      technical = list(NCYCLES = 5000),
      verbose = F
    )
  }

  #fit full scale mirt
  if (IRT) {
    full_fit = rlang::exec(
      .fn = mirt::mirt,
      data = items,
      !!!mirt_args
    )
  } else {
    full_fit = list(
      fit = NULL,
      scores = rowSums(items, na.rm = T) %>% as.matrix(),
      reliability = simple_cronbach_alpha(items) %>% as.vector(),
      item_stats = make_CTT_fit(items)$item_stats
    )
  }

  #get scores and their full set cors
  if (IRT) {
    full_scores = mirt::fscores(full_fit, full.scores.SE = T)
  } else {
    full_scores = rowSums(items, na.rm = T) %>% as.matrix()
  }


  #reliability
  if (IRT) {
    full_reliability = get_reliability_at(full_fit, reliability_at)
  } else {
    full_reliability = simple_cronbach_alpha(items) %>% as.vector()
  }


  #make set of criterion vars
  if (is.null(criterion_vars)) {
    criterion_vars = tibble(
      full_score = full_scores[, 1]
    )
  }

  #criterion cors with full set
  full_cors = wtd.cors(
    bind_cols(
      score = full_scores[, 1],
      criterion_vars
    )
  )

  #main loop
  #prep a list
  item_set_results_all = list()

  if (method == "backwards") {
    #drop first, then based on results, drop a second etc. until desired size is reached
    items_to_drop = ncol(items) - item_target
    items_to_drop_seq = seq(ncol(items) - item_target)
    for (i in items_to_drop_seq) {
      message(str_glue("removing item {i} out of {items_to_drop} ({items_to_drop - i} remaining)"))

      #if the first round, just drop one item
      #dont need to use the prior item set
      if (i == 1) {

        item_set_results_all[[i]] = backwards_drop(
          #begin with all items
          all_items = items,
          current_selection = colnames(items),
          criterion_vars = criterion_vars,
          criterion_cors_full = full_cors,
          selection_method = selection_method,
          mirt_args = mirt_args,
          save_fits = save_fits,
          full_fit = full_fit,
          full_reliability = full_reliability,
          reliability_at = reliability_at,
          IRT = IRT
        )

        next

      } else {

        #best prior set
        best_prior_i = which.max(item_set_results_all[[i - 1]]$criterion_value)
        best_prior_set = item_set_results_all[[i - 1]]$item_set[[best_prior_i]]

        #use prior best set
        item_set_results_all[[i]] = backwards_drop(
          all_items = items,
          current_selection = best_prior_set,
          criterion_vars = criterion_vars,
          criterion_cors_full = full_cors,
          selection_method = selection_method,
          mirt_args = mirt_args,
          save_fits = save_fits,
          full_fit = full_fit,
          full_reliability = full_reliability,
          reliability_at = reliability_at,
          IRT = IRT
        )

        next
      }

    }
  }

  if (method == "forwards") {

    #pick first, then based on results, pick a second etc. until desired size is reached
    items_to_pick = item_target
    items_to_pick_seq = seq(item_target - 2) #because we start with 3 items (minimum for IRT)
    for (i in items_to_pick_seq) {

      if (i != 1) {
        message(str_glue("adding item {i + 2} out of {items_to_pick}"))
      }

      #if the first round, try each item, one at a time
      #dont need to use the prior item set
      if (i == 1) {
        item_set_results_all[[i]] = forwards_pick(
          #begin with all items
          items = items,
          current_items = c(),
          criterion_vars = criterion_vars,
          criterion_cors_full = full_cors,
          selection_method = selection_method,
          mirt_args = mirt_args,
          save_fits = save_fits,
          full_fit = full_fit,
          full_reliability = full_reliability,
          reliability_at = reliability_at,
          IRT = IRT
        )

        next

      } else {

        #best prior set
        best_prior_i = which.max(item_set_results_all[[i - 1]]$criterion_value)
        best_prior_set = item_set_results_all[[i - 1]]$item_set[[best_prior_i]]

        #use prior best set
        item_set_results_all[[i]] = forwards_pick(
          items = items,
          current_items = best_prior_set,
          criterion_vars = criterion_vars,
          criterion_cors_full = full_cors,
          selection_method = selection_method,
          mirt_args = mirt_args,
          save_fits = save_fits,
          full_fit = full_fit,
          full_reliability = full_reliability,
          reliability_at = reliability_at,
          IRT = IRT
        )

        next
      }

    }
  }

  #loadings method
  if (method == "max_loading") {
    item_set_results_all = max_loading_method(
      items = items,
      max_items = item_target,
      criterion_vars = criterion_vars,
      criterion_cors_full = full_cors,
      mirt_args = mirt_args,
      save_fits = save_fits,
      full_fit = full_fit,
      full_reliability = full_reliability,
      selection_method = selection_method,
      difficulty_balance_groups = difficulty_balance_groups,
      residualize_loadings = residualize_loadings,
      reliability_at = reliability_at,
      IRT = IRT
    )
  }

  #genetic algo
  if (method == "genetic") {
    item_set_results_all = abbreviate_by_genetic_algo(
      all_items = items,
      item_target = item_target,
      criterion_vars = criterion_vars,
      criterion_cors_full = full_cors,
      selection_method = selection_method,
      full_fit = full_fit,
      full_reliability = full_reliability,
      mirt_args = mirt_args,
      save_fits = save_fits,
      max_generations = max_generations,
      population_size = population_size,
      mutation_rate = mutation_rate,
      selection_ratio = selection_ratio,
      stop_search_after_generations = stop_search_after_generations,
      include_parents = include_parents,
      reliability_at = reliability_at,
      IRT = IRT
    )

    #split to a list
    item_set_results_all = split(item_set_results_all, item_set_results_all$generation)
  }

  #return results
  full_results = item_set_results_all %>% ldf_to_df(by_name = "set") %>% as_tibble()

  list(
    full_results = full_results,
    best_sets = full_results %>%
      filter(criterion_value == max(criterion_value), .by = set),
    method = method,
    #add results for full fit, for plotting
    full_fit_stats = compute_fit_stats(
      fit = list(fit = full_fit, scores = full_scores, reliability = full_reliability),
      item_set = 1:ncol(items),
      selection_method = selection_method,
      criterion_vars = criterion_vars,
      full_reliability = full_reliability,
      criterion_cors_full = full_cors,
      save_fits = save_fits
    )
  )

}


#' Test for differential item functioning (DIF)
#'
#' Tests are done following the `mirt` package approach outlined by Chalmers.
#'
#' @param items Item data to use
#' @param model Item model
#' @param group Group (2 groups at most)
#' @param fscores_pars Any extra scoring parameters used
#' @param messages Show messages
#' @param method Method
#' @param technical Further technical args to pass to mirt
#' @param itemtype Item type
#' @param verbose Verbose output
#' @param DIF_args Arguments to pass to mirt::DIF
#' @param multiple_testing_method Method to use for multiple testing correction, see p.adjust(). Default is "bonferroni"
#' @param ... Other arguments passed to mirt functions
#'
#' @return A list of results
#' @export
#'
#' @examples
#' library(mirt)
#' n = 1000
#' n_items = 10
#'
#' #slopes
#' set.seed(1)
#' a1 = runif(n_items, min = .5, max = 2)
#' a2 = a1
#' a2[1] = 0 #item doesnt work for this group
#' #intercepts
#' i1 = rnorm(n_items, mean = -0.5, sd = 2)
#' i2 = i1
#' i2[2] = -2 #item much harder for this group
#'
#' #simulate data twice
#' d1 = simdata(
#' a1,
#' i1,
#' N = n,
#' itemtype = "2PL",
#' mu = 0
#' )
#'
#' d2 = simdata(
#' a2,
#' i2,
#' N = n,
#'   itemtype = "2PL",
#'   mu = 1
#' )
#'
#' #combine
#' d = rbind(
#'   d1 %>% set_names("item_" + 1:n_items),
#'   d2 %>% set_names("item_" + 1:n_items)
#' ) %>% as.data.frame()
#'
#' #find the bias
#' DIF_results = DIF_test(d, model = 1, itemtype = "2PL", group = rep(c(1, 2), each = n))
#' DIF_results$effect_size_items$conservative
#' plot(DIF_results$fits$anchor_conservative)
#' plot(DIF_results$fits$anchor_conservative, type = "trace")
DIF_test = function(items, model, group, fscores_pars = list(full.scores = T, full.scores.SE = T), messages = T, method = "EM", technical = list(), itemtype = NULL, verbose = T, DIF_args = NULL, multiple_testing_method = "bonferroni", ...) {

  #deal with missing data in the group var
  group_keep = !is.na(group)
  items = items[group_keep, ]
  group = group[group_keep]


  #make mirt args
  mirt_args = c(list(data = items, model = model, technical = technical, verbose = verbose, method = method, itemtype = itemtype), list(...))
  mirt_args_set2 = mirt_args[!names(mirt_args) %in% c("model", "itemtype")]

  #regular fit joint group
  if (messages) message("There are 8 steps")
  if (messages) message("Step 1: Initial joint fit\n")
  mirt_fit = rlang::exec(mirt::mirt, !!!mirt_args)

  #step 3
  if (!is.character(group) && !is.factor(group)) group = factor(group)
  if (messages) message("\nStep 2: Initial MI fit")
  mirt_fit_MI = rlang::exec(mirt::multipleGroup, !!!mirt_args, group = group, invariance = c('intercepts','slopes', 'free_means', 'free_var'))

  #DIFs
  if (messages) message("\nStep 3: Leave one out MI testing")

  if (is.null(DIF_args)) {
    DIF_args = list(
      #test all pars in model
      which.par = mirt::coef(mirt_fit, simplify = T)$items %>% colnames(),
      scheme = "drop"
    )
  }

  #call DIF() with arguments
  DIFs = rlang::exec(
    mirt::DIF,
    MGmodel = mirt_fit_MI,
    !!!(mirt_args[!names(mirt_args) %in% c("data", "model", "group", "itemtype")]),
    !!!DIF_args
  )

  DIFs = DIFs %>% rownames_to_column("item")
  DIFs$number = 1:nrow(DIFs)

  #adjust p values
  DIFs$p_adj = DIFs$p %>% p.adjust(method = multiple_testing_method)

  #with significant DIF
  DIFs_detected_liberal = DIFs %>% filter(p < .05)
  DIFs_detected_conservative = DIFs %>% filter(p_adj < .05)

  #subset itmes
  items_noDIF_liberal = items %>% dplyr::select(!!setdiff(DIFs$item, DIFs_detected_liberal$item))
  items_noDIF_conservative = items %>% dplyr::select(!!setdiff(DIFs$item, DIFs_detected_conservative$item))

  #subset models
  #tricky!
  #if its a g only model, we dont have to do anything
  #but if its complex we need name format or Q matrix format
  #extract loadings matrix
  #convert to Q matrix

  mirt_fit_loadings = get_loadings(mirt_fit)
  model_noDIF_liberal_Q = mirt_fit_loadings %>% apply(MARGIN = 2, as.logical) %>% magrittr::set_rownames(rownames(mirt_fit_loadings))
  model_noDIF_conservative_Q = model_noDIF_liberal_Q

  #set unused items' rows to FALSE
  model_noDIF_liberal_Q[DIFs_detected_liberal$item, ] = F
  model_noDIF_conservative_Q[DIFs_detected_conservative$item, ] = F

  #fit together without DIF
  if (messages) message("\nStep 4: Fit without DIF items, liberal threshold")
  mirt_fit_noDIF_liberal = rlang::exec(mirt::mirt, model = mirt::mirt.model(model_noDIF_liberal_Q), !!!mirt_args_set2)

  if (messages) message("\nStep 5: Fit without DIF items, conservative threshold")
  mirt_fit_noDIF_conservative = rlang::exec(mirt::mirt, model = mirt::mirt.model(model_noDIF_conservative_Q), !!!mirt_args_set2)


  #with anchors
  if (messages) message("\nStep 6: Fit with anchor items, liberal threshold")
  mirt_fit_anchors_liberal = rlang::exec(mirt::multipleGroup, !!!mirt_args, group = group, invariance = c(items_noDIF_liberal %>% names(), 'free_means', 'free_var'))

  if (messages) message("\nStep 7: Fit with anchor items, conservative threshold")
  mirt_fit_anchors_conservative = rlang::exec(mirt::multipleGroup, !!!mirt_args, group = group, invariance = c(items_noDIF_conservative %>% names(), 'free_means', 'free_var'))


  #get scores
  if (messages) message("\nStep 8: Get scores")

  orig_scores = do.call(what = mirt::fscores, args = c(list(object = mirt_fit), fscores_pars))
  noDIF_scores_liberal = do.call(what = mirt::fscores, args = c(list(object = mirt_fit_noDIF_liberal), fscores_pars))
  noDIF_scores_conservative = do.call(what = mirt::fscores, args = c(list(object = mirt_fit_noDIF_conservative), fscores_pars))
  anchor_scores_liberal = do.call(what = mirt::fscores, args = c(list(object = mirt_fit_anchors_liberal), fscores_pars))
  anchor_scores_conservative = do.call(what = mirt::fscores, args = c(list(object = mirt_fit_anchors_conservative), fscores_pars))

  #in a data frame
  scores = list(
    #original scores
    original = orig_scores,

    #after DIF removal
    noDIF_liberal = noDIF_scores_liberal,
    noDIF_conservative = noDIF_scores_conservative,

    #anchor scores
    anchor_liberal = anchor_scores_liberal,
    anchor_conservative = anchor_scores_conservative
  )

  #effect sizes
  #this only works with 1 dimensional models
  #https://groups.google.com/forum/#!topic/mirt-package/hAj7jfdzsxY
  if (ncol(mirt_fit_loadings) == 1) {
    #item level
    effect_size_items = list(
      liberal = mirt::empirical_ES(mirt_fit_anchors_liberal, DIF = T, plot = F),
      conservative = mirt::empirical_ES(mirt_fit_anchors_conservative, DIF = T, plot = F)
    )

    #test level
    effect_size_test = list(
      liberal = mirt::empirical_ES(mirt_fit_anchors_liberal, DIF = F, plot = F),
      conservative = mirt::empirical_ES(mirt_fit_anchors_conservative, DIF = F, plot = F)
    )
  } else {
    #we fill in NULLS to keep structure
    #item
    effect_size_items = list(
      liberal = NULL,
      conservative = NULL
    )

    #test
    effect_size_test = list(
      liberal = NULL,
      conservative = NULL
    )
  }

  #out
  list(
    scores = scores,
    fits = list(
      original = mirt_fit,
      noDIF_liberal = mirt_fit_noDIF_liberal,
      noDIF_conservative = mirt_fit_noDIF_conservative,
      anchor_liberal = mirt_fit_anchors_liberal,
      anchor_conservative = mirt_fit_anchors_conservative
    ),
    DIF_stats = DIFs,
    effect_size_items = effect_size_items,
    effect_size_test = effect_size_test
  )

}

#convert from slope to loading
#' Convert from factor loading to discrimination
#'
#' @param x Vector of factor loadings
#' @param logit_scaling Whether to use logit scaling
#'
#' @return A vector of slopes (discrimination)
#' @export
slope_to_loading = function(x, logit_scaling = T) {
  if (logit_scaling) {
    scaling_factor = 3
  } else {
    scaling_factor = 1
  }

  sqrt(x^2 / (x^2 + scaling_factor))
}


#' Convert from slopes to factor loadings
#'
#' @param x A vector of slopes
#' @param logit_scaling Whether to use logit scaling
#'
#' @return A vector of loadings
#' @export
loading_to_slope = function(x, logit_scaling = T) {
  if (logit_scaling) {
    scaling_factor = 3
  } else {
    scaling_factor = 1
  }

  sqrt((scaling_factor*x^2) / (1 - x^2))
}

#' Calculate item gaps
#'
#' @param x Item data frame
#' @param group A grouping variable
#' @param return_data_frame Whether you want a data frame back, or just a vector
#'
#' @return a data frame or a vector
#' @export
#' @examples
#' set.seed(1)
#' X = matrix(rbinom(10000, 1, .5), ncol = 10)
#' group = rbinom(1000, 1, .5)
#' calc_item_gaps(X, group)
calc_item_gaps = function(x, group, return_data_frame = T) {

  #subset to complete cases for group
  no_na = !is.na(group)
  x = x[no_na, ]
  group = group[no_na] %>% as.factor()

  #input check
  assert_that(is.matrix(x) | is.data.frame(x))
  x = as.data.frame(x)
  assert_that(all(map_lgl(x, is.numeric)))
  assert_that(length(levels(group)) == 2)

  #decide focal group
  focal_group = levels(group)[1]
  alt_group = levels(group)[2]
  message(str_glue("Focal group is {focal_group}. Positive values mean that {focal_group} > {alt_group}"))

  #prep data frame
  res = tibble(
    item_i = 1:ncol(x),
    item = colnames(x),
    focal_pass_rate = map_dbl(x[group == focal_group, ], mean, na.rm = T),
    alt_pass_rate = map_dbl(x[group == alt_group, ], mean, na.rm = T),
    d_gap = qnorm(focal_pass_rate) - qnorm(alt_pass_rate)
  )

  #return
  if (return_data_frame) {
    return(res)
  } else {
    return(res$d_gap)
  }
}

#apply reversing
#internal function for below
apply_reversing = function(x, vars) {
  #to reverse
  x$reversed = x$loading < 0

  #copy values for non-reversed
  x$loading = if_else(x$reversed, -x$loading, x$loading)

  #loop over other vars and do the same
  for (v in vars) {
    x[[v]] = if_else(x$reversed, -x[[v]], x[[v]])
  }

  x
}


#' Calculate item associations with a criterion variable
#'
#' For using Jensen's method, one needs to calculate the correlation between each item and the criterion variable. This function does that, and also calculates the incremental R for each item if control variables are given.
#'
#' @param .item_data A data frame with item data from `get_mirt_stats()`
#' @param .data A data frame with the case-level data. Must have the items, the criterion variable, and any control variables.
#' @param criterion_var The name of the criterion variable
#' @param control_vars A vector of names of control variables to use in linear regression.
#' @param reverse_negative_loadings Whether to reverse the sign of the loadings, correlations, and model incremental R if the loading is negative. Default is TRUE.
#' @param winsorize_r2_adj Whether to winsorize the incremental R2 values to avoid negative values (probelmatic with square root transformation). Default is TRUE.
#'
#' @returns The input data frame with added columns for the correlation with the criterion variable and the incremental R if control variables are given.
#' @export
add_item_associations_with_criterion_var = function(.item_data, .data, criterion_var, control_vars = NULL, reverse_negative_loadings = T, winsorize_r2_adj = T, use_criterion_name = T) {

  #correlations are relatively simple
  item_cors = map_dbl(.item_data$item, function(v) {
    sink(tempfile())
    cor = psych::mixedCor(
      bind_cols(
        item = .data[[v]],
        criterion = .data[[criterion_var]]
      )
    )
    sink()

    cor[["rho"]][1, 2]
  })

  #if controls are given, then also do incremental regression models
  if (!is.null(control_vars)) {
    #loop items
    item_r_incs = map_dbl(.item_data$item, function(item) {

      form1 = str_glue("{criterion_var} ~ {str_c(control_vars, collapse = ' + ')}")
      form2 = str_glue("{criterion_var} ~ {item} + {str_c(control_vars, collapse = ' + ')}")

      #fit 2 models
      fit1 = lm(form1, data = .data)
      fit2 = lm(form2, data = .data)

      #incremental R2
      r2_inc = (summary(fit2)$r.squared - summary(fit1)$r.squared)

      #winsorize negative values?
      if (winsorize_r2_adj) {
        r2_inc = r2_inc %>% winsorise(lower = 0)
      }

      #sqrt to get r-like metric
      r_inc = r2_inc %>% {quietly(sqrt)(.)} %>% pluck("result")

      #reverse if beta is negative, so that we preserve directionality
      if (coef(fit2)[[item]] < 0) {
        r_inc = -r_inc
      }

      r_inc
    })
  }

  #return data with added variables
  y = bind_cols(.item_data, criterion_r = item_cors)

  if (!is.null(control_vars)) {
    y$criterion_r_inc = item_r_incs
  }

  #reverse negative loadings to avoid inflation
  if (reverse_negative_loadings) {
    y = apply_reversing(y, vars = intersect(c("criterion_r", "criterion_r_inc"), names(y)))
  }

  #use the criterion name as predix instead of just criterion
  if (use_criterion_name) {
    y[[str_glue("{criterion_var}_r")]] = y$criterion_r
    y$criterion_r = NULL

    if (!is.null(control_vars)) {
      y[[str_glue("{criterion_var}_r_inc")]] = y$criterion_r_inc
      y$criterion_r_inc = NULL
    }
  }

  y
}
