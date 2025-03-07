#https://rpubs.com/EmilOWK/IRT_DIF_examples

context("mirt")

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

  #repeat with CTT
  #forwards
  d_sim_abbrev_forwards_CTT = abbreviate_scale(
    d_sim,
    method = "forwards",
    item_target = max_items,
    IRT = F
  )

  #backwards
  d_sim_abbrev_backwards_CTT = abbreviate_scale(
    d_sim,
    method = "backwards",
    item_target = max_items,
    IRT = F
  )

  #max loading
  d_sim_abbrev_max_loading_CTT = abbreviate_scale(
    d_sim,
    method = "max_loading",
    item_target = max_items,
    IRT = F
  )

  #genetic
  d_sim_abbrev_genetic_CTT = abbreviate_scale(
    d_sim,
    method = "genetic",
    item_target = max_items,
    max_generations = 3,
    population_size = 10,
    IRT = F
  )

  #turn off sink
  sink()

  #verify outputs are correct
  #are lists
  expect_is(d_sim_abbrev_forwards, "list")
  expect_is(d_sim_abbrev_backwards, "list")
  expect_is(d_sim_abbrev_max_loading, "list")
  expect_is(d_sim_abbrev_genetic, "list")

  expect_is(d_sim_abbrev_forwards_CTT, "list")
  expect_is(d_sim_abbrev_backwards_CTT, "list")
  expect_is(d_sim_abbrev_max_loading_CTT, "list")
  expect_is(d_sim_abbrev_genetic_CTT, "list")

  #has some of the right items
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_forwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_backwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_max_loading)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_genetic)))

  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_forwards_CTT)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_backwards_CTT)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_max_loading_CTT)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_genetic_CTT)))

  #test the plots
  p_forwards = d_sim_abbrev_forwards %>% GG_scale_abbreviation()
  p_backwards = d_sim_abbrev_backwards %>% GG_scale_abbreviation()
  p_max_loading = d_sim_abbrev_max_loading %>% GG_scale_abbreviation()
  p_genetic = d_sim_abbrev_genetic %>% GG_scale_abbreviation()

  p_forwards_CTT = d_sim_abbrev_forwards_CTT %>% GG_scale_abbreviation()
  p_backwards_CTT = d_sim_abbrev_backwards_CTT %>% GG_scale_abbreviation()
  p_max_loading_CTT = d_sim_abbrev_max_loading_CTT %>% GG_scale_abbreviation()
  p_genetic_CTT = d_sim_abbrev_genetic_CTT %>% GG_scale_abbreviation()

  #check the plots
  expect_true(p_forwards %>% is.ggplot())
  expect_true(p_backwards %>% is.ggplot())
  expect_true(p_max_loading %>% is.ggplot())
  expect_true(p_genetic %>% is.ggplot())

  expect_true(p_forwards_CTT %>% is.ggplot())
  expect_true(p_backwards_CTT %>% is.ggplot())
  expect_true(p_max_loading_CTT %>% is.ggplot())
  expect_true(p_genetic_CTT %>% is.ggplot())

})

test_that("mirt helper functions", {
  set.seed(1)

  data = mirt::simdata(
    seq(0.2, 2, length.out = 5),
    seq(-2, 2, length.out = 5),
    1000,
    itemtype = "2PL")

  fit = mirt::mirt(data, 1, verbose = F)
  rel = get_reliabilities(fit)
  item_stats = get_mirt_stats(fit)

  expect_true(is.data.frame(rel))

  expect_true(is.data.frame(item_stats))
  expect_true(all(c("item", "loading", "discrimination", "difficulty", "pass_rate") %in% colnames(item_stats)))
})


test_that("add_item_associations_with_criterion_var", {
  #first we need to generate some correlated data
  set.seed(1)
  n = 1000
  n_items = 10
  d_cases = MASS::mvrnorm(n, mu = rep(0, 2), Sigma = matrix(c(1, 0.5, 0.5, 1), nrow = 2))
  colnames(d_cases) = c("trait", "confounder")
  d_cases = as.data.frame(d_cases)

  #make the outcome
  d_cases$outcome = (d_cases$trait + d_cases$confounder + rnorm(n)) %>% standardize()

  #generate some item data
  d_items = mirt::simdata(
    a = runif(n_items, min = -1, max = 2),
    d = rnorm(n_items),
    Theta = matrix(d_cases$trait, ncol = 1),
    itemtype = "2PL"
  )

  #fit the model
  fit = mirt::mirt(d_items, 1, verbose = F, itemtype = "2PL")

  #get item stats
  item_stats = get_mirt_stats(fit)

  #combine data
  d_cases = bind_cols(
    d_cases,
    d_items
  )

  #add associations
  item_stats_default = add_item_associations_with_criterion_var(
    .item_data = item_stats,
    criterion_var = "outcome",
    .data = d_cases,
    control_vars = "confounder"
  )

  item_stats_no_reverse = add_item_associations_with_criterion_var(
    .item_data = item_stats,
    criterion_var = "outcome",
    .data = d_cases,
    control_vars = "confounder",
    reverse_negative_loadings = F
  )

  item_stats_no_control = add_item_associations_with_criterion_var(
    .item_data = item_stats,
    criterion_var = "outcome",
    .data = d_cases
  )

  item_stats_no_winsorize = add_item_associations_with_criterion_var(
    .item_data = item_stats,
    criterion_var = "outcome",
    control_vars = "confounder",
    .data = d_cases,
    winsorize_r2_adj = F
  )

  #check that structure is correct
  expect_true(all(c("criterion_r", "criterion_r_inc", "reversed") %in% colnames(item_stats_default)))

  expect_true(all(c("criterion_r", "criterion_r_inc") %in% colnames(item_stats_no_reverse)))
  expect_true(!all(c("reversed") %in% colnames(item_stats_no_reverse)))

  expect_true(all(c("criterion_r") %in% colnames(item_stats_no_control)))
  expect_true(!all(c("criterion_r_inc") %in% colnames(item_stats_no_control)))

  expect_true(all(c("criterion_r", "criterion_r_inc") %in% colnames(item_stats_no_winsorize)))

  #check reversing is absent
  expect_true(any(item_stats_no_reverse$loading > 0) && any(item_stats_no_reverse$loading < 0))

  #check correlations
  expect_true(cor(item_stats_default$loading, item_stats_default$criterion_r) > 0.9)
  expect_true(cor(item_stats_default$loading, item_stats_default$criterion_r_inc) > 0.8)

  expect_true(cor(item_stats_no_reverse$loading, item_stats_no_reverse$criterion_r) > 0.99)

  expect_true(cor(item_stats_no_control$loading, item_stats_no_control$criterion_r) > 0.9)

  expect_true(cor(item_stats_no_winsorize$loading, item_stats_no_winsorize$criterion_r) > 0.9)
})
