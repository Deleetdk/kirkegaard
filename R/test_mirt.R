#https://rpubs.com/EmilOWK/IRT_DIF_examples


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

  #turn off sink
  sink()

  #verify outputs are correct
  #are lists
  expect_is(d_sim_abbrev_forwards, "list")
  expect_is(d_sim_abbrev_backwards, "list")
  expect_is(d_sim_abbrev_max_loading, "list")
  expect_is(d_sim_abbrev_genetic, "list")

  #has some of the right items
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_forwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_backwards)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_max_loading)))
  expect_true(all(c("full_results", "best_sets", "method") %in% names(d_sim_abbrev_genetic)))

  #test the plots
  p_forwards = d_sim_abbrev_forwards %>% GG_scale_abbreviation()
  p_backwards = d_sim_abbrev_backwards %>% GG_scale_abbreviation()
  p_max_loading = d_sim_abbrev_max_loading %>% GG_scale_abbreviation()
  p_genetic = d_sim_abbrev_genetic %>% GG_scale_abbreviation()

  #check the plots
  expect_true(p_forwards %>% is.ggplot())
  expect_true(p_backwards %>% is.ggplot())
  expect_true(p_max_loading %>% is.ggplot())
  expect_true(p_genetic %>% is.ggplot())

})

test_that("mirt helper function", {
  set.seed(1)
  data = mirt::simdata(seq(0.2, 2, length.out = 5), seq(-2, 2, length.out = 5), 1000, itemtype = "2PL")
  fit = mirt::mirt(data, 1, verbose = F)
  rel = get_reliabilities(fit)
  item_stats = get_mirt_stats(fit)

  expect_true(is.data.frame(rel))

  expect_true(is.data.frame(item_stats))
  expect_true(all(c("item", "loading", "discrimination", "difficulty", "pass_rate") %in% colnames(item_stats)))
})
