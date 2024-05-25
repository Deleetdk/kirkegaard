library(mirt)

set.seed(1)

d_sim = simdata(
  N = 1000,
  itemtype = '2PL',
  a = runif(50, .5, 2),
  d = runif(50, -2, 2)
)

irt_fit = mirt(d_sim, 1)

get_reliability_at(irt_fit, 'total')
get_reliability_at(irt_fit, c(0))
get_reliability_at(irt_fit, c(-2, -1, 0, 1, 2))


abbrev_at_1 = abbreviate_scale(
  d_sim,
  item_target = 20,
  selection_method = "r",
  reliability_at = 1
)

abbrev_at_0 = abbreviate_scale(
  d_sim,
  item_target = 20,
  selection_method = "r",
  reliability_at = 0
)

abbrev_at_m1 = abbreviate_scale(
  d_sim,
  item_target = 20,
  selection_method = "r",
  reliability_at = -1
)

abbrev_at_extremes = abbreviate_scale(
  d_sim,
  item_target = 20,
  selection_method = "r",
  reliability_at = c(-2, 2)
)

abbrev_at_total = abbreviate_scale(
  d_sim,
  item_target = 20,
  selection_method = "r",
  reliability_at = "total"
)

#extract reliability data
abbrev_reliabilities = map2_dfr(list(abbrev_at_m1, abbrev_at_0, abbrev_at_1, abbrev_at_extremes, abbrev_at_total), c(-1, 0, 1, "-2, 2", "total"), function(abbrev_obj, target) {
  abbrev_obj$best_sets %>%
    tail(1) %>%
    pull(fit) %>%
    extract2(1) %>%
    extract2("fit") %>%
    get_reliabilities() %>%
    mutate(target = as.character(target))
}) %>%
  bind_rows(
    get_reliabilities(irt_fit) %>%
      mutate(target = "full")
  )

#plot reliability data
abbrev_reliabilities %>%
  filter(is_between(z,-3, 3)) %>%
  mutate(
    target = factor(target)
  ) %>%
  ggplot(aes(z, y = rel, color = target)) +
  geom_line() +
  theme_bw() +
  #add target values at vertical lines
  geom_vline(xintercept = c(-1, 0, 1), linetype = "dashed")
