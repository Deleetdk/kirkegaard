context("vector operations")

# discretize --------------------------------------------------------------

#fixed vector of values
set.seed(1)
vals = c(0.001, runif(10), .999)

test_that("discretize", {
  #break count
  expect_length(discretize(vals, breaks = 10) %>% levels(), 10)
  expect_length(discretize(vals, breaks = 5) %>% levels(), 5)
  expect_length(discretize(vals, breaks = seq(0, 1, length.out = 6)) %>% levels(), 5)

  #labels
  expect_length(discretize(vals, breaks = 5, labels = "integer") %>% unique(), 5)
  expect_length(discretize(vals, breaks = 5, labels = "number") %>% unique(), 5)
  expect_length(discretize(vals, breaks = 5, labels = "midpoint") %>% levels(), 5)
  expect_length(discretize(vals, breaks = 5, labels = "interval") %>% levels(), 5)
})
