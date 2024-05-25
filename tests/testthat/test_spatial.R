context("spatial_")

#make simple dataset of US states
us_states = dplyr::bind_cols(
  datasets::state.center,
  datasets::state.x77
) %>% mutate(lon = x, lat = y)

#lag it
us_states_lagged = spatial_knn(us_states, vars = c("Income", "Illiteracy", "Life Exp"))

#try other names
us_states_lagged2 = spatial_knn(us_states %>% select(-lat, -lon), vars = c("Income", "Illiteracy", "Life Exp"), lon = "x", lat = "y")

test_that("spatial_knn", {
  #seems right
  expect_s3_class(us_states_lagged, "data.frame")
  expect_equivalent((ncol(us_states) + 3), ncol(us_states_lagged))
  expect_equivalent(names(us_states_lagged) %>% str_subset("_lag") %>% length(), 3)

  #same results as before with other variable names
  expect_equivalent(us_states_lagged$Income_lag, us_states_lagged2$Income_lag)
})

#compute autocors
test_that("spatial_lag_cors", {
  spatial_lags = spatial_lag_cors(us_states_lagged)
  spatial_lags_df = spatial_lag_cors(us_states_lagged, long_output = T)

  expect_equivalent(3, length(spatial_lags))
  expect_equivalent(3, nrow(spatial_lags_df))
})
