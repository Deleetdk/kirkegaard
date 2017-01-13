### tests for vector transformation functions

context("vector_transform")


# v_to_df -----------------------------------------------------------------

test_that("v_to_df", {
  expect_equal(v_to_df(1:4), data.frame(1:4) %>% set_colnames("value"))
  expect_equal(v_to_df(c(a = 1, b = 2, c = 3, d = 4)),
                    data.frame(1:4) %>% set_rownames(letters[1:4]) %>% set_colnames("value"))
  expect_equal(v_to_df(c(a = 1, b = 2, c = 3, d = 4), name_col = "name"),
                    data.frame(name = letters[1:4], value = 1:4))
})


