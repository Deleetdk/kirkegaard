context("IO")


# write_clipboard ---------------------------------------------------------

test_that("write_clipboard", {
  #print output not changed
  #had a bug with this previously
  expect_equivalent(iris[-5] %>% cor() %>% write_clipboard(print = F), iris[-5] %>% cor())
  expect_equivalent(iris[-5] %>% write_clipboard(print = F), iris[-5])

  #return modified object
  expect_type(iris[-5] %>% cor() %>% write_clipboard(print = F, return_modified = T), "list") %>%
    expect_s3_class("data.frame")

  #print
  expect_output(write_clipboard(iris, print = T))
})
