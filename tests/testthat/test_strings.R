context("str_")

# str_uniquify ------------------------------------------------------------
x = sample(LETTERS[1:10], size = 20, replace = T)

test_that("str_uniquify", {
  #uniquify normally
  expect_true(x %>% str_uniquify %>% duplicated %>% any %>% `!`)

  #custom suffix using a second %d.
  expect_true(x %>% str_uniquify(" [%d/%d]") %>% duplicated %>% any %>% `!`)
})
