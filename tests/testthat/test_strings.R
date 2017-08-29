context("str_")

# str_uniquify ------------------------------------------------------------
x = sample(LETTERS[1:10], size = 20, replace = T)

test_that("str_uniquify", {
  #uniquify normally
  expect_true(x %>% str_uniquify %>% duplicated %>% any %>% `!`)

  #custom suffix using a second %d.
  expect_true(x %>% str_uniquify(" [%d/%d]") %>% duplicated %>% any %>% `!`)
})



# str_to_upper_initial ----------------------------------------------------

test_that("str_uniquify", {
  #simple case
  expect_true(str_to_upper_initial("bleh") == "Bleh")

  #multiple words
  expect_true(str_to_upper_initial("bleh blah") == "Bleh blah")

  #multiple strings
  expect_true(all(str_to_upper_initial(c("bleh blah", "bleh blah", "bleh blah")) == c("Bleh blah", "Bleh blah", "Bleh blah")))
})
