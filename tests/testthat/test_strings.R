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

  #padding
  expect_true(all_the_same(str_length(str_uniquify(rep("ab", 10), "%d", pad = T))))
  expect_true(all_the_same(str_length(str_uniquify(rep("ab", 100), "%d", pad = T))))
})


# str_legalize ------------------------------------------------------------

test_that("str_legalize", {
  #slash
  expect_identical(str_legalize("a/b"), "a_b")
  #backslash
  expect_identical(str_legalize("a\\b"), "a_b")
  #ambersand
  expect_identical(str_legalize("a&b"), "a_and_b")
  #%
  expect_identical(str_legalize("%a"), "pcta")
  #dash
  expect_identical(str_legalize("a-b"), "a_b")
  #slash
  expect_identical(str_legalize("a/b"), "a_b")
  #backslash
  expect_identical(str_legalize("a\\b"), "a_b")
  #initial digit
  expect_identical(str_legalize("123"), "x123")
  #initial ?
  expect_identical(str_legalize("?123"), "x123")
  #empty and duplicated
  expect_identical(str_legalize(c("", "")), c("x_1", "x_2"))
})


# str_round ---------------------------------------------------------------


test_that("str_round", {
  expect_identical(str_round(seq(0, 7, by = 1), 2, less_than = 2, more_than = 5),
                   c("<2.00", "<2.00", "2.00", "3.00", "4.00", "5.00", ">5.00",
                     ">5.00"))

  expect_identical(str_round(1.123, 2), "1.12")
  expect_identical(str_round(1.1000000, 3), "1.100")
})
