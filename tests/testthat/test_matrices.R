### Some matrix functions

context("MAT_")



# MAT_find_size -----------------------------------------------------------

test_that("find_size", {
  #without diagonals
  expect_equivalent(MAT_find_size(1), 2)
  expect_equivalent(MAT_find_size(3), 3)

  #with
  expect_equivalent(MAT_find_size(3, diag = T), 2)
  expect_equivalent(MAT_find_size(6, diag = T), 3)
})


# MAT_half ----------------------------------------------------------------

m = matrix(1:9, ncol = 3)

test_that("half", {
  #without diagonals
  expect_equivalent(MAT_half(m), c(2, 3, 6))
  expect_equivalent(MAT_half(m, lower = F), c(4, 7, 8))

  #with
  expect_equivalent(MAT_half(m, diag = T), c(1, 2, 3, 5, 6, 9))
  expect_equivalent(MAT_half(m, lower = F, diag = T), c(1, 4, 5, 7, 8, 9))
})



# MAT_vector2full ---------------------------------------------------------

m = matrix(1:9, ncol=3)
diag(m) = 0

test_that("half", {
  #without diagonals
  expect_equivalent(MAT_vector2full(MAT_half(m)), matrix(c(0, 2, 3, 2, 0, 6, 3, 6, 0), ncol=3))
  expect_equivalent(MAT_vector2full(MAT_half(m, lower = F), byrow = F), matrix(c(0, 4, 7, 4, 0, 8, 7, 8, 0), ncol=3))

  #with
  expect_equivalent(MAT_vector2full(MAT_half(m, diag = T), diag = T), matrix(c(0, 2, 3, 2, 0, 6, 3, 6, 0), ncol=3))
  expect_equivalent(MAT_vector2full(MAT_half(m, lower = F, diag = T), diag = T, byrow = T), matrix(c(0, 4, 7, 4, 0, 8, 7, 8, 0), ncol=3))
})



# MAT_half2full -----------------------------------------------------------

m = matrix(1:9, ncol=3)
rownames(m) = letters[1:3]
colnames(m) = LETTERS[1:3]

m2 = matrix(1:9, ncol = 3)

test_that("half2full", {
  #names
  expect_equivalent(dimnames(m %>% MAT_half2full), dimnames(m))
  expect_equivalent(dimnames(m %>% MAT_half2full(lower=F)), dimnames(m))
  expect_equivalent(dimnames(m %>% MAT_half2full(diag=T)), dimnames(m))
  expect_equivalent(dimnames(m %>% MAT_half2full(lower=F, diag=T)), dimnames(m))

  #values
  expect_equivalent(m2 %>% MAT_half2full, matrix(c(0, 2, 3, 2, 0, 6, 3, 6, 0)))
  expect_equivalent(m2 %>% MAT_half2full(lower = F), matrix(c(0, 4, 7, 4, 0, 8, 7, 8, 0)))
  expect_equivalent(m2 %>% MAT_half2full(diag = T), matrix(c(1, 2, 3, 2, 5, 6, 3, 6, 9)))
  expect_equivalent(m2 %>% MAT_half2full(lower = F, diag = T), matrix(c(1, 4, 7, 4, 5, 8, 7, 8, 9)))
})


# MAT_divide_rowwise ------------------------------------------------------

#TODO: make tests
