context("merge_datasets")

# merge_datasets ----------------------------------------------------------
#some data to merge
silence(
  {
  d1 = iris[1:75, ] #split in two
  d2 = iris[76:150, ]
  set.seed(1);d2_na = miss_add_random(d2)
  t = merge_datasets(d1, d2, restore_factors = T) #merge into one
  t2 = silence(merge_datasets(d1, d2, join = "left"))
  t3 = silence(merge_datasets(d1, d2, join = "right"))
  t4 = merge_datasets(iris[1], iris[2:5],restore_factors = T)
  }
)



# tests -------------------------------------------------------------------
silence(
  test_that("Merge data frames", {
    expect_equal(t, iris, check.attributes = F) #because everything went back to original position
    expect_equal(t2, d1) #because nothing was joined
    expect_equal(t3, d2) #because nothing was joined
    expect_equal(t4, iris, check.attributes = F) #if not, likely that drop=F is needed!
    expect_equal(iris, merge_datasets_multi(iris[1:50, ], iris[51:100, ], iris[101:150, ], restore_factors = T), check.attributes = F)
  })
)


