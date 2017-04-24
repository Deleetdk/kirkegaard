context("meta_")

# a meta-analysis object to use -------------------------------------------

data(european_ancestry)
meta = metafor::rma(european_ancestry$r, sei = european_ancestry$SE_r)


# GG_funnel ---------------------------------------------------------------

test_that("funnel", {
  expect_s3_class(GG_funnel(meta), "ggplot")
  expect_s3_class(GG_funnel(meta, .study_CI = T), "ggplot")
})


# GG_forest ---------------------------------------------------------------

test_that("forest", {
  #bare
  expect_s3_class(GG_forest(meta), "ggplot")

  #custom names
  expect_s3_class(GG_forest(meta, .names = european_ancestry$Sample_type), "ggplot")
})
