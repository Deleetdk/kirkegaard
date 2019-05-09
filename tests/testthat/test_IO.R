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


# read_vcf ----------------------------------------------------------------

#vcf test file
vcf_file = system.file("extdata", "test.vcf", package = "kirkegaard")

#read
vcf_test = suppressWarnings(read_vcf(vcf_file))
vcf_test2 = read_vcf(vcf_file, var_id = "chrpos")

test_that("read_vcf", {
  #right output type
  expect_s3_class(vcf_test, "data.frame")
  expect_s3_class(vcf_test2, "data.frame")

  #right nrow
  expect_equal(nrow(vcf_test), 4)
  expect_equal(nrow(vcf_test2), 4)

  #right columns
  expect_equal(vcf_test %>% names() %>% .[1], "id")
  expect_equal(vcf_test2 %>% names() %>% .[1], "id")

  #warning on suspicious input
  expect_warning(read_vcf(vcf_file), "There were duplicated IDs among the variants")
})
