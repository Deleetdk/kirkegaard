context("rvest wrappers")


# write_rvest, read_rvest ------------------------------------------------------------------

#download google front
google = xml2::read_html("http://google.com")

#save
write_rvest(google, "google.rvest")

#test
test_that("write/read_rvest", {
  expect_equal(google, read_rvest("google.rvest"))
})

#delete file
file.remove("google.rvest")
