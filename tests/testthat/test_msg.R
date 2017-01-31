context("msg etc.")


# silence -----------------------------------------------------------------
#make stuff shut up

test_that("silence", {
  #minimal silent stuff
  expect_equivalent(capture.output(silence(warning("test"), messages = T, startupmessages = T)), character())
  expect_equivalent(capture.output(silence(message("test"), warnings = T, startupmessages = T)), character())
})




