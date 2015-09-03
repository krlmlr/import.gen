context("comment")

test_that("First non-comment starts with import::from", {
  ret <- from("clipr", .output = "return")
  comments <- grepl("^#", ret)
  expect_match(ret[[which(!comments)[[1L]]]], "^import::from")

  # Test very long generating code
  ret <- from(rep("clipr", getOption("width") %/% 3), .output = "return")
  comments <- grepl("^#", ret)
  expect_match(ret[[which(!comments)[[1L]]]], "^import::from")
})
