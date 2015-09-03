context("dots")

test_that("dots are equivalent to .pkgs", {
  expect_equal(from("rpart", .output = "return"), from(.pkgs = "rpart", .output = "return"))
})
