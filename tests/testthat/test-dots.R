context("dots")

test_that("dots are equivalent to .pkgs", {
  expect_equal(from("rpart", .output = "return", .comment = FALSE),
               from(.pkgs = "rpart", .output = "return", .comment = FALSE))
})
