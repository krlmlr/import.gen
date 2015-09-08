context("dupes")

test_that("Duplicates are detected", {
  pkg <- disposables::make_packages(aa = { a <- identity; b <- identity }, bb = { a <- identity })
  on.exit(disposables::dispose_packages(pkg), add = TRUE)

  symbols <- find_symbols(c("aa", "bb"))
  expect_equal(symbols, data_frame(name = c("aa", "aa", "bb"),
                                   symbol = c("a", "b", "a"),
                                   keep = c(FALSE, TRUE, TRUE)))
})
