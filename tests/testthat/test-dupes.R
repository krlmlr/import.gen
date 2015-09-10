context("dupes")

test_that("Duplicates are detected", {
  pkg <- disposables::make_packages(aa = { a <- identity; b <- identity }, bb = { a <- identity })
  on.exit(disposables::dispose_packages(pkg), add = TRUE)

  symbols <- find_symbols(c("aa", "bb"))
  expect_equal(symbols, data_frame(name = kimisc::ofactor(c("aa", "aa", "bb")),
                                   symbol = c("a", "b", "a"),
                                   keep = c(FALSE, TRUE, TRUE)),
               ignore_col_order = FALSE, ignore_row_order = FALSE)
})

test_that("Package order is kept", {
  pkg <- disposables::make_packages(aa = { a <- identity; b <- identity }, bb = { a <- identity })
  on.exit(disposables::dispose_packages(pkg), add = TRUE)

  symbols <- find_symbols(c("bb", "aa"))
  expect_equal(symbols, data_frame(name = kimisc::ofactor(c("bb", "aa", "aa")),
                                   symbol = c("a", "a", "b"),
                                   keep = c(FALSE, TRUE, TRUE)),
               ignore_col_order = FALSE, ignore_row_order = FALSE)
})
