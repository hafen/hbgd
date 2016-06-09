
# https://github.com/jimhester/lintr
if (requireNamespace("lintr", quietly = TRUE) & FALSE) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free(cache = TRUE)
  })
}
