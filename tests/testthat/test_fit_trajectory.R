
context("fit trajectory")

expect_class <- function(x, class) {
  expect_true(inherits(x, class)) # nolint
}

expect_data_frame <- function(x, colnames) {
  expect_class(x, "data.frame") # nolint
  expect_equivalent(colnames(x), colnames)
}

expect_fit_trajectory <- function(x, data) {
  expect_data_frame(x$xy, c("x", "y", "subjid", "idx", "yfit", "z", "zfit")) # nolint
  expect_class(x$fit, "numeric") # nolint
  expect_data_frame(x$fitgrid, c("x", "y", "z", "dy", "dz")) # nolint
  expect_data_frame(x$checkpoint, c("x", "y", "z", "zcat")) # nolint
  expect_class(x$pars, "list") # nolint
  expect_class(x$resid, "numeric") # nolint
  expect_equivalent(x$data, data) # nolint
  expect_class(x$sex, "character") # nolint
  expect_class(x$x_var, "character") # nolint
  expect_class(x$y_var, "character") # nolint
  expect_class(x$method, "character") # nolint

  expect_class(x, "fittedTrajectory") # nolint
}


test_that("basic class obj test", {

  dt <- subset(cpp, subjid == 2)
  fit <- fit_trajectory(dt, get_fit(cpp, y_var = "wtkg"))

  expect_fit_trajectory(fit, dt)

})
