
context("fit trajectory")

expect_class <- function(x, class) {
  testthat::expect_true(inherits(x, class))
}

expect_data_frame <- function(x, colnames) {
  expect_class(x, "data.frame") # nolint
  testthat::expect_equivalent(colnames(x), colnames)
}

expect_fit_trajectory <- function(x, data) {
  expect_data_frame(x$xy, c("x", "y", "idx", "yfit", "z", "zfit")) # nolint
  expect_class(x$fit, "numeric") # nolint
  expect_data_frame(x$fitgrid, c("x", "y", "z", "dy", "dz")) # nolint
  expect_data_frame(x$checkpoint, c("x", "y", "z", "zcat")) # nolint
  expect_class(x$pars, "list") # nolint
  expect_class(x$resid, "numeric") # nolint
  testthat::expect_equivalent(x$data, data)
  expect_class(x$sex, "character") # nolint
  expect_class(x$x_var, "character") # nolint
  expect_class(x$y_var, "character") # nolint
  expect_class(x$method, "character") # nolint

  expect_class(x, "fittedTrajectory") # nolint
}


test_that("basic class obj test", {
  dt <- subset(cpp, subjid == 2)
  fit <- fit_trajectory(dt, get_fit(cpp, y_var = "wtkg", method = "fda"))

  expect_fit_trajectory(fit, dt)
})


test_that("plots", {
  mod <- get_fit(cpp, y_var = "wtkg", method = "rlm")
  fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
  plot(fit)
  plot(fit, center = TRUE)
  plot(fit, hover = c("wtkg", "bmi", "waz", "haz"))

  expect_silent({
    plot_z(fit)
  })

  expect_silent({
    plot_velocity(fit)
  })

  expect_silent({
    plot_zvelocity(fit)
  })
})
