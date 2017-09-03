library(tibble)

calc_dy <- function(x_vec, y_vec) {
  dy_vec <- rep(NA, times = length(y_vec))
  dy_vec[2:length(dy_vec)] <- diff(y_vec)/diff(x_vec)
  return(dy_vec)
}

package_test_data <- function(age_days, y_vec, dy_vec) {
  age_days <- age_days
  y_vales <- y_vec
  fitgrid <- dy_vec

  test_data_tibble <- tibble::tibble(
    subjid = 1001L,
    fit = list(list(fitgrid = fitgrid))
  )
  return(test_data_tibble)
}

# x values generator functions -------------------------------------------------

gen_agedays <- function(from = 1L, to = 60L, by = 7L) {
  return(seq.int(from = from, to = to, by = by))
}

# y values generator functions -------------------------------------------------

gen_linear_y_data <- function(age_days) {
  y <- age_days
}

gen_zero_y_data <- function(age_days) {
  y <- rep(0.0, times = length(age_days))
}

gen_saw_tooth_y_data <- function(a_val = 4, age_days) {
  # https://en.wikipedia.org/wiki/Triangle_wave
  x_vals_for_sawtooth <- seq(-1, 1, length.out = length(age_days))
  floor_op_part <- floor( (x_vals_for_sawtooth / a_val) + (1/2))
  y_vec <- (2 / a_val) * (x_vals_for_sawtooth - (a_val * floor_op_part)) * (-1)^(floor_op_part)
}

# dy values generator functions ------------------------------------------------

gen_dy_data <- function(age_days, y_vec) {
  heights <- y_vec
  d_heights <- calc_dy(age_days, heights)

  fitgrid <- data.frame(
    x  = age_days,
    y  = heights,
    dy = d_heights
  )
  return(fitgrid)
}

# trajectory calculation functions ---------------------------------------------

# 'curry' the built-in `mean` function with NA removal option.
mean_na_rm_generator <- function() {
  return(function(x) {
    mean(x, na.rm = TRUE)
  })
}
mean_na_rm <- mean_na_rm_generator()

# test cases -------------------------------------------------------------------

test_that("mean of non-z velocity is constant", {
  age_days <- gen_agedays()
  y_vals <- gen_linear_y_data(age_days)
  dy_df <- gen_dy_data(age_days, y_vals)

  test_data <- package_test_data(age_days, y_vals, dy_df)
  avg_velocity <- get_traj_velocity(test_data, z_score = FALSE)
  testthat::expect_equal(avg_velocity$avgder[[1]], 1.00, label = "constant velocity")
})

test_that("mean of non-z velocity for sawtooth growth is zero", {
  age_days <- gen_agedays(from = 1, to = 300, by = 14)
  y_vals <- gen_saw_tooth_y_data(a_val = 4, age_days = age_days)
  dy_df <- gen_dy_data(age_days, y_vals)

  test_data <- package_test_data(age_days, y_vals, dy_df)
  # Supplying our own custom function `mean_na_rm`, as well as using
  # a tolerance threshold to allow for numerical differences, e.g.,
  # 0.005 == 0
  avg_velocity <- get_traj_velocity(test_data, z_score = FALSE, FUN = mean_na_rm)
  testthat::expect_equal(avg_velocity$avgder[[1]], 0.00,
                         tolerance = 5e-3,
                         label = "zero mean velocity")
})

