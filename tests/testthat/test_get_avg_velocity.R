library(tibble)
# library(brokenstick)

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

#' Corrects SMOCC dataset from \link{\code{brokenstick}} package into
#' \link{\code{hbgd}} format.
#'
bstick_smocc_to_hbgd <- function(data) {
  # Rename 'id' column to 'subjid'
  colnames(data)[colnames(data) == "id"] <- "subjid"
  # Rename 'age' column to 'agedays'
  colnames(data)[colnames(data) == "age"] <- "agedays"
  # Rename 'hgt' column to 'htcm'
  colnames(data)[colnames(data) == "hgt"] <- "htcm"
  # Rename 'wgt' column to 'wtkg'
  colnames(data)[colnames(data) == "wgt"] <- "wtkg"
  # Rename 'hgt.z' column to 'haz'
  colnames(data)[colnames(data) == "hgt.z"] <- "haz"


  # Change the 'sex' column from factors to characters.
  sex_col <- as.character(data$sex)
  # Change the case of letters in the 'sex' column
  sex_col <- replace(sex_col, sex_col == "female", "Female")
  sex_col <- replace(sex_col, sex_col == "male", "Male")
  data$sex <- sex_col

  # Remove observations where the 'sex' variable is not recorded
  data <- data[data$sex != "", ]
  # Remove observations where the 'sex' variable is NA
  data <- data[!is.na(data$sex), ]

  return(data)
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

test_that("number of subjects are the same", {
  smc <- get_smocc_data()[1:1000,]
  wandfit <- get_fit(smc, y_var = "haz", method = "wand")
  smc_tr <- hbgd::fit_all_trajectories(data, mod_fit)
  avg_velocity <- get_traj_velocity(smc_tr, z_score = TRUE)

  expected_subjids <- as.integer(as.character(smc$subjid))
  expected_n_subjs <- length(unique(expected_subjids))
  testthat::expect_equal(expected_n_subjs, expected_subjids,
                         label = "same number of subjects")
})

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

# test cases from bugs and feedbacks -------------------------------------------

test_that("growth trajectory is the same for the old SMOCC data", {
  # smocc_hbgd_data <- bstick_smocc_to_hbgd(brokenstick::smocc.hgtwgt)

  smocc_hbgd_data <- hbgd::get_smocc_data()

  wandfit <- hbgd::get_fit(smocc_hbgd_data, y_var = "haz", method = "wand")
  smc_tr <- hbgd::fit_all_trajectories(
    smocc_hbgd_data, wandfit, xg = seq(0,366, length = 367))

  avg_velocity <- get_traj_velocity(smc_tr, z_score = TRUE)

  # What we expect the first ten average velocities to be.
  # Obtained using the older version of `hbgd` that used `datadr`
  expected_velocities <- c(-0.0027878588, -0.0011913612,
                           -0.0018604061, -0.0012806619,
                            0.0025456199, -0.0005153804,
                           -0.0006043548, -0.0018132654,
                           -0.0038489583, -0.0008184987)

  # Using inclusive, `agedays_limit` - new behaviour.
  expected_velocities_lte <- c(-0.0027821509, -0.0011898129,
                               -0.0018618052, -0.0012824167,
                               0.0025471120,  -0.0005118098,
                               -0.0006063080, -0.0018079078,
                               -0.0038443618, -0.0008151201)

  # Extracts the first 10.
  actual_velocities <- avg_velocity[c(1:10), ]$avgder

  testthat::expect_equal(actual_velocities,
                         expected_velocities_lte,
                         tolerance = 5e-2,
                         label = "comparing first ten velocities.")
})

test_that("the range of growth trajectories are extracted correctly", {

  AGEDAYS_FLOOR <- 10.00
  AGEDAYS_LIMIT <- 30.00

  TEST_SUBJID_SMOCC <- 10001L

  smocc_hbgd_data <- hbgd::get_smocc_data()

  wandfit <- hbgd::get_fit(smocc_hbgd_data, y_var = "haz", method = "wand")
  smc_tr <- hbgd::fit_all_trajectories(
    smocc_hbgd_data, wandfit, xg = seq(0,366, length = 367))

  test_subj_fitgrid <- smc_tr[
    smc_tr$subjid == TEST_SUBJID_SMOCC, ]$fit[[1]]$fitgrid
  filtered_fitgrid <- test_subj_fitgrid[
    test_subj_fitgrid$x >= AGEDAYS_FLOOR & test_subj_fitgrid$x <= AGEDAYS_LIMIT,
    c("x", "dy", "dz")
  ]

  expected_num_rows <- 4L
  testthat::expect_equal(nrow(filtered_fitgrid), expected_num_rows,
                         label = "checking number of rows of data within agedays range.")


  expected_dz_mean <- mean(filtered_fitgrid$dz)
  avg_velocity_z <- get_traj_velocity(smc_tr, z_score = TRUE,
                                      agedays_floor = AGEDAYS_FLOOR,
                                      agedays_limit = AGEDAYS_LIMIT)
  actual_dz_mean <- as.numeric(
    avg_velocity_z[avg_velocity_z$subjid == TEST_SUBJID_SMOCC, "avgder"])
  testthat::expect_equal(actual_dz_mean, expected_dz_mean,
                         label = "comparing mean( dz ) within agedays range.")


  expected_dy_mean <- mean(filtered_fitgrid$dy)
  avg_velocity <- get_traj_velocity(smc_tr, z_score = FALSE,
                                    agedays_floor = AGEDAYS_FLOOR,
                                    agedays_limit = AGEDAYS_LIMIT)
  actual_dy_mean <- as.numeric(
    avg_velocity[avg_velocity$subjid == TEST_SUBJID_SMOCC, "avgder"])

  testthat::expect_equal(actual_dy_mean, expected_dy_mean,
                         label = "comparing mean( dy ) within agedays range.")
})
