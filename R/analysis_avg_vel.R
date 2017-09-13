#' _{\link[Curried]{https://en.wikipedia.org/wiki/Currying}}_ version
#' of \code{\link{mean}} function that removes \code{NA} values.
#'
#' @return Function that accepts a single argument, which is the value(s) to
#'         average over.
mean_na_rm_generator <- function() {
  return(function(x) {
    mean(x, na.rm = TRUE)
  })
}

#' Returns the veclocity of growth trajectories for _all_ subjects
#'
#' @param all_traj Trajectory object returned from \code{\link{fit_all_trajectories}}.
#'        Assumes all required data are present.
#' @param z_score Boolean indicating whether 'y_var' parameter was for
#'        z-score data or not. Default is TRUE.
#' @param agedays_floor Numeric, indicating the _floor_ (or start of range) of
#'        the number of `agedays` (inclusive) associated with the derivative of the
#'        trajectories, to which the function in `FUN` argument should apply to.
#'        Default is 0.
#'        Note, this parameter depends on the
#'        the number of grid points used to generate the trajectories, which
#'        is specified by the \code{xg} parameter for the functions
#'        \code{\link{hbgd::fit_trajectory}} and
#'        \code{\link{hbgd::fit_all_trajectories}}.
#' @param agedays_limit Numeric, indicating the _ceiling_ (or end of range) of
#'        the number of `agedays` (inclusive) associated with the derivative of the
#'        trajectories, to which the function in `FUN` argument should apply to.
#'        Default is 365.
#'        Note, this parameter depends on the
#'        the number of grid points used to generate the trajectories, which
#'        is specified by the \code{xg} parameter for the functions
#'        \code{\link{hbgd::fit_trajectory}} and
#'        \code{\link{hbgd::fit_all_trajectories}}.
#' @param FUN A function that will be applied to the first derivative of
#'        the fitted growth trajectory. This function should accept a single
#'        argument; a vector containing the values of the first derivative.
#'        Default value is a _{\link[curried]{https://en.wikipedia.org/wiki/Currying}}_
#'        version of the \code{\link{mean}} function, which removes
#'        \code{NA} values by default.
#' @return A \code{\link[tibble]{tibble}} with two columns, \code{subjid}
#'         and \code{avg_vel}
#' @export
get_traj_velocity <- function(all_traj, z_score = TRUE,
                              agedays_floor = 0.00,
                              agedays_limit = 365.00,
                              FUN = mean_na_rm_generator()) {

  # message(">>> Generating means of trajectories of a model…\n")
  subjids <- as.numeric(as.character(all_traj$subjid))

  avg_vel <- NULL
  if (z_score) {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      FUN(current_fitgrid[
        current_fitgrid$x >= agedays_floor & current_fitgrid$x <= agedays_limit,
        "dz"])
    }))
  } else {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      FUN(current_fitgrid[
        current_fitgrid$x >= agedays_floor & current_fitgrid$x <= agedays_limit,
        "dy"])
    }))
  }

  # message(sprintf(">>> Formattting the results…\n"))
  avg_vel_tibble <- tibble::tibble(
    subjid = subjids,
    avgder = avg_vel
  )
  return(avg_vel_tibble)
}
