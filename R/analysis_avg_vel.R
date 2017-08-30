#' _{\link[Curried]{https://en.wikipedia.org/wiki/Currying}}_ version
#' of \code{\link{mean}} function that removes \code{NA} values.
#'
#' @param
#' @return A function that accepts a single argument, which is the value(s) to
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
#' @param avg_over_days Number of days to averge the derivative (aka velocity)
#'        Default is 365.
#' @param FUN A function that will be applied to the first derivative of
#'        the fitted growth trajectory. This function should accept a single
#'        argument; a vector containing the values of the first derivative.
#'        Default value is a _{\link[curried]{https://en.wikipedia.org/wiki/Currying}}_
#'        version of the \code{\link{mean}} function, which removes
#'        \code{NA} values by default.
#' @return A \code{\link[tibble]{tibble}} with two columns, \code{subjid}
#'         and \code{avg_vel}
#' @export
get_traj_velocity <- function(all_traj, z_score = TRUE, avg_over_days = 365L, FUN = mean_na_rm_generator()) {

  # message(">>> Generating means of trajectories of a model…\n")
  subjids <- as.numeric(as.character(all_traj$subjid))

  avg_vel <- NULL
  if (z_score) {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      FUN(current_fitgrid[current_fitgrid$x <= avg_over_days, "dz"])
    }))
  } else {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      FUN(current_fitgrid[current_fitgrid$x <= avg_over_days, "dy"])
    }))
  }

  # message(sprintf(">>> Formattting the results…\n"))
  avg_vel_tibble <- tibble::tibble(
    subjid = subjids,
    avgder = avg_vel
  )
  return(avg_vel_tibble)
}
