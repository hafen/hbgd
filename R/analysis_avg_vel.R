#' Returns average veclocity for _all_ subjects
#'
#' @param all_traj Trajectory object returned from \code{\link{fit_all_trajectories}}.
#'        Assumes all required data are present.
#' @param z_score Boolean indicating whether 'y_var' parameter was for
#'        z-score data or not. Default is TRUE.
#' @param avg_over_days Number of days to averge the derivative (aka velocity)
#'        Default is 365.
#' @return A \code{\link[tibble]{tibble}} with two columns, \code{subjid}
#'         and \code{avg_vel}
#'
#' @export
avg_velocity <- function(all_traj, z_score = TRUE, avg_over_days = 365L) {

  # message(">>> Generating means of trajectories of a model…\n")
  subjids <- as.numeric(as.character(all_traj$subjid))

  avg_vel <- NULL
  if (z_score) {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      mean(current_fitgrid[current_fitgrid$x <= avg_over_days, "dz"], na.rm = TRUE)
    }))
  } else {
    avg_vel <- unlist(lapply(subjids, function(subjid) {
      current_fitgrid <- all_traj[all_traj$subjid == subjid, ]$fit[[1]]$fitgrid
      mean(current_fitgrid[current_fitgrid$x <= avg_over_days, "dy"], na.rm = TRUE)
    }))
  }

  # message(sprintf(">>> Formattting the results…\n"))
  avg_vel_tibble <- tibble::tibble(
    subjid = subjids,
    avgder = avg_vel
  )
  return(avg_vel_tibble)
}
