% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_avg_vel.R
\name{get_traj_velocity}
\alias{get_traj_velocity}
\title{Returns the veclocity of growth trajectories for _all_ subjects}
\usage{
get_traj_velocity(all_traj, z_score = TRUE, agedays_floor = 0,
  agedays_limit = 365, FUN = mean_na_rm_generator())
}
\arguments{
\item{all_traj}{Trajectory object returned from \code{\link{fit_all_trajectories}}.
Assumes all required data are present.}

\item{z_score}{Boolean indicating whether 'y_var' parameter was for
z-score data or not. Default is TRUE.}

\item{agedays_floor}{Numeric, indicating the _floor_ (or start of range) of
the number of `agedays` (inclusive) associated with the derivative of the
trajectories, to which the function in `FUN` argument should apply to.
Default is 0.
Note, this parameter depends on the
the number of grid points used to generate the trajectories, which
is specified by the \code{xg} parameter for the functions
\code{\link{hbgd::fit_trajectory}} and
\code{\link{hbgd::fit_all_trajectories}}.}

\item{agedays_limit}{Numeric, indicating the _ceiling_ (or end of range) of
the number of `agedays` (inclusive) associated with the derivative of the
trajectories, to which the function in `FUN` argument should apply to.
Default is 365.
Note, this parameter depends on the
the number of grid points used to generate the trajectories, which
is specified by the \code{xg} parameter for the functions
\code{\link{hbgd::fit_trajectory}} and
\code{\link{hbgd::fit_all_trajectories}}.}

\item{FUN}{A function that will be applied to the first derivative of
the fitted growth trajectory. This function should accept a single
argument; a vector containing the values of the first derivative.
Default value is a _{\link[curried]{https://en.wikipedia.org/wiki/Currying}}_
version of the \code{\link{mean}} function, which removes
\code{NA} values by default.}
}
\value{
A \code{\link[tibble]{tibble}} with two columns, \code{subjid}
        and \code{avg_vel}
}
\description{
Returns the veclocity of growth trajectories for _all_ subjects
}
