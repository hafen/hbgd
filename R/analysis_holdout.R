#' Add indicator column for per-subject holdout
#'
#' @param dat data
#' @param random if TRUE, a random observation per subject will be designated as the holdout, if FALSE, the endpoint for each subject will be designated as the holdout
#' @export
add_holdout_ind <- function(dat, random = TRUE) {

  sq <- 1:nrow(dat)

  samplerandom <- function(x)
    ifelse(length(x) == 1, 0, sample(x, 1))

  samplemax <- function(x)
    ifelse(length(x) == 1, 0, max(x))

  if (random) {
    validation_set <- tapply(X = sq, INDEX = dat$subjid, FUN = samplerandom)
  } else {
    validation_set <- tapply(X = sq, INDEX = dat$subjid, FUN = samplemax)
  }

  dat$hold <- sq %in% validation_set

  dat
}

#' Get holdout errors
#'
#' @param d an object returned from \code{\link{fit_all_trajectories}}
#' @param z compute MSE on z-score scale or original scale?
#' @export
get_fit_holdout_errors <- function(d, z = TRUE) {
  if (!inherits(d, "ddo"))
    stop("Input must be a distributed data object.")
  if (!inherits(d[[1]]$value, "fittedTrajectory"))
    stop("Input must be the result of fit_all_trajectories().")
  if (is.null(d[[1]]$value$holdout))
    stop("This input was not fit with a holdout - cannot compute MSE.")

  trns <- function(x) {
    if (nrow(x$holdout) > 0) {
      if (z) {
        return(x$holdout$z - x$xy$zfit[x$xy$hold])
      } else {
        return(x$holdout$y - x$xy$yfit[x$xy$hold])
      }
    } else {
      return(NULL)
    }
  }

  d %>% datadr::addTransform(trns) %>% datadr::recombine(datadr::combRbind)
}


#' Get MSE for holdout
#'
#' @param d an object returned from \code{\link{fit_all_trajectories}}
#' @param z compute MSE on z-score scale or original scale?
#' @export
get_fit_holdout_mse <- function(d, z = TRUE) {
  a <- get_fit_holdout_errors(d, z)
  mean(a$val ^ 2)
}
