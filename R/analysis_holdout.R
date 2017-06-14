#' Add indicator column for per-subject holdout
#'
#' @param dat data
#' @param random if TRUE, a random observation per subject will be designated as the holdout, if FALSE, the endpoint for each subject will be designated as the holdout
#' @export
add_holdout_ind <- function(dat, random = TRUE) {

  samplerandom <- function(x)
    ifelse(length(x) == 1, 0, sample(x, 1))

  samplemax <- function(x)
    ifelse(length(x) == 1, 0, max(x))

  if ("longi" %in% names(dat)) {
    # the data has been nested so need to add holdout indicator to longi
    dat$longi <- purrr::map(dat$longi, function(x) {
      x$hold <- FALSE
      if (random) {
        idx <- samplerandom(seq_len(nrow(x)))
      } else {
        idx <- samplemax(seq_len(nrow(x)))
      }
      x$hold[idx] <- TRUE
      x
    })
  } else {
    sq <- 1:nrow(dat)
    if (random) {
      validation_set <- tapply(X = sq, INDEX = dat$subjid, FUN = samplerandom)
    } else {
      validation_set <- tapply(X = sq, INDEX = dat$subjid, FUN = samplemax)
    }
    dat$hold <- sq %in% validation_set
  }

  dat
}

#' Get holdout errors
#'
#' @param d an object returned from \code{\link{fit_all_trajectories}}
#' @param z compute MSE on z-score scale or original scale?
#' @export
get_fit_holdout_errors <- function(d, z = TRUE) {

  if (!inherits(d$fit[[1]], "fittedTrajectory"))
    stop("Input must have a column 'fit' containing fitted trajectories from ",
      "fit_all_trajectories().")
  if (is.null(d$fit[[1]]$holdout))
    stop("This input was not fit with a holdout - cannot compute MSE.")

  trns <- function(x) {
    if (nrow(x$holdout) > 0) {
      if (z && !is.null(x$xy$zfit)) {
        return(x$holdout$z - x$xy$zfit[x$xy$hold])
      } else if (!is.null(x$xy$yfit)) {
        return(x$holdout$y - x$xy$yfit[x$xy$hold])
      } else {
        return(NA)
      }
    }
    return(NA)
  }

  purrr::map_dbl(d$fit, trns)
}


#' Get MSE for holdout
#'
#' @param d an object returned from \code{\link{fit_all_trajectories}}
#' @param z compute MSE on z-score scale or original scale?
#' @export
get_fit_holdout_mse <- function(d, z = TRUE) {
  a <- get_fit_holdout_errors(d, z)
  mean(a ^ 2, na.rm = TRUE)
}
