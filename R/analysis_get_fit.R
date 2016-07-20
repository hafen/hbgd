#' Obtain a trajectory "fit" object for a dataset
#'
#' @param dat data frame containing variables to model
#' @param x_var name of x variable to model (default is "agedays")
#' @param y_var name of y variable to model (usually an anthropometric measure or z-score scaled anthropometric measure)
#' @param method name of fitting method to use (see \code{\link{get_avail_methods}})
#' @param holdout should an observation be held out for fitting (will use column \code{hold} in \code{dat} to which observations to hold out)
#' @param x_trans,y_trans transformation functions to be applied to x and y prior to modeling
#' @param x_inv,y_inv inverse transformation functions for x and y to get back to the original scale after modeling
#' @param \ldots parameters passed on to the fitting method
#' @export
get_fit <- function(
  dat,
  x_var = "agedays", y_var = "htcm",
  method = "fda",
  holdout = FALSE,
  x_trans = NULL, x_inv = NULL,
  y_trans = NULL, y_inv = NULL, ...) {

  ## handle transformations

  # default_trans <- log10_1
  # default_inv <- exp10_1
  # if (y_var %in% c("haz", "waz") && x_var == "agedays") {
  #   default_trans <- identity
  #   default_inv <- identity
  # }

  if (holdout) {
    if (is.null(dat$hold))
      stop("'holdout' is TRUE but there is not a column 'hold' in the input data.
Please first use add_holdout_ind() to the input data to create this column.")

    dat <- subset(dat, !dat$hold)
  }

  default_trans <- identity
  default_inv <- identity

  if (is.null(x_trans))
    x_trans <- default_trans
  if (is.null(y_trans))
    y_trans <- default_trans
  if (is.null(x_inv))
    x_inv <- default_inv
  if (is.null(y_inv))
    y_inv <- default_inv

  method <- match.arg(method, get_avail_methods())

  ## fit model
  # sex <- dat$sex[1]
  keep_idx <- !is.na(dat[[y_var]])
  dat2 <- dat[keep_idx, , drop = FALSE] # nolint

  ## get x and y
  x <- dat2[[x_var]]
  y <- dat2[[y_var]]
  xt <- x_trans(x)
  yt <- y_trans(y)
  dd <- data.frame(x = xt, y = yt, subjid = dat2$subjid)
  class(dd) <- c("data.frame", method)
  fit <- fit_method(dd, ...)
  fit$holdout <- holdout

  structure(list(
    x_var = x_var,
    y_var = y_var,
    method = method,
    x_trans = x_trans, x_inv = x_inv,
    y_trans = y_trans, y_inv = y_inv,
    fit = fit,
    holdout = holdout
  ), class = c("fitObj", "list"))
}

print.fitObj <- function(x, ...) {
  res <- strwrap(c(
    paste0("Object obtained from get_fit() using method '", x$method, "'."),
    "Use str() to inspect or fit_trajectory() or fit_all_trajectories() to obtain fitted values for subjects.", "")) # nolint

  cat(paste(res, collapse = "\n"))
}
