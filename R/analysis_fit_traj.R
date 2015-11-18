#' Fit a model to an individual's trajectory and optionally add checkpoints
#'
#' @param dat data frame containing variables to model
#' @param x_var name of x variable to model
#' @param y_var name of y variable to model
#' @param method one of "gam", "loess", "fda", "rlm"
#' @param xg grid of x points at which the fit should be evaluated for plotting (if \code{NULL} it will be set to an equally-spaced grid of 150 points across \code{x})
#' @param checkpoints x values at which to compute "checkpoints" of the subjects's growth trajectory to compare to other subjects
#' @param z_bins a vector indicating binning of z-scores for the subject's trajectory at each checkpoint with respect to the the WHO growth standard
#' @param x_trans,y_trans transformation functions to be applied to x and y prior to modeling
#' @param x_inv,y_inv inverse transformation functions for x and y to get back to the original scale after modeling
#' @param \ldots parameters passed on to the fitting method
#' @examples
#' fit <- fit_trajectory(subset(cpp, subjid == 2), y_var = "wtkg", method = "rlm")
#' plot(fit$xy$x, fit$xy$y)
#' lines(fit$fitgrid$x, fit$fitgrid$y)
#' # there is also a plot method:
#' plot(fit, who_range = c(0, 2560))
#' # we can fit the z-scores instead
#' fit2 <- fit_trajectory(subset(cpp, subjid == 2), y_var = "waz", method = "rlm")
#' plot(fit2$xy$x, fit2$xy$z)
#' lines(fit2$fitgrid$x, fit2$fitgrid$z)
#' @export
fit_trajectory <- function(dat, x_var = "agedays", y_var = "htcm",
  method = "gam",
  xg = NULL,
  checkpoints = 365 * c(1:2),
  z_bins = c(-2, 2),
  x_trans = NULL, x_inv = NULL,
  y_trans = NULL, y_inv = NULL, ...) {

  pair <- paste(y_var, x_var, sep = "_")
  # check_pair(pair)

  sex <- dat$sex[1]
  dat2 <- dat[!is.na(dat[[y_var]]),, drop = FALSE]

  method <- match.arg(method, c("gam", "loess", "fda", "rlm"))

  ## get x and y
  x <- dat2[[x_var]]
  y <- dat2[[y_var]]

  if(length(x) == 0) {
    res <- list(
      xy = data.frame(x = numeric(0), y = numeric(0)),
      resid = NULL,
      fitgrid = NULL,
      checkpoint = data.frame(x = checkpoints, y = NA, z = NA, zcat = NA),
      pars = NULL,
      sex = sex, x_var = x_var, y_var = y_var, data = dat)
    class(res) <- "fittedTrajectory"
    return(res)
  }

  default_trans <- log10_1
  default_inv <- exp10_1
  if(y_var %in% c("haz", "waz") && x_var == "agedays") {
    default_trans <- identity
    default_inv <- identity
  }

  ## handle transformation
  if(is.null(x_trans))
    x_trans <- default_trans
  if(is.null(y_trans))
    y_trans <- default_trans
  if(is.null(x_inv))
    x_inv <- default_inv
  if(is.null(y_inv))
    y_inv <- default_inv
  xt <- x_trans(x)
  yt <- y_trans(y)

  ## set up xgrid for fit plotting
  xrng <- range(x, na.rm = TRUE)
  if(is.null(xg))
    xg <- seq(xrng[1], xrng[2], length = 150)
  xgt <- x_trans(xg)

  cpx <- checkpoints
  if(!is.null(cpx))
    cpxt <- x_trans(cpx)

  ## fit model
  dd <- data.frame(x = xt, y = yt)
  class(dd) <- c("data.frame", method)
  res <- fit_method(dd, xg = xgt, cpx = cpxt, ...)

  # if none of the approaches worked, populate an empty object
  if(is.null(res)) {
    res <- list(
      xy = data.frame(x = dat2[[x_var]], y = dat2[[y_var]]),
      resid = NULL,
      fitgrid = NULL,
      checkpoint = data.frame(x = checkpoints, y = NA, z = NA, zcat = NA),
      pars = NULL,
      sex = sex, x_var = x_var, y_var = y_var, data = dat)
    class(res) <- "fittedTrajectory"
    return(res)
  }

  ## untransform things
  res$xy$x <- x_inv(res$xy$x)
  res$xy$y <- x_inv(res$xy$y)
  res$resid <- res$xy$y - y_inv(res$fit)
  if(!is.null(res$fitgrid)) {
    res$fitgrid$x <- x_inv(res$fitgrid$x)
    res$fitgrid$y <- x_inv(res$fitgrid$y)
  }
  if(!is.null(res$checkpoint)) {
    res$checkpoint$x <- x_inv(res$checkpoint$x)
    res$checkpoint$y <- x_inv(res$checkpoint$y)

    # make checkpoints outside range of x_var NA
    idx <- res$checkpoint$x < min(res$xy$x) | res$checkpoint$x > max(res$xy$x)
    res$checkpoint$y[idx] <- NA
  }

  ## if it is a z-score, inverse transform
  if(y_var %in% c("haz", "waz") && x_var == "agedays") {
    yy_var <- ifelse(y_var == "haz", "htcm", "wtkg")

    res$xy$z <- res$xy$y
    res$xy$y <- who_zscore2value(res$xy$x, res$xy$z,
      x_var = x_var, y_var = yy_var, sex = sex)
    # residuals?

    if(!is.null(res$fitgrid)) {
      res$fitgrid$z <- res$fitgrid$y
      res$fitgrid$y <- who_zscore2value(res$fitgrid$x,
        res$fitgrid$y, x_var = x_var, y_var = yy_var, sex)
    }
    if(!is.null(res$checkpoint)) {
      res$checkpoint$z <- res$checkpoint$y
      res$checkpoint$y <- who_zscore2value(res$checkpoint$x,
        res$checkpoint$y, x_var = x_var, y_var = yy_var, sex)

      cpz <- res$checkpoint$z
      cpzc <- cut(cpz, c(-Inf, z_bins, Inf))
      levels(cpzc)[1] <- paste0("<", z_bins[1])
      levels(cpzc)[1 + length(z_bins)] <- paste0(">", tail(z_bins, 1))
      cpzc <- as.character(cpzc)
      res$checkpoint$z <- cpz
      res$checkpoint$zcat <- cpzc
    }
  } else if(pair %in% names(hbgd::who_coefs)) {
    ## if x_var and y_var are available in WHO
    ## add z to fitgrid and checkpoint

    res$xy$z <- who_value2zscore(res$xy$x, res$xy$y,
      x_var, y_var, sex)

    if(!is.null(res$fitgrid)) {
      res$fitgrid$z <- who_value2zscore(res$fitgrid$x,
        res$fitgrid$y, x_var, y_var, sex)
    }

    # add z-score and category to checkpoints
    if(!is.null(res$checkpoint)) {
      # "checkpoints" at which to check where trajectory lies wrt z score

      res$checkpoint$z <- who_value2zscore(res$checkpoint$x,
        res$checkpoint$y, x_var, y_var, sex)

      cpz <- who_value2zscore(res$checkpoint$x, res$checkpoint$y,
        x_var, y_var, sex)
      # categorize z-scores according to z_bins
      cpzc <- cut(cpz, c(-Inf, z_bins, Inf))
      levels(cpzc)[1] <- paste0("<", z_bins[1])
      levels(cpzc)[1 + length(z_bins)] <- paste0(">", tail(z_bins, 1))
      cpzc <- as.character(cpzc)

      res$checkpoint$z <- cpz
      res$checkpoint$zcat <- cpzc
    }
  }

  ## add derivative on original and z-score scale
  res$fitgrid$dy <- grid_deriv(res$fitgrid$x, res$fitgrid$y)
  if(!is.null(res$fitgrid$z))
    res$fitgrid$dz <- grid_deriv(res$fitgrid$x, res$fitgrid$z)

  res$data <- dat # keep track of all data for this subject
  res$sex <- sex
  res$x_var <- x_var
  res$y_var <- y_var

  class(res) <- "fittedTrajectory"
  res
}

#' Apply trajectory fitting to each subject in a data set
#'
#' @param dat a data frame containing data for several subjects or a 'ddf' already divided by subject, as obtained from \code{\link{by_subject}}
#' @param subjid variable name in \code{dat} that contains the subject's identifier
#' @param x_var name of x variable to model
#' @param y_var name of y variable to model
#' @param method one of "gam", "loess", "fda", "rlm"
#' @param checkpoints x values at which to compute "checkpoints" of the subjects's growth trajectory to compare to other subjects
#' @param z_bins a vector indicating binning of z-scores for the subject's trajectory at each checkpoint with respect to the the WHO growth standard
#' @param x_trans,y_trans transformation functions to be applied to x and y prior to modeling
#' @param x_inv,y_inv inverse transformation functions for x and y to get back to the original scale after modeling
#' @examples
#' cppt <- fit_all_trajectories(cpp, y_var = "wtkg", method = "rlm")
#' cppt[[1]]
#' plot(cppt[[1]]$value)
#' @export
fit_all_trajectories <- function(dat, subjid = "subjid",
  x_var = "agedays", y_var = "htcm",
  method = "gam",
  checkpoints = 365 * c(1:2),
  z_bins = c(-2, 2),
  x_trans = NULL, x_inv = NULL, y_trans = NULL, y_inv = NULL) {

  if(inherits(dat, "data.frame"))
    dat <- by_subject(dat, subjid = subjid)

  check_subj_split(dat)

  trans_dat <- dat %>% addTransform(function(k, x) {
    fit_trajectory(x, x_var = x_var, y_var = y_var, method = method,
      checkpoints = checkpoints, z_bins = z_bins,
      x_trans = x_trans, x_inv = x_inv, y_trans = y_trans, y_inv = y_inv)
  })

  res <- trans_dat %>% drPersist()
  attr(res, "hbgd") <- attr(dat, "hbgd")
  res
}

#' Estimate derivative given a grid of points
#'
#' @param x x variable (should be a regularly-spaced grid of points)
#' @param y y variable
#' @importFrom numDeriv grad
#' @export
grid_deriv <- function(x, y) {
  idx <- 2:(length(x) - 1)
  ff <- approxfun(x, y)
  c(NA, numDeriv::grad(ff, x[idx]), NA)
}


#' Plot a fitted trajectory
#'
#' @param x an object returned from \code{\link{fit_trajectory}}
#' @param center should the trajectory be centered around the median WHO standard?
#' @param who_range a vector specifying the range (min, max) that the superposed WHO growth standard should span on the x-axis
#' @param width width of the plot
#' @param height height of the plot
#' @param hover variable names in \code{x$data} to show on hover for each point (only variables with non-NA data will be shown)
#' @param checkpoints should the checkpoints be plotted (if available)?
#' @param p centiles at which to draw the WHO polygons
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @examples
#' fit <- fit_trajectory(subset(cpp, subjid == 2), y_var = "wtkg", method = "rlm")
#' plot(fit)
#' plot(fit, center = TRUE)
#' plot(fit, hover = c("wtkg", "bmi", "waz", "haz"))
#' @export
plot.fittedTrajectory <- function(x, center = FALSE, who_range = NULL, width = 500, height = 520, hover = NULL, checkpoints = TRUE, p = 100 * pnorm(-3:0), ...) {

  if(nrow(x$xy) == 0)
    return(empty_plot(paste0("No '", x$y_var, "' vs. '", x$x_var, "' data for this subject")))

  if(is.null(who_range))
    who_range <- range(x$xy$x, na.rm = TRUE)

  ylab <- hbgd::hbgd_labels[[x$y_var]]

  if(!is.null(hover)) {
    hover <- intersect(names(x$data), hover)
    if(length(hover) == 0) {
      hover <- NULL
    } else {
      hover <- hover[sapply(x$data[,hover], function(x) !all(is.na(x)))]
      hover <- x$data[,hover]
    }
  }

  if(center) {
    for(el in c("xy", "fitgrid", "checkpoint"))
      if(!is.null(x[[el]]))
        x[[el]]$y <- x[[el]]$y - who_centile2value(x[[el]]$x, p = 50,
          x_var = x$x_var, y_var = x$y_var, sex = x$sex)

    ylab <- paste(ylab, "(WHO median-centered)")
  }

  fig <- figure(width = width, height = height,
    xlab = hbgd::hbgd_labels[[x$x_var]], ylab = ylab, ...) %>%
    ly_who(x = seq(who_range[1], who_range[2], length = 100), center = center,
      x_var = x$x_var, y_var = x$y_var, sex = x$sex,
      p = p) %>%
    ly_points(x, y, hover = hover,
      data = x$xy, color = "black")
  if(!is.null(x$fitgrid))
    fig <- fig %>%
      ly_lines(x, y, data = x$fitgrid, color = "black")

  if(!all(is.na(x$checkpoint$y)) && checkpoints) {
    x$checkpoint <- subset(x$checkpoint, !is.na(y))
    fig <- fig %>%
      ly_points(x, y, size = 15, hover = zcat, data = x$checkpoint, glyph = 13, color = "black", alpha = 0.6)
  }

  fig
}

empty_plot <- function(lab) {
  figure(xaxes = FALSE, yaxes = FALSE, xgrid = FALSE, ygrid = FALSE) %>%
    ly_text(0, 0, c("", lab), align = "center")
}
