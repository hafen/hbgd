#' Apply a model fit to an individual's trajectory
#'
#' @param dat data frame containing variables for one subject to apply a fit to
#' @param fit an object returned from \code{\link{get_fit}}
#' @param xg grid of x points at which the fit should be evaluated for plotting (if \code{NULL} it will be set to an equally-spaced grid of 150 points across \code{x})
#' @param checkpoints x values at which to compute "checkpoints" of the subjects's growth trajectory to compare to other subjects
#' @param z_bins a vector indicating binning of z-scores for the subject's trajectory at each checkpoint with respect to the the WHO growth standard
#' @examples
#' mod <- get_fit(cpp, y_var = "wtkg")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
#' plot(fit$xy$x, fit$xy$y)
#' lines(fit$fitgrid$x, fit$fitgrid$y)
#' # there is also a plot method:
#' plot(fit, x_range = c(0, 2560))
#'
#' # we can fit the z-scores instead
#' mod2 <- get_fit(cpp, y_var = "waz")
#' fit2 <- fit_trajectory(subset(cpp, subjid == 2), mod2)
#' plot(fit2$xy$x, fit2$xy$z)
#' lines(fit2$fitgrid$x, fit2$fitgrid$z)
#' # using the plot method
#' plot(fit2, x_range = c(0, 2560), center = TRUE)
#' @export
fit_trajectory <- function(dat, fit,
  xg = NULL,
  checkpoints = 365 * c(1:2),
  z_bins = -2
) {

  if (!is_by_subject(dat))
    dat <- by_subject(dat)

  if (nrow(dat) > 1)
    stop("Must only supply one subject's data to fit_trajectory().")

  y_var   <- fit$y_var
  x_var   <- fit$x_var
  method  <- fit$method
  x_trans <- fit$x_trans
  x_inv   <- fit$x_inv
  y_trans <- fit$y_trans
  y_inv   <- fit$y_inv
  holdout <- fit$holdout

  pair <- paste(y_var, x_var, sep = "_")
  # check_pair(pair)

  y_var_out <- y_var
  if (y_var == "haz")
    y_var_out <- "htcm"

  if (y_var == "waz")
    y_var_out <- "wtkg"

  # if (y_var == "hcaz")
  #   y_var_out <- "hcircm"

  # if (y_var == "baz")
  #   y_var_out <- "bmi"

  # if (y_var == "muaz")
  #   y_var_out <- "muaccm"

  # if (y_var == "ssftaz")
  #   y_var_out <- "ssftmm"

  sex <- dat$sex
  longi <- dat$longi[[1]]
  keep_idx <- !is.na(longi[[y_var]])
  dat2 <- longi[keep_idx, , drop = FALSE] # nolint

  ## get x and y
  x <- dat2[[x_var]]
  y <- dat2[[y_var]]

  xt <- x_trans(x)
  yt <- y_trans(y)

  if (length(x) == 0) {
    return(NULL)
  } else {
    ## set up xgrid for fit plotting
    xrng <- range(x, na.rm = TRUE)
    if (is.null(xg))
      xg <- seq(xrng[1], xrng[2], length = 150)
    xgt <- x_trans(xg)

    cpx <- checkpoints
    if (!is.null(cpx))
      cpxt <- x_trans(cpx)
    # if (is.null(dat$subjid[1]))
    #   browser()

    ## fit model
    dd <- data.frame(x = xt, y = yt, subjid = dat$subjid)
    if (holdout)
      dd$y[dat2$hold] <- NA
    res <- fit$fit$fit_apply(dd, xg = xgt, cpx = cpxt, fit = fit$fit)
  }

  # if none of the approaches worked, populate an empty object
  if (is.null(res)) {
    res <- list(
      xy = data.frame(x = dat2[[x_var]], y = dat2[[y_var]],
        idx = which(keep_idx)),
      fit = NULL,
      fitgrid = NULL,
      fitgrid = NULL,
      checkpoint = data.frame(x = checkpoints, y = NA, z = NA, zcat = NA),
      pars = NULL
    )
  } else {
    res$xy <- data.frame(x = dat2[[x_var]], y = dat2[[y_var]],
      idx = which(keep_idx))
    ## untransform things
    res$xy$x <- x_inv(res$xy$x)
    res$xy$y <- x_inv(res$xy$y)
    res$xy$yfit <- y_inv(res$fit)
    res$resid <- res$xy$y - y_inv(res$fit)
  }

  if (!is.null(res$fitgrid)) {
    res$fitgrid$x <- x_inv(res$fitgrid$x)
    res$fitgrid$y <- x_inv(res$fitgrid$y)
  }
  if (!all(is.na(res$checkpoint$y))) {
    res$checkpoint$x <- x_inv(res$checkpoint$x)
    res$checkpoint$y <- x_inv(res$checkpoint$y)

    # make checkpoints outside range of x_var NA
    idx <- res$checkpoint$x < min(res$xy$x) | res$checkpoint$x > max(res$xy$x)
    res$checkpoint$y[idx] <- NA
  }

  if (holdout) {
    res$holdout <- data.frame(x = x[dat2$hold], y = y[dat2$hold])
    res$xy$hold <- dat2$hold
  }

  ## if it is a z-score, inverse transform
  if (y_var %in% c("haz", "waz") && x_var == "agedays") {
    yy_var <- ifelse(y_var == "haz", "htcm", "wtkg")

    res$xy$z <- res$xy$y
    res$xy$zfit <- res$xy$yfit

    if (nrow(res$xy) > 0)
      res$xy$y <- growthstandards::who_zscore2value(res$xy$x, fix_big_z(res$xy$z),
        x_var = x_var, y_var = yy_var, sex = sex)

    if (length(res$xy$zfit) > 0)
      res$xy$yfit <- growthstandards::who_zscore2value(res$xy$x, fix_big_z(res$xy$zfit),
        x_var = x_var, y_var = yy_var, sex = sex)

    if (!is.null(res$fitgrid)) {
      res$fitgrid$z <- res$fitgrid$y
      res$fitgrid$y <- growthstandards::who_zscore2value(res$fitgrid$x,
        fix_big_z(res$fitgrid$z), x_var = x_var, y_var = yy_var, sex)
    }

    if (!all(is.na(res$checkpoint$y))) {
      res$checkpoint$z <- res$checkpoint$y
      res$checkpoint$y <- growthstandards::who_zscore2value(res$checkpoint$x,
        res$checkpoint$y, x_var = x_var, y_var = yy_var, sex)

      cpz <- res$checkpoint$z
      cpzc <- cut(cpz, c(-Inf, z_bins, Inf))
      levels(cpzc)[1] <- paste0("<", z_bins[1])
      levels(cpzc)[1 + length(z_bins)] <- paste0(">", tail(z_bins, 1))
      cpzc <- as.character(cpzc)
      res$checkpoint$z <- cpz
      res$checkpoint$zcat <- cpzc
    }
    if (!is.null(res$holdout) && nrow(res$holdout) > 0) {
      res$holdout$z <- res$holdout$y
      res$holdout$y <- growthstandards::who_zscore2value(res$holdout$x,
        res$holdout$y, x_var = x_var, y_var = yy_var, sex)
    }
  } else if (pair %in% names(growthstandards::who_coefs)) {
    ## if x_var and y_var are available in WHO
    ## add z to fitgrid and checkpoint

    if (nrow(res$xy) > 0)
      res$xy$z <- growthstandards::who_value2zscore(res$xy$x, res$xy$y,
        x_var, y_var, sex)

    if (length(res$xy$yfit) > 0)
      res$xy$zfit <- growthstandards::who_value2zscore(res$xy$x, res$xy$yfit,
        x_var, y_var, sex)

    if (!is.null(res$fitgrid)) {
      res$fitgrid$z <- growthstandards::who_value2zscore(res$fitgrid$x,
        res$fitgrid$y, x_var, y_var, sex)
    }

    # add z-score and category to checkpoints
    if (!all(is.na(res$checkpoint$y))) {
      # "checkpoints" at which to check where trajectory lies wrt z score

      res$checkpoint$z <- growthstandards::who_value2zscore(res$checkpoint$x,
        res$checkpoint$y, x_var, y_var, sex)

      cpz <- growthstandards::who_value2zscore(res$checkpoint$x, res$checkpoint$y,
        x_var, y_var, sex)
      # categorize z-scores according to z_bins
      cpzc <- cut(cpz, c(-Inf, z_bins, Inf))
      levels(cpzc)[1] <- paste0("<", z_bins[1])
      levels(cpzc)[1 + length(z_bins)] <- paste0(">", tail(z_bins, 1))
      cpzc <- as.character(cpzc)

      res$checkpoint$z <- cpz
      res$checkpoint$zcat <- cpzc
    }

    if (!is.null(res$holdout) && nrow(res$holdout) > 0) {
      res$holdout$z <- growthstandards::who_value2zscore(res$holdout$x,
        res$holdout$y, x_var, y_var, sex)
    }
  }

  ## add derivative on original and z-score scale
  if (!is.null(res$fitgrid$y))
    res$fitgrid$dy <- grid_deriv(res$fitgrid$x, res$fitgrid$y)
  if (!is.null(res$fitgrid$z))
    res$fitgrid$dz <- grid_deriv(res$fitgrid$x, res$fitgrid$z)

  # res$data <- dat # keep track of all data for this subject
  res$sex <- sex
  res$x_var <- x_var
  res$y_var <- y_var_out
  res$method <- method

  class(res) <- "fittedTrajectory"
  res
}

#' Apply trajectory fitting to each subject in a dataset
#'
#' @param dat a data frame containing data for several subjects or a data frame with one row per subject, as obtained from \code{\link{by_subject}}
#' @param fit an object returned from \code{\link{get_fit}}
#' @param xg grid of x points at which the fit should be evaluated for plotting (if \code{NULL} it will be set to an equally-spaced grid of 150 points across \code{x})
#' @param checkpoints x values at which to compute "checkpoints" of the subjects's growth trajectory to compare to other subjects
#' @param z_bins a vector indicating binning of z-scores for the subject's trajectory at each checkpoint with respect to the the WHO growth standard
#' @examples
#' \dontrun{
#' cppfit <- get_fit(cpp, y_var = "wtkg", method = "rlm")
#' cpptr  <- fit_all_trajectories(cpp, cppfit)
#' cpptr$fit[[1]]
#' plot(cpptr$fit[[1]])
#' }
#' @export
#' @importFrom progress progress_bar
#' @importFrom purrrlyr by_row
fit_all_trajectories <- function(dat, fit,
  xg = NULL,
  checkpoints = 365 * c(1:2),
  z_bins = -2
) {
  if (!is_by_subject(dat))
    dat <- by_subject(dat)
  check_by_subject(dat)

  # only keep subjects that have at least one record of non-NA
  nrec <- purrr::map_int(dat$longi, function(x)
    length(which((complete.cases(x[, c(fit$x_var, fit$y_var)]))))) # nolint
  dat <- dat[nrec > 0, ]

  pb <- progress::progress_bar$new(
    format = "fitting trajectories [:bar] :percent :current/:total eta::eta",
    total = nrow(dat), width = getOption("width") - 4)

  res <- purrrlyr::by_row(dat, function(x) {
    pb$tick()
    fit_trajectory(x, fit, xg = xg, checkpoints = checkpoints, z_bins = z_bins)
  }, .to = "fit") # nolint
  attr(res, "hbgd") <- attr(dat, "hbgd")
  res
}
