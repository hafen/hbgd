#' Plot a fitted trajectory
#'
#' @param x an object returned from \code{\link{fit_trajectory}}
#' @param center should the trajectory be centered around the median WHO standard?  This is equivalent to plotting the age difference score (like height-for-age difference - HAD)
#' @param x_range a vector specifying the range (min, max) that the superposed growth standard should span on the x-axis
#' @param width width of the plot
#' @param height height of the plot
#' @param hover variable names in \code{x$data} to show on hover for each point (only variables with non-NA data will be shown)
#' @param checkpoints should the checkpoints be plotted (if available)?
#' @param p centiles at which to draw the WHO polygons
#' @param x_units units of age x-axis (days, months, or years)
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @examples
#' mod <- get_fit(cpp, y_var = "wtkg", method = "rlm")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
#' plot(fit)
#' plot(fit, x_units = "years")
#' plot(fit, center = TRUE)
#' plot(fit, hover = c("wtkg", "bmi", "waz", "haz"))
#' @export
plot.fittedTrajectory <- function(x, center = FALSE, x_range = NULL,
  width = 500, height = 520, hover = NULL, checkpoints = TRUE,
  p = 100 * pnorm(-3:0),
  x_units = c("days", "months", "years"), ...) {

  x_units <- match.arg(x_units)
  x_denom <- switch(x_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  if (nrow(x$xy) == 0)
    return(empty_plot(paste0("No '", x$y_var, "' vs. '", x$x_var, "' data for this subject")))

  if (is.null(x_range)) {
    x_range <- range(x$xy$x, na.rm = TRUE)
    x_range <- x_range + c(-1, 1) * diff(x_range) * 0.07
  }

  # if (missing(hover)) {
  #   hover <- names(x$data)[sapply(x$data, function(x) !all(is.na(x)))]
  #   hover <- x$data[x$xy$idx, hover]
  # } else
  if (is.null(hover))
    hover <- c(x$x_var, x$y_var)

  hover <- intersect(names(x$data), hover)
  if (length(hover) == 0) {
    hover <- NULL
  } else {
    hover <- hover[sapply(x$data[, hover], function(x) !all(is.na(x)))]
    hover <- x$data[x$xy$idx, hover]
  }

  ylab <- hbgd::hbgd_labels[[x$y_var]]

  if (center) {
    for (el in c("xy", "fitgrid", "checkpoint", "holdout")) {
      if (!is.null(x[[el]]))
        x[[el]]$y <- x[[el]]$y - who_centile2value(x[[el]]$x, p = 50,
          x_var = x$x_var, y_var = x$y_var, sex = x$sex)
        if (!is.null(x[[el]]$yfit))
          x[[el]]$yfit <- x[[el]]$yfit - who_centile2value(x[[el]]$x, p = 50,
            x_var = x$x_var, y_var = x$y_var, sex = x$sex)
    }

    ylab <- paste(ylab, "(WHO median-centered)")
  }

  xlab <- hbgd::hbgd_labels[[x$x_var]]
  if (x_units == "months")
    xlab <- gsub("\\(days\\)", "(months)", xlab)
  if (x_units == "years")
    xlab <- gsub("\\(days\\)", "(years)", xlab)

  fig <- figure(width = width, height = height,
    xlab = xlab, ylab = ylab, logo = NULL, ...) %>%
    ly_who(x = seq(x_range[1], x_range[2], length = 100), center = center,
      x_var = x$x_var, y_var = x$y_var, sex = x$sex, p = p, x_units = x_units) %>%
    rbokeh::ly_points(x / x_denom, y, hover = hover, data = x$xy, color = "black")
  if (!is.null(x$fitgrid)) {
    fig <- fig %>%
      rbokeh::ly_lines(x / x_denom, y, data = x$fitgrid, color = "black") %>%
      rbokeh::ly_points(x / x_denom, yfit, data = x$xy, color = "black", glyph = 19, size = 4)
  }
  if (!is.null(x$holdout))
    fig <- fig %>%
      rbokeh::ly_points(x / x_denom, y, data = x$holdout, color = "red")

  if (!all(is.na(x$checkpoint$y)) && checkpoints) {
    x$checkpoint <- subset(x$checkpoint, !is.na(y))
    x$checkpoint <- data.frame(lapply(x$checkpoint, unname))
    x$checkpoint$zcat <- as.character(x$checkpoint$zcat)

    fig <- fig %>%
      rbokeh::ly_points(x / x_denom, y, size = 15, hover = c("zcat", "x"),
        data = x$checkpoint, glyph = 13, color = "black", alpha = 0.6)
  }

  fig
}

#' Plot a fitted trajectory on z-score scale
#'
#' @param x an object returned from \code{\link{fit_trajectory}}
#' @param x_range a vector specifying the range (min, max) that the superposed z-score bands should span on the x-axis
#' @param nadir should a guide be added to the plot showing the location of the nadir?
#' @param recovery age in days at which to plot recovery from nadir (only valid if nadir is TRUE) - if NULL (default), will not be plotted
#' @param width width of the plot
#' @param height height of the plot
#' @param hover variable names in \code{x$data} to show on hover for each point (only variables with non-NA data will be shown)
#' @param checkpoints should the checkpoints be plotted (if available)?
#' @param z z-scores at which to draw the z-score bands
#' @param x_units units of age x-axis (days, months, or years)
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @examples
#' mod <- get_fit(cpp, y_var = "wtkg", method = "rlm")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
#' plot_z(fit)
#' @export
plot_z <- function(x, x_range = NULL, nadir = FALSE, recovery = NULL,
  width = 500, height = 520,
  hover = NULL, checkpoints = TRUE, z = -3:0,
  x_units = c("days", "months", "years"), ...) {

  x_units <- match.arg(x_units)
  x_denom <- switch(x_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  if (is.null(x$xy$z))
    return(empty_plot("No z transformation data for this subject"))

  if (is.null(x_range)) {
    x_range <- range(x$xy$x, na.rm = TRUE)
    x_range <- x_range + c(-1, 1) * diff(x_range) * 0.07
  }

  if (is.null(hover)) {
    y_var_out <- x$y_var
    if (x$y_var == "htcm")
      y_var_out <- "haz"
    if (x$y_var == "wtkg")
      y_var_out <- "waz"
    hover <- c(x$x_var, y_var_out)
  }
  hover <- intersect(names(x$data), hover)
  if (length(hover) == 0) {
    hover <- NULL
  } else {
    hover <- hover[sapply(x$data[, hover], function(x) !all(is.na(x)))]
    hover <- x$data[x$xy$idx, hover]
  }

  xlab <- hbgd::hbgd_labels[[x$x_var]]
  if (x_units == "months")
    xlab <- gsub("\\(days\\)", "(months)", xlab)
  if (x_units == "years")
    xlab <- gsub("\\(days\\)", "(years)", xlab)
  ylab <- paste(hbgd::hbgd_labels[[x$y_var]], "z-score")

  fig <- figure(width = width, height = height,
    xlab = xlab, ylab = ylab, logo = NULL, ...) %>%
    ly_zband(x = c(x_range[1], x_range[2]), z = z,
      color = ifelse(x$sex == "Male", "blue", "red"), x_units = x_units) %>%
    rbokeh::ly_points(x / x_denom, z, hover = hover, data = x$xy, color = "black")
  if (!is.null(x$fitgrid)) {
    fig <- fig %>%
      rbokeh::ly_lines(x / x_denom, z, data = x$fitgrid, color = "black") %>%
      rbokeh::ly_points(x / x_denom, zfit, data = x$xy, color = "black",
        glyph = 19, size = 4)
  }
  if (!is.null(x$holdout))
    fig <- fig %>%
      rbokeh::ly_points(x / x_denom, z, data = x$holdout, color = "red")

  if (!all(is.na(x$checkpoint$y)) && checkpoints) {
    x$checkpoint <- subset(x$checkpoint, !is.na(y))
    fig <- fig %>%
      rbokeh::ly_points(x / x_denom, z, size = 15, hover = zcat,
        data = x$checkpoint, glyph = 13, color = "black", alpha = 0.6)
  }

  if (nadir) {
    nadir <- get_nadir(x)
    if (!is.na(nadir$at)) {
      fig <- fig %>%
        rbokeh::ly_segments(nadir$at / x_denom, 0, nadir$at / x_denom, nadir$mag,
          line_width = 5, color = "red", alpha = 0.5)

      if (!is.null(recovery)) {
        recov <- get_recovery(x, nadir, recovery)
        if (!is.na(recov$at)) {
          fig <- fig %>%
            rbokeh::ly_segments(nadir$at / x_denom, nadir$mag,
              recov$at / x_denom, nadir$mag,
              width = 5, color = "orange", alpha = 0.5) %>%
            rbokeh::ly_segments(recov$at / x_denom, nadir$mag,
              recov$at / x_denom, recov$z,
              width = 5, color = "green", alpha = 0.5)
        }
      }
    }
  }

  fig
}

#' Plot a fitted trajectory's velocity
#'
#' @param x an object returned from \code{\link{fit_trajectory}}
#' @param width width of the plot
#' @param height height of the plot
#' @param x_units units of age x-axis (days, months, or years)
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @examples
#' mod <- get_fit(cpp, y_var = "wtkg", method = "rlm")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
#' plot_velocity(fit)
#' @export
plot_velocity <- function(x, width = 500, height = 520,
  x_units = c("days", "months", "years"), ...) {

  x_units <- match.arg(x_units)
  x_denom <- switch(x_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  if (is.null(x$fitgrid$dy))
    return(empty_plot("No velocity data for this subject"))

  xlab <- hbgd::hbgd_labels[[x$x_var]]
  if (x_units == "months")
    xlab <- gsub("\\(days\\)", "(months)", xlab)
  if (x_units == "years")
    xlab <- gsub("\\(days\\)", "(years)", xlab)
  ylab <- paste(hbgd::hbgd_labels[[x$y_var]], "growth velocity")

  # remove blip in velocity
  xx <- x$fitgrid$x
  dyy <- x$fitgrid$dy
  ind <- which.min(abs(xx - 365.25 * 2))
  if (abs(365.25 * 2 - xx[ind]) < 2 * diff(xx[1:2])) {
    dyy[max(1, ind - 2):min(length(xx), ind + 2)] <- NA
  }

  figure(width = width, height = height,
    xlab = xlab, ylab = ylab, logo = NULL, ...) %>%
    ly_lines(xx / x_denom, dyy, color = "black")
}

#' Plot a fitted trajectory's z-score velocity
#'
#' @param x an object returned from \code{\link{fit_trajectory}}
#' @param width width of the plot
#' @param height height of the plot
#' @param x_units units of age x-axis (days, months, or years)
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @examples
#' mod <- get_fit(cpp, y_var = "wtkg", method = "rlm")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), mod)
#' plot_zvelocity(fit)
#' @export
plot_zvelocity <- function(x, width = 500, height = 520,
  x_units = c("days", "months", "years"), ...) {

  x_units <- match.arg(x_units)
  x_denom <- switch(x_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  if (is.null(x$fitgrid$dz))
    return(empty_plot("No z-score velocity data for this subject"))

  xlab <- hbgd::hbgd_labels[[x$x_var]]
  if (x_units == "months")
    xlab <- gsub("\\(days\\)", "(months)", xlab)
  if (x_units == "years")
    xlab <- gsub("\\(days\\)", "(years)", xlab)
  ylab <- paste(hbgd::hbgd_labels[[x$y_var]], "z-score growth velocity")

  # remove blip in velocity
  xx <- x$fitgrid$x
  dzz <- x$fitgrid$dz
  ind <- which.min(abs(xx - 365.25 * 2))
  if (abs(365.25 * 2 - xx[ind]) < 2 * diff(xx[1:2])) {
    dzz[max(1, ind - 2):min(length(xx), ind + 2)] <- NA
  }

  figure(width = width, height = height,
    xlab = xlab, ylab = ylab, logo = NULL, ...) %>%
    ly_lines(xx / x_denom, dzz, color = "black")
}

empty_plot <- function(lab) {
  figure(xaxes = FALSE, yaxes = FALSE,
    xgrid = FALSE, ygrid = FALSE, logo = NULL) %>%
    ly_text(0, 0, c("", lab), align = "center")
}

#' Get nadir of z-scale growth trajectory
#'
#' @param obj object created from \code{\link{fit_trajectory}}
#' @export
get_nadir <- function(obj) {
  if (is.null(obj$fitgrid))
    return(data.frame(at = NA, mag = NA, end = NA))
  if (is.null(obj$fitgrid$dz))
    return(data.frame(at = NA, mag = NA, end = NA))

  nn <- nrow(obj$fitgrid) - 1

  # get crossings of zero of dz
  cross <- which(diff(sign(obj$fitgrid$dz)) > 0) + 1
  if (length(cross) == 0) {
    if (all(obj$fitgrid$dz[nn] >= obj$fitgrid$dz, na.rm = TRUE)) {
      return(data.frame(at = obj$fitgrid$x[nn], mag = obj$fitgrid$z[nn], end = TRUE))
    } else {
      return(data.frame(at = NA, mag = NA, end = NA))
    }
  }

  cross <- cross[which.min(obj$fitgrid$z[cross])]
  end <- FALSE
  if (obj$fitgrid$z[nn] < obj$fitgrid$z[cross]) {
    cross <- nn
    end <- TRUE
  }

  data.frame(at = obj$fitgrid$x[cross], mag = obj$fitgrid$z[cross], end = end)
}

#' Get recovery statistics of z-scale growth trajectory
#'
#' @param obj object created from \code{\link{fit_trajectory}}
#' @param nadir object created from \code{\link{get_nadir}} (if NULL, will be automatically generated)
#' @param at age (in days) at which to estimate recovery
#' @export
get_recovery <- function(obj, nadir = NULL, at = 365.25 * 3) {

  if (is.null(obj$fitgrid))
    return(data.frame(at = NA, mag = NA, end = FALSE))
  if (is.null(obj$fitgrid$z))
    return(data.frame(at = NA, mag = NA, end = FALSE))

  if (is.null(nadir)) {
    nadir <- get_nadir(obj)
  }

  if (!is.na(nadir$at) && nadir$at < at) {
    val <- approxfun(obj$fitgrid$x, obj$fitgrid$z)(at)
    return(data.frame(at = at, z = val, recov = val - nadir$mag))
  } else {
    return(data.frame(at = NA, z = NA, recov = NA))
  }
}
