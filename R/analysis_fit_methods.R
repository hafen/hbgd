fit_method <- function(obj, ...)
  UseMethod("fit_method", obj)

#' Get the result of fitting brokenstick to a dataset
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[brokenstick]{brokenstick}}, most notably \code{knots}
# @importFrom face select_knots face.sparse
#' @details This essentially gets an anthropometric dataset into shape for \code{\link[brokenstick]{brokenstick}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' bsfit <- get_fit(cpp, y_var = "haz", method = "brokenstick")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = bsfit)
#' plot(fit)
#' }
#' @export
#' @importFrom stats na.omit
# @importFrom brokenstick brokenstick
fit_method.brokenstick <- function(dat, ...) {

  # brokenstick is still not publicly available...
  if (!requireNamespace("brokenstick", quietly = TRUE))
    stop("Can't apply brokenstick method as the brokenstick package isn't installed.")

  dots <- list(...)

  # unique number of ages after missing data removal
  nx <- length(unique(stats::na.omit(data.frame(x = dat$x, y = dat$y))$x))
  knots <- min(6, nx)

  if (!is.null(dots$knots)) {
    knots <- dots$knots
    dots$knots <- NULL
  }

  mn <- min(dat$x, na.rm = TRUE)
  mx <- max(dat$x, na.rm = TRUE)

  if (!is.null(dots$mn)) {
    mn <- dots$mn
    dots$mn <- NULL
  }

  if (!is.null(dots$mx)) {
    mx <- dots$mx
    dots$mx <- NULL
  }

  if (length(knots) == 1)
    knots <- seq(mn, mx, length = knots)[-knots]

  fit_obj <- brokenstick::brokenstick(
    x = dat$x,
    y = dat$y,
    subjid = dat$subjid,
    knots = knots,
    boundary = c(mn, mx))

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {

    bfit <- predict(fit$fit_obj, dat$y, dat$x, output = "vector")

    ## get xgrid fits
    ##---------------------------------------------------------

    tmpd <- data.frame(
      y = c(dat$y, rep(NA, length(xg))),
      x = c(dat$x, xg))
    yg <- predict(fit$fit_obj, tmpd$y, tmpd$x, output = "vector")
    yg <- tail(yg, length(xg))

    ## get control point fits
    ##---------------------------------------------------------

    cpy <- NULL
    if (!is.null(cpx)) {
      tmpd <- data.frame(
        y = c(dat$y, rep(NA, length(cpx))),
        x = c(dat$x, cpx))
      cpy <- predict(fit$fit_obj, tmpd$y, tmpd$x, output = "vector")
      cpy <- tail(cpy, length(cpx))
    }

    list(
      fit = bfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}


#' Get the result of fitting sitar to a dataset
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[sitar]{sitar}}, most notably \code{df} which defaults to 3
# @importFrom face select_knots face.sparse
#' @details This essentially gets an anthropometric dataset into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' sitfit <- get_fit(cpp, y_var = "haz", method = "sitar")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = sitfit)
#' plot(fit)
#' }
#' @export
#' @importFrom sitar sitar
fit_method.sitar <- function(dat, ...) {

  dots <- list(...)

  df <- 3
  if (!is.null(dots$df)) {
    df <- dots$df
    dots$df <- NULL
  }

  fit_obj <- sitar(x = x, y = y, id = subjid, df = df, data = dat)

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {

    tmpd <- data.frame(x = dat$x, id = dat$subjid)
    sfit <- predict(fit$fit_obj, newdata = tmpd)

    ## get xgrid fits
    ##---------------------------------------------------------

    tmpd <- data.frame(x = xg, id = dat$subjid[1])
    yg <- predict(fit$fit_obj, newdata = tmpd)

    ## get control point fits
    ##---------------------------------------------------------

    cpy <- NULL
    if (!is.null(cpx)) {
      tmpd <- data.frame(x = cpx, id = dat$subjid[1])
      cpy <- predict(fit$fit_obj, newdata = tmpd)
    }

    list(
      fit = sfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Get the result of fitting a Laird and Ware linear or quadratic model to a dataset
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters, most notably \code{deg} which controls the degree of polynomial for the fit (1 for linear and 2 for quadratic)
#' @details This essentially gets an anthropometric dataset into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' lwfit <- get_fit(cpp, y_var = "haz", method = "lwmod", deg = 2)
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = lwfit)
#' plot(fit)
#' }
#' @export
#' @importFrom lme4 lmer
#' @importFrom scales rescale
fit_method.lwmod <- function(dat, ...) {

  dots <- list(...)

  if (is.null(dots$deg))
    dots$deg <- 2
  deg <- dots$deg

  # scale variables
  dots$rng <- range(dat$x)

  dat$x <- scales::rescale(dat$x, from = dots$rng)
  dat$x2 <- dat$x ^ 2

  if (deg == 1) {
    fit_obj <- lme4::lmer(y ~ x + (x | subjid), data = dat)
  } else if (deg == 2) {
    dat$x2 <- dat$x ^ 2
    fit_obj <- lme4::lmer(y ~ x + x2 + (x + x2 | subjid), data = dat)
  } else {
    stop("deg must be 1 or 2 for fitting 'lwmod'")
  }

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {

    dat2 <- dat
    dat2$x <- scales::rescale(dat$x, from = fit$dots$rng)
    dat2$x2 <- dat2$x ^ 2

    lwfit <- unname(predict(fit$fit_obj, newdata = dat2))

    ## get xgrid fits
    ##---------------------------------------------------------

    xg2 <- scales::rescale(xg, from = fit$dots$rng)
    tmpd <- data.frame(x = xg2, x2 = xg2 ^ 2, subjid = dat$subjid[1])
    yg <- unname(predict(fit$fit_obj, newdata = tmpd))

    ## get control point fits
    ##---------------------------------------------------------

    cpy <- NULL
    if (!is.null(cpx)) {
      cpx2 <- scales::rescale(cpx, from = fit$dots$rng)
      tmpd <- data.frame(x = cpx2, x2 = cpx2 ^ 2, subjid = dat$subjid[1])
      cpy <- unname(predict(fit$fit_obj, newdata = tmpd))
    }

    list(
      fit = lwfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Get the result of fitting a "Wand" model to a dataset
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters, most notably \code{deg} which controls the degree of polynomial for the fit (1 for linear and 2 for quadratic)
#' @details This essentially gets an anthropometric dataset into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' wfit <- get_fit(cpp, y_var = "haz", method = "lwmod", deg = 2)
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = wfit)
#' plot(fit)
#' }
#' @export
#' @importFrom lme4 lmer
#' @importFrom scales rescale
fit_method.wand <- function(dat, ...) {

  dots <- list(...)

  if (is.null(dots$pop_k))
    dots$pop_k <- 10
  if (is.null(dots$subj_k))
    dots$subj_k <- 5

  fit_obj <- do.call(wand_fit, c(as.list(dat), dots))

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {

    wfit <- predict(fit$fit_obj, dat)

    ## get xgrid fits
    ##---------------------------------------------------------

    x_range <- range(dat$x, na.rm = TRUE)

    xg_idx <- which(xg <= x_range[2] & xg >= x_range[1])
    yg <- rep(NA, length(xg))
    if (length(xg_idx) > 0) {
      tmpd <- data.frame(x = xg[xg_idx], subjid = dat$subjid[1])
      yg[xg_idx] <- predict(fit$fit_obj, newdata = tmpd)
    }

    ## get control point fits
    ##---------------------------------------------------------

    cpy <- NULL
    if (!is.null(cpx)) {
      cpx_idx <- which(cpx <= x_range[2] & cpx >= x_range[1])
      cpy <- rep(NA, length(cpx))
      if (length(cpx_idx) > 0) {
        tmpd <- data.frame(x = cpx[cpx_idx], subjid = dat$subjid[1])
        cpy[cpx_idx] <- predict(fit$fit_obj, newdata = tmpd)
      }
    }

    list(
      fit = wfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}



#' Get the result of fitting face.sparse to a dataset
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[face]{face.sparse}}, most notably \code{knots} which defaults to 10
# @importFrom face select_knots face.sparse
#' @details This essentially gets an anthropometric dataset into shape for \code{\link[face]{face.sparse}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' facefit <- get_fit(cpp, y_var = "haz", method = "face")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = facefit)
#' plot(fit)
#' }
#' @export
fit_method.face <- function(dat, ...) {

  # latest face not on CRAN...
  if (!requireNamespace("face", quietly = TRUE))
    stop("Can't apply face method as the face package isn't installed.")

  dots <- list(...)
  knots <- 10
  if (!is.null(dots$knots)) {
    knots <- dots$knots
    dots$knots <- NULL
  }

  facedat <- data.frame(
    argvals = dat$x,
    subj = dat$subjid,
    y = dat$y
  )
  facedat <- facedat[complete.cases(facedat), ]

  knots <- face::select.knots(facedat$argvals, knots = knots)

  fit_obj <- suppressMessages(face::face.sparse(facedat, knots = knots))

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    ## get fits at data
    tmpd <- data.frame(
      argvals = c(dat$x, dat$x),
      subj = dat$subjid[1],
      y = c(dat$y, rep(NA, nrow(dat)))
    )

    fpredict <- getFromNamespace("predict.face.sparse", "face")
    aa <- fpredict(fit$fit_obj, tmpd)$y.pred
    dfit <- tail(aa, nrow(dat))

    ## get xgrid fits
    ##---------------------------------------------------------

    tmpd <- data.frame(
      argvals = c(dat$x, xg),
      subj = dat$subjid[1],
      y = c(dat$y, rep(NA, length(xg)))
    )

    aa <- fpredict(fit$fit_obj, tmpd)$y.pred
    yg <- tail(aa, length(xg))

    ## get control point fits
    ##---------------------------------------------------------

    cpy <- NULL
    if (!is.null(cpx)) {
      tmpd <- data.frame(
        argvals = c(dat$x, cpx),
        subj = dat$subjid[1],
        y = c(dat$y, rep(NA, length(cpx)))
      )
      aa <- fpredict(fit$fit_obj, tmpd)$y.pred
      cpy <- tail(aa, length(cpx))
    }

    list(
      fit = dfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Compute loess fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[stats]{loess}}, notably \code{span}, \code{degree}, and \code{family}
#' @export
#' @importFrom stats loess.control
fit_method.loess <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    span <- fit$dots$span
    if (is.null(span))
      span <- c(0.5, 3)

    # if span is single value, add a little on each side
    # so optimization function is happy
    if (length(span) == 1)
      span <- span + c(-1, 1) * 1e-10

    degree <- fit$dots$degree
    if (is.null(degree))
      degree <- c(1, 2)

    family <- fit$dots$family
    if (is.null(family))
      family <- "symmetric"

    lfit <- try(auto_loess(data = dat, span = span, degree = degree,
      which = "gcv", family = family,
      control = stats::loess.control(surface = "direct")), silent = TRUE)

    if (inherits(lfit, "try-error"))
      return(NULL)

    yfit <- predict(lfit, newdata = dat$x)

    yg <- predict(lfit, newdata = xg)

    cpy <- NULL
    if (!is.null(cpx)) {
      cpy <- predict(lfit, newdata = cpx)
    }

    list(
      fit = yfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = list(span = lfit$pars$span, degree = lfit$pars$degree)
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Compute gam spline fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[mgcv]{gam}}
#' @importFrom mgcv gam
#' @export
fit_method.gam <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {

    args <- c(list(formula = y ~ s(x, k = 5), data = dat), fit$dots)
    gfit <- try(do.call(mgcv::gam, args), silent = TRUE)

    if (inherits(gfit, "try-error"))
      return(NULL)

    yfit <- predict(gfit, newdata = data.frame(x = dat$x))

    yg <- predict(gfit, newdata = data.frame(x = xg))

    cpy <- NULL
    if (!is.null(cpx))
      cpy <- predict(gfit, newdata = data.frame(x = cpx))

    list(
      fit = yfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Compute smooth.spline fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[stats]{smooth.spline}}
#' @importFrom stats smooth.spline
#' @export
fit_method.smooth.spline <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    dat2 <- subset(dat, !is.na(y))

    args <- c(list(x = dat2$x, y = dat2$y), fit$dots)
    sfit <- try(do.call(stats::smooth.spline, args), silent = TRUE)

    if (inherits(sfit, "try-error"))
      return(NULL)

    yfit <- predict(sfit, x = dat$x)$y

    yg <- predict(sfit, x = xg)$y

    cpy <- NULL
    if (!is.null(cpx))
      cpy <- predict(sfit, x = cpx)$y

    list(
      fit = yfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Compute robust linear model fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[MASS]{rlm}}, also \code{p} which is the order of polynomial fit (default is quadratic, p=2)
#' @importFrom MASS rlm
#' @export
fit_method.rlm <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    p <- fit$dots$p
    if (is.null(p))
      p <- 2

    # set up data for fitting
    tmpx <- lapply(seq_len(p), function(pow) dat$x ^ pow)
    names(tmpx) <- paste0("x", seq_len(p))

    tmpx2 <- lapply(seq_len(p), function(pow) dat$x ^ pow)
    names(tmpx2) <- paste0("x", seq_len(p))

    rlmfit <- suppressWarnings(try(MASS::rlm(y ~ .,
      data = data.frame(y = dat$y, tmpx2)), silent = TRUE))
    if (inherits(rlmfit, "try-error"))
      return(NULL)

    tmpd <- data.frame(tmpx)
    yfit <- predict(rlmfit, newdata = tmpd)

    tmpx <- lapply(seq_len(p), function(pow) xg ^ pow)
    names(tmpx) <- paste0("x", seq_len(p))
    tmpd <- data.frame(tmpx)
    yg <- predict(rlmfit, newdata = tmpd)

    cpy <- NULL
    if (!is.null(cpx)) {
      tmpx <- lapply(seq_len(p), function(pow) cpx ^ pow)
      names(tmpx) <- paste0("x", seq_len(p))
      tmpd <- data.frame(tmpx)
      cpy <- predict(rlmfit, newdata = tmpd)
    }

    list(
      fit = yfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = NULL
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}


#' Compute functional "fda" fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[fda]{smooth.basisPar}}, notably \code{lambda} which defaults to 0.1
#' @importFrom fda smooth.basisPar
#' @export
fit_method.fda <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    lambda <- fit$dots$lambda
    if (is.null(lambda)) {
      lambda <- 2
      fit$dots$lambda <- NULL
    }

    if (any(duplicated(dat$x))) {
      message("had to jitter age for fda because of duplicates")
      dat$x <- jitter(dat$x)
    }
    dat <- dat[order(dat$x), ]

    args <- c(list(argvals = dat$x, y = dat$y, lambda = lambda), fit$dots)
    fdafit <- suppressWarnings(try(do.call(fda::smooth.basisPar, args),
      silent = TRUE))

    if (inherits(fdafit, "try-error"))
      return(NULL)

    x_idx <- which(dat$x <= max(dat$x, na.rm = TRUE) & dat$x >= min(dat$x, na.rm = TRUE))
    yfit <- rep(NA, length(dat$x))
    if (length(x_idx) > 0)
      yfit[x_idx] <- as.numeric(predict(fdafit, newdata = dat$x[x_idx]))
    if (any(is.na(yfit)))
      message("Note: holdout at beginning or end has resulted in NA fitted value")

    xg_idx <- which(xg <= max(dat$x, na.rm = TRUE) & xg >= min(dat$x, na.rm = TRUE))
    yg <- rep(NA, length(xg))
    if (length(xg_idx) > 0)
      yg[xg_idx] <- as.numeric(predict(fdafit, newdata = xg[xg_idx]))

    cpy <- NULL
    if (!is.null(cpx)) {
      cpx_idx <- which(cpx <= max(dat$x, na.rm = TRUE) & cpx >= min(dat$x, na.rm = TRUE))
      cpy <- rep(NA, length(cpx))
      if (length(cpx_idx) > 0)
        cpy[cpx_idx] <- as.numeric(predict(fdafit, newdata = cpx[cpx_idx]))
    }

    list(
      fit = yfit,
      fitgrid = data.frame(x = xg, y = yg),
      checkpoint = data.frame(x = cpx, y = cpy),
      pars = c(list(lambda = lambda), list(...))
    )
  }

  list(
    fit_obj = fit_obj,
    fit_apply = fit_apply,
    dots = dots
  )
}

#' Find best loess fit based on aic or gcv
#'
#' @param data a data frame with columns x and y to be used in the fitting
#' @param span a vector indicating a range (min, max) of spans to search over
#' @param degree a vector of degrees to search over (valid values are combinations of 0, 1, 2)
#' @param family loess \code{family} parameter
#' @param which which method to use, "aicc" or "gcv"
#' @param \ldots additional parameters passed to \code{\link{loess}}
#' @export
auto_loess <- function(
  data,
  span = c(0.01, 2),
  degree = c(1, 2),
  family = "gaussian",
  which = "gcv",
  ...
) {

  fit <- suppressWarnings(
    loess(y ~ x, data = data, span = mean(span),
      degree = degree[1], family = family, ...)
  )
  res <- lapply(degree, function(dg) {
    f <- function(span) {
      res <- suppressWarnings(
        try(
          loess_aic(
            update(fit, span = span, degree = dg, family = family, data = data),
            which = which
          ),
          silent = TRUE
        )
      )
      if (inherits(res, "try-error"))
        res <- Inf
      res
    }
    suppressWarnings(optimize(f, span))
  })

  idx <- which.min(sapply(res, function(x) x$objective))
  sp <- res[[idx]]$minimum
  dg <- degree[idx]

  return(update(fit, span = sp, degree = dg))
}

# Hurvich, C.M., Simonoff, J.S., and Tsai, C. L. 1998. Smoothing
# parameter selection in nonparametric regression using an improved
# Akaike Information Criterion. Journal of the Royal Statistical
# Society B 60: 271–293.
loess_aic <- function(fit, which = "aicc") {
  # span <- fit$pars$span
  n <- fit$n
  trace_l <- fit$trace.hat
  sigma2 <- sum( fit$residuals ^ 2 ) / (n - 1)
  # delta1 <- fit$one.delta
  # delta2 <- fit$two.delta
  # enp <- fit$enp

  if (which == "aicc") {
    res <- log(sigma2) + 1 + 2 * (2 * (trace_l + 1)) / (n - trace_l - 2)
  } else if (which == "gcv") {
    res <- n * sigma2 / (n - trace_l) ^ 2
  }
  res
}
