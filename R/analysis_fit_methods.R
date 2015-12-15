fit_method <- function(obj, ...)
  UseMethod("fit_method", obj)

#' Compute loess fit of growth trajectory
#'
#' @template par-fit
#' @param span \code{\link[stats]{loess}} smoothing parameter
#' @param degree \code{\link[stats]{loess}} smoothing parameter
#' @param family \code{\link[stats]{loess}} smoothing parameter
#' @param \ldots additional parameters passed to \code{\link[stats]{loess}}
#' @export
fit_method.loess <- function(dat, xg = NULL, cpx = NULL,
  span = c(0.05, 3), degree = c(1, 2), family = "symmetric", ...) {

  # if span is single value, add a little on each side
  # so optimization function is happy
  if(length(span) == 1)
    span <- span + c(-1, 1) * 1e-10

  fit <- try(auto_loess(data = dat, span = span, degree = degree,
    which = "gcv", family = family, ...), silent = TRUE)

  if(inherits(fit, "try-error"))
    return(NULL)

  yg <- predict(fit, newdata = xg)

  cpy <- NULL
  if(!is.null(cpx)) {
    cpy <- predict(fit, newdata = cpx)
  }

  list(
    xy = dat,
    fit = fitted(fit),
    fitgrid = data.frame(x = xg, y = yg),
    checkpoint = data.frame(x = cpx, y = cpy),
    pars = list(span = fit$pars$span, degree = fit$pars$degree)
  )
}

#' Compute spline fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[mgcv]{gam}}
#' @importFrom mgcv gam
#' @export
fit_method.gam <- function(dat, xg = NULL, cpx = NULL, ...) {

  fit <- try(mgcv::gam(y ~ s(x), data = dat, ...), silent = TRUE)

  if(inherits(fit, "try-error"))
    return(NULL)

  yg <- predict(fit, newdata = data.frame(x = xg))

  cpy <- NULL
  if(!is.null(cpx))
    cpy <- predict(fit, newdata = data.frame(x = cpx))

  list(
    xy = dat,
    fit = fitted(fit),
    fitgrid = data.frame(x = xg, y = yg),
    checkpoint = data.frame(x = cpx, y = cpy),
    pars = NULL
  )
}

#' Compute robust linear model fit of growth trajectory
#'
#' @template par-fit
#' @param p order of polynomial fit (default is quadratic, p=2)
#' @param \ldots additional parameters passed to \code{\link[MASS]{rlm}}
#' @importFrom MASS rlm
#' @export
fit_method.rlm <- function(dat, xg = NULL, cpx = NULL, p = 2, ...) {

  # set up data for fitting
  tmpx <- lapply(seq_len(p), function(pow) dat$x^pow)
  names(tmpx) <- paste0("x", seq_len(p))
  rlmfit <- suppressWarnings(try(MASS::rlm(y ~ .,
    data = data.frame(y = dat$y, tmpx), ...), silent = TRUE))
  if(inherits(rlmfit, "try-error"))
    return(NULL)

  tmpx <- lapply(seq_len(p), function(pow) xg^pow)
  names(tmpx) <- paste0("x", seq_len(p))
  tmpd <- data.frame(tmpx)
  yg <- predict(rlmfit, newdata = tmpd)

  cpy <- NULL
  if(!is.null(cpx)) {
    tmpx <- lapply(seq_len(p), function(pow) cpx^pow)
    names(tmpx) <- paste0("x", seq_len(p))
    tmpd <- data.frame(tmpx)
    cpy <- predict(rlmfit, newdata = tmpd)
  }

  list(
    xy = dat,
    fit = fitted(rlmfit),
    fitgrid = data.frame(x = xg, y = yg),
    checkpoint = data.frame(x = cpx, y = cpy),
    pars = NULL
  )
}

#' Get the result of fitting face.sparse to a data set
#'
#' @param dat data frame containing variables to model
#' @param x_var name of x variable to model (default "agedays")
#' @param y_var name of y variable to model (default "htcm")
#' @param knots number of knots, sent to \code{\link[face]{select.knots}}
#' @param x_trans,y_trans transformation functions to be applied to x and y prior to modeling (see note)
#' @param \ldots additional parameters passed to \code{\link[face]{face.sparse}}
# @importFrom face select.knots face.sparse
#' @details This essentially gets an anthropometric data set into shape for \code{\link[face]{face.sparse}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \donttest{
#' facefit <- get_face_fit(cpp, y_var = "wtkg")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), y_var = "wtkg",
#'   method = "face", fit = facefit)
#' plot(fit)
#' }
#' @export
get_face_fit <- function(dat, x_var = "agedays", y_var = "htcm", knots = 10,
  x_trans = NULL, y_trans = NULL, ...) {

  # temporary check until we can import face
  chk <- try(face::face.sparse, silent = TRUE)
  if(inherits(chk, "try-error"))
    stop("must install 'face' package")

  ## handle transformation
  default_trans <- log10_1
  if(y_var %in% c("haz", "waz") && x_var == "agedays")
    default_trans <- identity
  if(is.null(x_trans))
    x_trans <- default_trans
  if(is.null(y_trans))
    y_trans <- default_trans

  facedat <- data.frame(
    argvals = x_trans(dat[[x_var]]),
    subj = dat$subjid,
    y = y_trans(dat[[y_var]])
  )
  facedat <- facedat[complete.cases(facedat),]

  knots <- face::select.knots(facedat$argvals, knots = 10)

  face::face.sparse(facedat, knots = knots)
}

#' Compute fpca "face" fit of growth trajectory
#'
#' @template par-fit
#' @param fit the result of running \code{\link[face]{face.sparse}} against the entire data set (with convenience wrapper \code{\link{get_face_fit}}) which is used to compute per-subject fits
# @importFrom face predict.face.sparse
#' @export
fit_method.face <- function(dat, xg = NULL, cpx = NULL, fit) {

  # temporary check until we can import face
  chk <- try(face::predict.face.sparse, silent = TRUE)
  if(inherits(chk, "try-error"))
    stop("must install 'face' package")

  # tmp <- subset(cpp, subjid == 2)
  # dat <- data.frame(
  #   x = tmp$agedays,
  #   y = tmp$wtkg,
  #   subjid = tmp$subjid
  # )

  ## get xgrid fits
  ##---------------------------------------------------------

  tmpd <- data.frame(
    argvals = c(dat$x, xg),
    subj = dat$subjid[1],
    y = c(dat$y, rep(NA, length(xg)))
  )

  aa <- face::predict.face.sparse(fit, tmpd)$y.pred
  yg <- tail(aa, length(xg))

  ## get control point fits
  ##---------------------------------------------------------

  cpy <- NULL
  if(!is.null(cpx)) {
    tmpd <- data.frame(
      argvals = c(dat$x, cpx),
      subj = dat$subjid[1],
      y = c(dat$y, rep(NA, length(cpx)))
    )
    aa <- face::predict.face.sparse(fit, tmpd)$y.pred
    cpy <- tail(aa, length(cpx))
  }

  list(
    xy = dat,
    fit = NULL,
    fitgrid = data.frame(x = xg, y = yg),
    checkpoint = data.frame(x = cpx, y = cpy),
    pars = NULL
  )
}

#' Compute functional "fda" fit of growth trajectory
#'
#' @template par-fit
#' @param lambda smoothing parameter passed to \code{\link[fda]{smooth.basisPar}}
#' @param \ldots additional parameters passed to \code{\link[fda]{smooth.basisPar}}
#' @importFrom fda smooth.basisPar
#' @export
fit_method.fda <- function(dat, xg = NULL, cpx = NULL, lambda = 0.1, ...) {

  fdafit <- suppressWarnings(try(
    fda::smooth.basisPar(argvals = dat$x, y = dat$y, lambda = lambda, ...),
    silent = TRUE))

  if(inherits(fdafit, "try-error"))
    return(NULL)

  xg_idx <- which(xg <= max(dat$x, na.rm = TRUE) & xg >= min(dat$x, na.rm = TRUE))
  yg <- rep(NA, length(xg))
  if(length(xg_idx) > 0)
    yg[xg_idx] <- as.numeric(predict(fdafit, newdata = xg[xg_idx]))

  cpy <- NULL
  if(!is.null(cpx)) {
    cpx_idx <- which(cpx <= max(dat$x, na.rm = TRUE) & cpx >= min(dat$x, na.rm = TRUE))
    cpy <- rep(NA, length(cpx))
    if(length(cpx_idx) > 0)
      cpy[cpx_idx] <- as.numeric(predict(fdafit, newdata = cpx[cpx_idx]))
  }

  list(
    xy = dat,
    fit = as.numeric(fitted(fdafit)),
    fitgrid = data.frame(x = xg, y = yg),
    checkpoint = data.frame(x = cpx, y = cpy),
    pars = c(list(lambda = lambda), list(...))
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
auto_loess <- function(data, span = c(0.01, 2), degree = c(1, 2), family = "gaussian", which = "gcv", ...) {

  fit <- suppressWarnings(loess(y ~ x, data = data, span = mean(span), degree = degree[1], family = family, ...))
  res <- lapply(degree, function(dg) {
    f <- function(span) {
      res <- suppressWarnings(try(loess_aic(update(fit, span = span, degree = dg, family = family, data = data), which = which), silent = TRUE))
      if(inherits(res, "try-error"))
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
  span <- fit$pars$span
  n <- fit$n
  traceL <- fit$trace.hat
  sigma2 <- sum( fit$residuals^2 ) / (n-1)
  delta1 <- fit$one.delta
  delta2 <- fit$two.delta
  enp <- fit$enp

  if(which == "aicc") {
    res <- log(sigma2) + 1 + 2 * (2 * (traceL + 1)) / (n - traceL - 2)
  } else if(which == "gcv") {
    res <- n * sigma2 / (n - traceL)^2
  }
}


