fit_method <- function(obj, ...)
  UseMethod("fit_method", obj)

#' Compute loess fit of growth trajectory
#'
#' @template par-fit
#' @param span \code{\link{loess}} smoothing parameter
#' @param degree \code{\link{loess}} smoothing parameter
#' @param family \code{\link{loess}} smoothing parameter
#' @param \ldots additional parameters passed to \code{\link{loess}}
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
#' @param \ldots additional parameters passed to \code{\link{gam}}
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
#' @param \ldots additional parameters passed to \code{\link{rlm}}
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


# # fda
# girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf, lambda=0.1))
# plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
#   main="Girls in Berkeley Growth Study" )

# girlGrowthSm <- with(growth, smooth.basisPar(argvals=age, y=hgtf, lambda=0.0002))
# plot(girlGrowthSm$fd, xlab="age", ylab="height (cm)",
#   main="Girls in Berkeley Growth Study" )

# plot(deriv(girlGrowthSm$fd), xlab="age", ylab="growth rate (cm / year)",
#   main="Girls in Berkeley Growth Study" )
# plot(deriv(girlGrowthSm$fd, 2), xlab="age",
#   ylab="growth acceleration (cm / year^2)",
#   main="Girls in Berkeley Growth Study" )


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


