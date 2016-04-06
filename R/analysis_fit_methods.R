
fit_method <- function(obj, ...)
  UseMethod("fit_method", obj)

#' Super Learner wrapper for the brokenstick method
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[brokenstick]{brokenstick}}, most notably \code{knots}
#' @examples
#' \dontrun{
#' smc <- get_smocc_data()[1:2000,]
#' bsfit <- get_fit(smc, y_var = "haz", method = "brokenstick")
#' fit <- fit_trajectory(subset(smc, subjid == 10001), fit = bsfit)
#' plot(fit)
#' }
#' @export
#' @importFrom brokenstick brokenstick
SL.brokenstick <- function(Y, X, newX = X, family = gaussian(), knots = 6, mn = NULL, mx = NULL, ...) {
  if (family$family == "binomial")
    stop("family = binomial() not currently implemented for SL.brokenstick")

  if(is.null(mn))
    mn <- min(X$x, na.rm = TRUE)
  if(is.null(mx))
    mx <- max(X$x, na.rm = TRUE)
  knots <- seq(mn, mx, length = knots)[-knots]

  fit_obj <- brokenstick(
    x = X$x,
    y = Y,
    subject = X$subjid,
    storeX = TRUE,
    knots = knots,
    Boundary.knots = c(mn, mx))

  pred <- predict(fit_obj, newX$y, newX$x, type = "response")

  fit <- list(object = fit_obj)
  class(fit) <- "SL.brokenstick"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export
predict.SL.brokenstick <- function(object, newdata, ...) {
  predict(object$object, newdata$y, newdata$x, type = "response")
}


#' Get the result of fitting sitar to a data set
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[sitar]{sitar}}, most notably \code{df} which defaults to 3
# @importFrom face select_knots face.sparse
#' @details This essentially gets an anthropometric data set into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' smc <- get_smocc_data()[1:2000,]
#' sitfit <- get_fit(smc, y_var = "htcm", method = "sitar")
#' fit <- fit_trajectory(subset(smc, subjid == 10001), fit = sitfit)
#' plot(fit)
#' }
#' @export
#' @importFrom sitar sitar
SL.sitar <- function(Y, X, newX = X, family = gaussian(), df = 3, ...) {
  if (family$family == "binomial")
    stop("family = binomial() not currently implemented for SL.sitar")

  tmp <- data.frame(x = X$x, y = Y, id = X$subjid)
  fit_obj <- sitar(x = x, y = y, id = id, data = tmp, df = df)

  tmp <- data.frame(x = newX$x, id = newX$subjid)
  pred <- predict(fit_obj, newdata = tmp)

  fit <- list(object = fit_obj)
  class(fit) <- "SL.sitar"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export
predict.SL.sitar <- function(object, newdata, ...) {
  names(newdata)[names(newdata) == "subjid"] <- "id"
  predict(object$object, newdata = newdata)
}


#' Get the result of fitting a Laird and Ware linear or quadratic model to a data set
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters, most notably \code{deg} which controls the degree of polynomial for the fit (1 for linear and 2 for quadratic)
#' @details This essentially gets an anthropometric data set into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' lwfit <- get_fit(cpp, y_var = "haz", method = "lwmod", deg = 1)
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = lwfit)
#' plot(fit)
#' }
#' @export
#' @importFrom lme4 lmer
#' @importFrom scales rescale
SL.lwmod <- function(Y, X, newX = X, family = gaussian(), deg = 2, mn = NULL, mx = NULL, ...) {
  if (family$family == "binomial")
    stop("family = binomial() not currently implemented for SL.lwmod")

  if(is.null(mn))
    mn <- min(X$x, na.rm = TRUE)
  if(is.null(mx))
    mx <- max(X$x, na.rm = TRUE)

  dat <- data.frame(
    x = scales::rescale(X$x, from = c(mn, mx)),
    y = Y,
    subjid = X$subjid)

  if(deg == 1) {
    fit_obj <- lme4::lmer(y ~ x + (x | subjid), data = dat)
  } else if(deg == 2) {
    dat$x2 <- dat$x^2
    fit_obj <- lme4::lmer(y ~ x + x2 + (x + x2 | subjid), data = dat)
  } else {
    stop("deg must be 1 or 2 for fitting 'lwmod'")
  }

  pred <- predict(fit_obj, newdata = dat)

  fit <- list(object = fit_obj, rng = c(mn, mx))
  class(fit) <- "SL.lwmod"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export
predict.SL.lwmod <- function(object, newdata, ...) {
  newdata$x <- scales::rescale(newdata$x, from = object$rng)
  newdata$x2 <- newdata$x^2
  unname(predict(object$object, newdata = newdata))
}

#' Get the result of fitting a "Wand" model to a data set
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters, most notably \code{deg} which controls the degree of polynomial for the fit (1 for linear and 2 for quadratic)
#' @details This essentially gets an anthropometric data set into shape for \code{\link[sitar]{sitar}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' wfit <- get_fit(cpp, y_var = "haz", method = "wand")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = wfit)
#' plot(fit)
#' }
#' @export
#' @export
SL.wand <- function(Y, X, newX = X, family = gaussian(), pop_k = 2, subj_k = 2, ...) {
  if (family$family == "binomial")
    stop("family = binomial() not currently implemented for SL.wand")

  fit_obj <- wand_fit(X$x, Y, X$subjid,
    pop_k = pop_k, subj_k = subj_k, ...)

  pred <- predict(fit_obj, newdata = newX)

  fit <- list(object = fit_obj)
  class(fit) <- "SL.wand"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export
predict.SL.wand <- function(object, newdata, ...) {
  x_range <- range(newdata$x, na.rm = TRUE)
  x_idx <- which(newdata$x <= x_range[2] & newdata$x >= x_range[1])
  res <- rep(NA, length(newdata$x))
  if(length(x_idx) > 0) {
    tmpd <- data.frame(x = newdata$x[x_idx], subjid = newdata$subjid[x_idx])
    res[x_idx] <- predict(object$object, newdata = tmpd)
  }
  res
}


#' Get the result of fitting face.sparse to a data set
#'
#' @param dat data frame containing variables to model
#' @param \ldots additional parameters passed to \code{\link[face]{face.sparse}}, most notably \code{knots} which defaults to 10
# @importFrom face select_knots face.sparse
#' @details This essentially gets an anthropometric data set into shape for \code{\link[face]{face.sparse}} (sets appropriate data structure and removes missing values) and runs the fitting routine.
#' @note The settings for \code{x_trans} and \code{y_trans} must match that used in \code{\link{fit_trajectory}} and appropriate inverse transformations must be set there accordingly as well.
#' @examples
#' \dontrun{
#' facefit <- get_fit(cpp, y_var = "haz", method = "face")
#' fit <- fit_trajectory(subset(cpp, subjid == 2), fit = facefit)
#' plot(fit)
#' }
#' @export
SL.face <- function(Y, X, newX = X, family = gaussian(), knots = 10, fulldata = X, ...) {
  if (family$family == "binomial")
    stop("family = binomial() not currently implemented for SL.face")

  if(!identical(names(fulldata), c("x", "y", "subjid")))
    stop("Argument 'fulldata' for SL.face must have names 'x', 'y', and 'subjid'")
  fulldata <- data.frame(
    argvals = fulldata$x,
    subj = fulldata$subjid,
    y = fulldata$y)

  facedat <- data.frame(
    argvals = X$x,
    subj = X$subjid,
    y = Y)

  facedat <- facedat[complete.cases(facedat),]
  knots <- face::select.knots(facedat$argvals, knots = knots)
  fit_obj <- suppressMessages(face::face.sparse(facedat, knots = knots))

  newdat <- data.frame(
    argvals = newX$x,
    subj = newX$subjid,
    y = newX$y)

  fpredict <- getFromNamespace("predict.face.sparse", "face")
  pred <- tail(fpredict(fit_obj, newdat)$y.pred, nrow(newX))

  # store the fit object as well as the original data so we can get Y values
  # for future predictions
  fit <- list(object = fit_obj, fulldata = fulldata)
  class(fit) <- "SL.face"

  out <- list(pred = pred, fit = fit)
  return(out)
}

#' @export
predict.SL.face <- function(object, newdata, ...) {
  newdata <- data.frame(
    argvals = newdata$x,
    subj = newdata$subjid,
    y = NA)
  tmp <- subset(object$fulldata, subj %in% newdata$subj)

  fpredict <- getFromNamespace("predict.face.sparse", "face")
  pred <- fpredict(object$object, rbind(tmp, newdata))$y.pred
  tail(pred, nrow(newdata))
}

#' @importFrom SuperLearner SuperLearner
#' @examples
#' \dontrun{
#' smc <- get_smocc_data()[1:2000,]
#' slfit <- get_fit(smc, y_var = "haz", method = "SL")
#' fit <- fit_trajectory(subset(smc, subjid == 10001), fit = facefit)
#' plot(fit)
#' }
#' @export
SL.SL <- function(Y, X, newX = X, family = gaussian(),
  SL.library = c("SL.face", "SL.wand", "SL.lwmod", "SL.sitar", "SL.brokenstick"),
  method = "method.NNLS", verbose = FALSE, cvControl = list(V = 10L), ...) {
  sl1 <- SuperLearner(
    Y = Y,
    X = X,
    id = X$subjid,
    family = family,
    SL.library = SL.library,
    method = method,
    verbose = verbose,
    cvControl = cvControl,
    ...
  )
}

#' Compute loess fit of growth trajectory
#'
#' @template par-fit
#' @param \ldots additional parameters passed to \code{\link[stats]{loess}}, notably \code{span}, \code{degree}, and \code{family}
#' @export
fit_method.loess <- function(dat, ...) {
  dots <- list(...)

  fit_obj <- NULL

  fit_apply <- function(dat, xg = NULL, cpx = NULL, fit) {
    span <- fit$dots$span
    if(is.null(span))
      span <- c(0.5, 3)

    # if span is single value, add a little on each side
    # so optimization function is happy
    if(length(span) == 1)
      span <- span + c(-1, 1) * 1e-10

    degree <- fit$dots$degree
    if(is.null(degree))
      degree <- c(1, 2)

    family <- fit$dots$family
    if(is.null(family))
      family <- "symmetric"

    if(fit$holdout) {
      dat2 <- subset(dat, !hold)
    } else {
      dat2 <- dat
    }

    lfit <- try(auto_loess(data = dat2, span = span, degree = degree,
      which = "gcv", family = family), silent = TRUE)

    if(inherits(lfit, "try-error"))
      return(NULL)

    yfit <- predict(lfit, newdata = dat$x)

    yg <- predict(lfit, newdata = xg)

    cpy <- NULL
    if(!is.null(cpx)) {
      cpy <- predict(lfit, newdata = cpx)
    }

    list(
      xy = dat,
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

    if(fit$holdout) {
      dat2 <- subset(dat, !hold)
    } else {
      dat2 <- dat
    }

    args <- c(list(formula = y ~ s(x, k = 5), data = dat2), fit$dots)
    gfit <- try(do.call(mgcv::gam, args), silent = TRUE)

    if(inherits(gfit, "try-error"))
      return(NULL)

    yfit <- predict(gfit, newdata = data.frame(x = dat$x))

    yg <- predict(gfit, newdata = data.frame(x = xg))

    cpy <- NULL
    if(!is.null(cpx))
      cpy <- predict(gfit, newdata = data.frame(x = cpx))

    list(
      xy = dat,
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

    if(fit$holdout) {
      dat2 <- subset(dat, !hold)
    } else {
      dat2 <- dat
    }

    args <- c(list(x = dat2$x, y = dat2$y), fit$dots)
    sfit <- try(do.call(stats::smooth.spline, args), silent = TRUE)

    if(inherits(sfit, "try-error"))
      return(NULL)

    yfit <- predict(sfit, x = dat$x)$y

    yg <- predict(sfit, x = xg)$y

    cpy <- NULL
    if(!is.null(cpx))
      cpy <- predict(sfit, x = cpx)$y

    list(
      xy = dat,
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
    if(is.null(p))
      p <- 2

    if(fit$holdout) {
      dat2 <- subset(dat, !hold)
    } else {
      dat2 <- dat
    }

    # set up data for fitting
    tmpx <- lapply(seq_len(p), function(pow) dat$x^pow)
    names(tmpx) <- paste0("x", seq_len(p))

    tmpx2 <- lapply(seq_len(p), function(pow) dat2$x^pow)
    names(tmpx2) <- paste0("x", seq_len(p))

    rlmfit <- suppressWarnings(try(MASS::rlm(y ~ .,
      data = data.frame(y = dat2$y, tmpx2)), silent = TRUE))
    if(inherits(rlmfit, "try-error"))
      return(NULL)

    tmpd <- data.frame(tmpx)
    yfit <- predict(rlmfit, newdata = tmpd)

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
    if(is.null(lambda)) {
      lambda <- 2
      fit$dots$lambda <- NULL
    }

    if(any(duplicated(dat$x))) {
      message("had to jitter age for fda because of duplicates")
      dat$x <- jitter(dat$x)
    }
    dat <- dat[order(dat$x),]

    if(fit$holdout) {
      dat2 <- subset(dat, !hold)
    } else {
      dat2 <- dat
    }

    args <- c(list(argvals = dat2$x, y = dat2$y, lambda = lambda), fit$dots)
    fdafit <- suppressWarnings(try(do.call(fda::smooth.basisPar, args),
      silent = TRUE))

    if(inherits(fdafit, "try-error"))
      return(NULL)

    x_idx <- which(dat$x <= max(dat2$x, na.rm = TRUE) & dat$x >= min(dat2$x, na.rm = TRUE))
    yfit <- rep(NA, length(dat$x))
    if(length(x_idx) > 0)
      yfit[x_idx] <- as.numeric(predict(fdafit, newdata = dat$x[x_idx]))
    if(any(is.na(yfit)))
      message("Note: holdout at beginning or end has resulted in NA fitted value")

    xg_idx <- which(xg <= max(dat2$x, na.rm = TRUE) & xg >= min(dat2$x, na.rm = TRUE))
    yg <- rep(NA, length(xg))
    if(length(xg_idx) > 0)
      yg[xg_idx] <- as.numeric(predict(fdafit, newdata = xg[xg_idx]))

    cpy <- NULL
    if(!is.null(cpx)) {
      cpx_idx <- which(cpx <= max(dat2$x, na.rm = TRUE) & cpx >= min(dat2$x, na.rm = TRUE))
      cpy <- rep(NA, length(cpx))
      if(length(cpx_idx) > 0)
        cpy[cpx_idx] <- as.numeric(predict(fdafit, newdata = cpx[cpx_idx]))
    }

    list(
      xy = dat,
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


