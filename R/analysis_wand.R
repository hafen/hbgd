#' @importFrom nlme pdIdent pdSymm pdIdent lme
wand_fit <- function(x, y, subjid, pop_k = 10, subj_k = 5, ...) {

  dots <- list(...)

  xrange_orig <- range(x, na.rm = TRUE)
  if (!is.null(dots$mn)){
    xrange_orig[1] <- dots$mn
    dots$mn <- NULL
  }

  if (!is.null(dots$mx)){
    xrange_orig[2] <- dots$mx
    dots$mx <- NULL
  }

  x <- x / xrange_orig[2]
  xrange <- range(x, na.rm = TRUE)

  # nolint start
  numObs <- length(y)
  numSubj <- length(unique(subjid))
  uqID <- unique(subjid)

  # set up X matrix, and Z matrix for mean curve:
  X <- model.matrix(y ~ x)
  num_int_knots <- pop_k
  int_knots <- quantile(unique(x),
    seq(0, 1, length = num_int_knots + 2))[-c(1, num_int_knots + 2)]

  Z <- ZOSull(x, xrange, int_knots)

  # Set up Z matrix for subject specific curves:
  num_int_knots_subj <- subj_k
  int_knots_subj <- quantile(unique(x),
    seq(0, 1, length = num_int_knots_subj + 2))[-c(1, num_int_knots_subj + 2)]

  ZSubj <- ZOSull(x, xrange, int_knots_subj)

  dummyId <- factor(rep(1, numObs))

  # assign("X", X, envir = .GlobalEnv)
  # assign("Z", Z, envir = .GlobalEnv)
  # assign("ZSubj", ZSubj, envir = .GlobalEnv)
  # assign("dummyId", dummyId, envir = .GlobalEnv)

  Z.block <- list(
    dummyId = nlme::pdIdent(~ -1 + Z),
    subjid = nlme::pdSymm(~ x),
    subjid = nlme::pdIdent(~ -1 + ZSubj))

  mod <- nlme::lme(y ~ -1 + X, random = Z.block)
  # nolint end

  # data_fr <- data.frame(y, X, Z, ZSubj, subjid, rep(1, length = numObs))
  # mod <- lme(y ~ -1 + X, data = data_fr, random = Z.block)

  attr(mod, "pop_k") <- pop_k
  attr(mod, "subj_k") <- subj_k
  attr(mod, "xrange_orig") <- xrange_orig
  attr(mod, "intKnots") <- int_knots
  attr(mod, "intKnotsSubj") <- int_knots_subj

  class(mod) <- c("wand", class(mod))
  mod
}

#' @importFrom splines spline.des
ZOSull <- function(x, range.x, int_knots, drv = 0) {

  # check legality of range.x

  if (!missing(range.x)) {
    if (length(range.x) != 2)    stop("range.x must be of length 2")
    if (range.x[1] > range.x[2]) stop("range.x[1] exceeds range.x[1]")
    if (range.x[1] > min(x))     stop("range.x[1] must be <= than min(x)")
    if (range.x[2] < max(x))     stop("range.x[2] must be >= than max(x)")
  }

  if (drv > 2) stop("splines not smooth enough for more than 2 derivatives")

  # set defaults for range.x and int_knots

  if (missing(range.x))
    range.x <- c(1.05 * min(x) - 0.05 * max(x), 1.05 * max(x) - 0.05 * min(x))

  if (missing(int_knots)) {
    num_int_knots <- min(length(unique(x)), 35)
    int_knots <- quantile(unique(x),
      seq(0, 1, length = (num_int_knots + 2))[-c(1, (num_int_knots + 2))])
  }
  num_int_knots <- length(int_knots)

  # obtain the penalty matrix
  all_knots <- c(rep(range.x[1], 4), int_knots, rep(range.x[2], 4))
  K <- length(int_knots)
  L <- 3 * (K + 8)
  xtilde <- (rep(all_knots, each = 3)[-c(1, (L - 1), L)] +
    rep(all_knots, each = 3)[-c(1, 2, L)]) / 2
  wts <- rep(diff(all_knots), each = 3) * rep(c(1, 4, 1) / 6, K + 7)
  Bdd <- splines::spline.des(all_knots, xtilde,
    derivs = rep(2, length(xtilde)),
    outer.ok = TRUE)$design
  Omega <- t(Bdd * wts) %*% Bdd

  # use the spectral decomposition of Omega to obtain Z
  svd_omega <- svd(Omega)
  inds_z <- 1:(num_int_knots + 2)
  UZ <- svd_omega$u[, inds_z]
  LZ <- t(t(UZ) / sqrt(svd_omega$d[inds_z]))

  # perform stability check
  inds_x <- (num_int_knots + 3):(num_int_knots + 4)
  UX <- svd_omega$u[, inds_x]
  L <- cbind(UX, LZ)
  stab_check <- t(crossprod(L, t(crossprod(L, Omega))))
  if (sum(stab_check ^ 2) > 1.0001 * (num_int_knots + 2))
    message("WARNING: NUMERICAL INSTABILITY ARISING FROM SPECTRAL DECOMPOSITION")

  # obtain B and post-multiply by LZ matrix to get Z
  B <- splines::spline.des(all_knots, x, derivs = rep(drv, length(x)),
    outer.ok = TRUE)$design

  Z <- B %*% LZ

  # add range.x and int_knots as attributes
  attr(Z, "range.x") <- range.x
  attr(Z, "intKnots") <- int_knots

  # return Z matrix with 2 attributes
  return(Z)
}

predict.wand <- function(mod, newdata) {
  # pop_k <- attr(mod, "pop_k")
  # subj_k <- attr(mod, "subj_k")
  xrange_orig <- attr(mod, "xrange_orig")
  int_knots <- attr(mod, "intKnots")
  int_knots_subj <- attr(mod, "intKnotsSubj")

  x <- scales::rescale(newdata$x, from = xrange_orig)

  # num_int_knots <- pop_k
  # int_knots <- quantile(unique(x),
  #   seq(0, 1, length = num_int_knots + 2))[-c(1, num_int_knots + 2)]

  # num_int_knots_subj <- subj_k
  # int_knots_subj <- quantile(unique(x),
  #   seq(0, 1, length = num_int_knots_subj + 2))[-c(1, num_int_knots_subj + 2)]

  subjid <- paste0("1/", newdata$subjid[1])

  Xg <- cbind(rep(1, length(x)), x)
  Zg <- ZOSull(x, c(0, 1), int_knots)
  Zsubjg <- ZOSull(x, c(0, 1), int_knots_subj)
  beta_hat <- as.vector(mod$coef$fixed)
  u_hat <- as.vector(mod$coef$random[[1]])
  fhatg <- Xg %*% beta_hat + Zg %*% u_hat

  u_lin_hati <- as.vector(mod$coef$random[[2]][subjid, ])
  u_spl_hati <- as.vector(mod$coef$random[[3]][subjid, ])
  ghati <- Xg %*% u_lin_hati + Zsubjg %*% u_spl_hati

  as.vector(fhatg + ghati)
}
