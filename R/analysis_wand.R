#' @importFrom nlme pdIdent pdSymm pdIdent lme
wand_fit <- function(x, y, subjid, pop_k = 10, subj_k = 5, ...) {

  dots <- list(...)
  
  xrange_orig <- range(x, na.rm = TRUE)
  if(!is.null(dots$mn)){
    xrange_orig[1] <- dots$mn
    dots$mn <- NULL
  }
  
  if(!is.null(dots$mx)){
    xrange_orig[2] <- dots$mx
    dots$mx <- NULL
  }
  
  x <- x/xrange_orig[2]
  xrange <- range(x, na.rm = TRUE)
  
  

  numObs <- length(y)
  numSubj <- length(unique(subjid))
  uqID <- unique(subjid)

  # set up X matrix, and Z matrix for mean curve:
  X <- model.matrix(y ~ x)
  numIntKnots <- pop_k
  intKnots <- quantile(unique(x),
    seq(0, 1, length = numIntKnots + 2))[-c(1, numIntKnots + 2)]

  Z <- ZOSull(x, xrange, intKnots)

  # Set up Z matrix for subject specific curves:
  numIntKnotsSubj <- subj_k
  intKnotsSubj <- quantile(unique(x),
    seq(0, 1, length = numIntKnotsSubj + 2))[-c(1, numIntKnotsSubj + 2)]

  ZSubj <- ZOSull(x, xrange, intKnotsSubj)

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

  # data_fr <- data.frame(y, X, Z, ZSubj, subjid, rep(1, length = numObs))
  # mod <- lme(y ~ -1 + X, data = data_fr, random = Z.block)

  attr(mod, "pop_k") <- pop_k
  attr(mod, "subj_k") <- subj_k
  attr(mod, "xrange_orig") <- xrange_orig
  attr(mod, "intKnots") <- intKnots
  attr(mod, "intKnotsSubj") <- intKnotsSubj

  class(mod) <- c("wand", class(mod))
  mod
}

#' @importFrom splines spline.des
ZOSull <- function(x, range.x, intKnots, drv = 0) {

  # check legality of range.x

  if (!missing(range.x)) {
    if (length(range.x) != 2)    stop("range.x must be of length 2")
    if (range.x[1] > range.x[2]) stop("range.x[1] exceeds range.x[1]")
    if (range.x[1] > min(x))     stop("range.x[1] must be <= than min(x)")
    if (range.x[2] < max(x))     stop("range.x[2] must be >= than max(x)")
  }

  if (drv > 2) stop("splines not smooth enough for more than 2 derivatives")

  # set defaults for range.x and intKnots

  if (missing(range.x))
    range.x <- c(1.05 * min(x) - 0.05 * max(x), 1.05 * max(x) - 0.05 * min(x))

  if (missing(intKnots)) {
    numIntKnots <- min(length(unique(x)), 35)
    intKnots <- quantile(unique(x),
      seq(0, 1, length = (numIntKnots + 2))[-c(1, (numIntKnots + 2))])
  }
  numIntKnots <- length(intKnots)

  # obtain the penalty matrix
  allKnots <- c(rep(range.x[1], 4), intKnots, rep(range.x[2], 4))
  K <- length(intKnots)
  L <- 3 * (K + 8)
  xtilde <- (rep(allKnots, each = 3)[-c(1, (L - 1), L)] +
    rep(allKnots, each = 3)[-c(1, 2, L)]) / 2
  wts <- rep(diff(allKnots), each = 3) * rep(c(1, 4, 1) / 6, K + 7)
  Bdd <- splines::spline.des(allKnots, xtilde,
    derivs = rep(2, length(xtilde)),
    outer.ok = TRUE)$design
  Omega <- t(Bdd*wts) %*% Bdd

  # use the spectral decomposition of Omega to obtain Z
  svdOmega <- svd(Omega)
  indsZ <- 1:(numIntKnots + 2)
  UZ <- svdOmega$u[,indsZ]
  LZ <- t(t(UZ) / sqrt(svdOmega$d[indsZ]))

  # perform stability check
  indsX <- (numIntKnots + 3):(numIntKnots + 4)
  UX <- svdOmega$u[,indsX]
  L <- cbind(UX, LZ)
  stabCheck <- t(crossprod(L, t(crossprod(L, Omega))))
  if (sum(stabCheck^2) > 1.0001 * (numIntKnots + 2))
    message("WARNING: NUMERICAL INSTABILITY ARISING FROM SPECTRAL DECOMPOSITION")

  # obtain B and post-multiply by LZ matrix to get Z
  B <- splines::spline.des(allKnots, x, derivs = rep(drv, length(x)),
    outer.ok = TRUE)$design

  Z <- B %*% LZ

  # add range.x and intKnots as attributes
  attr(Z, "range.x") <- range.x
  attr(Z, "intKnots") <- intKnots

  # return Z matrix with 2 attributes
  return(Z)
}

predict.wand <- function(mod, newdata) {
  pop_k <- attr(mod, "pop_k")
  subj_k <- attr(mod, "subj_k")
  xrange_orig <- attr(mod, "xrange_orig")
  intKnots <- attr(mod, "intKnots")
  intKnotsSubj <- attr(mod, "intKnotsSubj")

  x <- scales::rescale(newdata$x, from = xrange_orig)

  # numIntKnots <- pop_k
  # intKnots <- quantile(unique(x),
  #   seq(0, 1, length = numIntKnots + 2))[-c(1, numIntKnots + 2)]

  # numIntKnotsSubj <- subj_k
  # intKnotsSubj <- quantile(unique(x),
  #   seq(0, 1, length = numIntKnotsSubj + 2))[-c(1, numIntKnotsSubj + 2)]

  subjid <- paste0("1/", newdata$subjid[1])

  Xg <- cbind(rep(1, length(x)), x)
  Zg <- ZOSull(x, c(0, 1), intKnots)
  Zsubjg <- ZOSull(x, c(0, 1), intKnotsSubj)
  betaHat <- as.vector(mod$coef$fixed)
  uHat <- as.vector(mod$coef$random[[1]])
  fhatg <- Xg %*% betaHat + Zg %*% uHat

  uLinHati <- as.vector(mod$coef$random[[2]][subjid,])
  uSplHati <- as.vector(mod$coef$random[[3]][subjid,])
  ghati <- Xg %*% uLinHati + Zsubjg %*% uSplHati

  as.vector(fhatg + ghati)
}
