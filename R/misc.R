#' Get names of all available fitting methods
#'
#' @seealso \code{\link{get_fit}}
#' @export
get_avail_methods <- function() {
  avail_methods <- as.character(methods("fit_method"))
  gsub("fit_method\\.", "", avail_methods)
}

#' Estimate derivative given a grid of points
#'
#' @param x x variable (should be a regularly-spaced grid of points)
#' @param y y variable
#' @importFrom numDeriv grad
#' @export
grid_deriv <- function(x, y) {
  idx <- which(!is.na(y))
  if (length(idx) == 0)
    return(rep(NA, length(x)))
  idx2 <- 2:(length(idx) - 1)
  ff <- try(approxfun(x[idx], y[idx]), silent = TRUE)
  if (inherits(ff, "try-error"))
    return(rep(NA, length(x)))
  dres <- rep(NA, length(x))
  dd <- try(numDeriv::grad(ff, x[idx][idx2]), silent = TRUE)
  if (inherits(dd, "try-error"))
    return(rep(NA, length(x)))
  dres[idx] <- c(NA, dd, NA)
  dres
}

#' Merge 'htcm' and 'lencm' into the 'htcm' variable
#'
#' @param dat data
#' @export
fix_height <- function(dat) {
  if (is.null(dat$htcm)) {
    message("note: 'htcm' variable is not present - populating with NA")
    dat$htcm <- NA
  }
  if (!is.null(dat$lencm)) {
    idx1 <- which(!is.na(dat$lencm))
    if (length(idx1) > 0)
      dat$htcm[idx1] <- dat$lencm[idx1]

    idx2 <- which(!is.na(dat$htcm))
    if (length(idx2) > 0)
      dat$htcm[idx2] <- dat$htcm[idx2]
  }

  dat
}

#' log base 10 plus 1
#'
#' @param x vector of data
#' @export
log10_1 <- function(x) log10(x + 1)

#' Inverse of log base 10 plus 1
#'
#' @param x vector of data
#' @export
exp10_1 <- function(x) 10 ^ (x) - 1


add_labels <- function(vars, missing = "no label") {
  unname(sapply(vars, function(x) {
    tmp <- hbgd::hbgd_labels[[x]]
    if (is.null(tmp))
      tmp <- missing
    paste0(x, " (", tmp, ")")
  }))
}

fix_big_z <- function(z, val = 8) {
  ind <- which(abs(z) > 8)
  if (length(ind) > 0) {
    message("some z-scores were too large - setting to ", val)
    z[ind] <- sign(z[ind]) * val
  }
  z
}

#' Unit conversion utility functions
#'
#' @param x value(s) to convert
#' @export
#' @rdname unit_conversion
cm2in <- function(x) x / 2.54

#' @export
#' @rdname unit_conversion
in2cm <- function(x) x * 2.54

#' @export
#' @rdname unit_conversion
lb2kg <- function(x) x / 2.20462262

#' @export
#' @rdname unit_conversion
kg2lb <- function(x) x * 2.20462262

#' @export
#' @rdname unit_conversion
days2years <- function(x) x / 365.25

#' @export
#' @rdname unit_conversion
years2days <- function(x) x * 365.25

#' @export
#' @rdname unit_conversion
days2months <- function(x) x / 30.4375

#' @export
#' @rdname unit_conversion
months2days <- function(x) x * 30.4375

#' @export
#' @rdname unit_conversion
months2years <- function(x) x / 12

#' @export
#' @rdname unit_conversion
years2months <- function(x) x * 12


v_eval <- function(x, tryres, data) {
  if (!inherits(tryres, "try-error") && !inherits(x, "name"))
    return(x)

  res <- try(eval(x, data), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- try(eval(x), silent = TRUE)
    if (inherits(res, "try-error")) {
      stop("argument '", deparse(x), "' cannot be found")
    }
  }

  ## variable name could have been supplied in quotes
  if (length(res) == 1 && is.character(res) && nrow(data) > 1) {
    if (res %in% names(data)) {
      nm <- res
      res <- data[[res]]
      attr(res, "stringName") <- nm
    } else {
      res <- rep(res, nrow(data))
    }
  }

  res
}
