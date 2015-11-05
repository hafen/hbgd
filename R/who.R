#' Get values of a specified measurement that correspond a given WHO quantile or z-score
#'
#' Get values of a specified measurement for a given WHO quantile/z-score and growth standard pair (e.g. length vs. age) and sex over a specified grid
#'
#' @param x vector specifying the values of x over which to provide the quantiles for y
#' @param q quantile or vector of quantiles at which to compute values (a number between 0 and 1 - 0.5 is median)
#' @param y_var y variable name (typically "htcm" or "wtkg") that specifies which variable of values should be returned for the specified value of q - see details
#' @param x_var x variable name (typically "agedays") - see details
#' @param sex "Male" or "Female"
#' @param data optional data frame that supplies any of the other variables provided to the function
#' @details for all supported pairings of \code{x_var} and \code{y_var}, type \code{names(who)}
#' @seealso \code{\link{who_value2quantile}}, \code{\link{who_value2zscore}}
#' @examples
#' # median height vs. age for females
#' x <- seq(0, 365, by = 7)
#' med <- who_quantile2value(x)
#' plot(x, med, xlab = "age in days", ylab = "median female height (cm)")
#'
#' # 99th percentile of weight vs. age for males from age 0 to 1461 days
#' dat <- data.frame(x = rep(seq(0, 1461, length = 100), 2),
#'   sex = rep(c("Male", "Female"), each = 100))
#' dat$p99 <- who_quantile2value(x, q = 0.99, y_var = "wtkg", sex = sex, data = dat)
#' lattice::xyplot(kg2lb(p99) ~ days2years(x), groups = sex, data = dat,
#'   ylab = "99th percentile weight (pounds) for males",
#'   xlab = "age (years)", auto.key = TRUE)
#' @export
#' @rdname who_quantile2value
who_quantile2value <- function(x, q = 0.5, x_var = "agedays", y_var = "htcm",
  sex = "Female", data = NULL) {

  if(!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    q <- v_eval(substitute(q), try(q, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  # if(length(q) > 1)
  #   stop("can only supply a singleton quantile")

  dat <- data.frame(x = x, q = q, x_var = x_var, y_var = y_var, sex = sex, stringsAsFactors = FALSE)

  if(! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  quantile2value_single_pars <- function(x, y, x_var, y_var, sex) {
    pair <- paste(y_var, x_var, sep = "_")
    check_pair(pair)

    coefs <- who[[pair]][[sex]]$data

    # subset to neighborhood surrounding input
    idx <- get_coef_idx(x, coefs$x)
    coefs <- coefs[idx,, drop=FALSE]
    if(nrow(coefs) == 1) {
      coefs <- data.frame(y = y, coefs, row.names = NULL)
    } else {
      coefs <- data.frame(
        x = x,
        y = y,
        l = approx(coefs$x, coefs$l, x)$y,
        m = approx(coefs$x, coefs$m, x)$y,
        s = approx(coefs$x, coefs$s, x)$y)
    }

    with(coefs, m * ((1 + qnorm(y) * l * s)^(1 / l)))
  }

  dat <- dat %>%
    dplyr::group_by(x_var, y_var, sex) %>%
    dplyr::mutate(res = quantile2value_single_pars(x, q, x_var[1], y_var[1], sex[1]))

  dat$res
}

#' @param z z-score or vector of z-scores at which to compute values
#' @export
#' @rdname who_quantile2value
who_zscore2value <- function(x, z = 0, y_var = "htcm", x_var = "agedays",
  sex = "Female", data = NULL) {

  if(!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    z <- v_eval(substitute(z), try(z, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  who_quantile2value(q = pnorm(z), x = x, y_var = y_var, x_var = x_var, sex = sex)
}

#' Get WHO z-scores or quantiles
#'
#' Compute z-scores or quantiles with respect to the WHO growth standard for given values of x vs. y (typically x is "agedays" and y is a measure like "htcm").
#'
#' @param x value or vector of values that correspond to a measure defined by \code{x_var}
#' @param y value or vector of values that correspond to a measure defined by \code{y_var}
#' @param x_var x variable name (typically "agedays") - see details
#' @param y_var y variable name (typically "htcm" or "wtkg") - see details
#' @param sex "Male" or "Female"
#' @param data optional data frame that supplies any of the other variables provided to the function
#' @details for all supported pairings of \code{x_var} and \code{y_var}, type \code{names(who)}
#' @seealso \code{\link{who_quantile2value}}, \code{\link{who_zscore2value}}
#' @examples
#' # z-scores
#' who_value2zscore(1670, in2cm(44))
#' who_value2zscore(1670, lb2kg(48), y_var = "wtkg")
#'
#' who_value2quantile(1670, in2cm(44))
#' who_value2quantile(1670, lb2kg(48), y_var = "wtkg")
#'
#' # add haz derived from WHO data and compare to that provided with data
#' cpp$haz2 <- who_value2zscore(x = agedays,  y = lencm, sex = sex, data = cpp)
#' plot(cpp$haz, cpp$haz2)
#'
#' # note that you can also do it this way
#' #' cpp$haz2 <- who_value2zscore(cpp$agedays, cpp$lencm, sex = cpp$sex)
#' @export
#' @rdname who_value2zscore
who_value2zscore <- function(x, y, x_var = "agedays", y_var = "htcm", sex = "Female", data = NULL) {

  if(!is.null(data)) {
    x <- v_eval(substitute(x), try(x, silent = TRUE), data)
    y <- v_eval(substitute(y), try(y, silent = TRUE), data)
    x_var <- v_eval(substitute(x_var), try(x_var, silent = TRUE), data)
    y_var <- v_eval(substitute(y_var), try(y_var, silent = TRUE), data)
    sex <- v_eval(substitute(sex), try(sex, silent = TRUE), data)
  }

  dat <- data.frame(x = x, y = y, x_var = x_var, y_var = y_var, sex = sex, stringsAsFactors = FALSE)

  if(! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  value2zscore_single_pars <- function(x, y, x_var, y_var, sex) {
    pair <- paste(y_var, x_var, sep = "_")
    check_pair(pair)

    coefs <- who[[pair]][[sex]]$data

    # subset to neighborhood surrounding input
    idx <- get_coef_idx(x, coefs$x)
    coefs <- coefs[idx,, drop=FALSE]

    if(nrow(coefs) == 1) {
      coefs <- data.frame(y = y, coefs, row.names = NULL)
    } else {
      coefs <- data.frame(
        x = x,
        y = y,
        m = approx(coefs$x, coefs$m, x)$y,
        l = approx(coefs$x, coefs$l, x)$y,
        s = approx(coefs$x, coefs$s, x)$y
      )
    }

    with(coefs, ((y / m)^l - 1) / (s * l))
  }

  dat <- dat %>%
    dplyr::group_by(x_var, y_var, sex) %>%
    dplyr::mutate(res = value2zscore_single_pars(x, y, x_var[1], y_var[1], sex[1]))

  dat$res
}

#' @export
#' @rdname who_value2zscore
who_value2quantile <- function(x, y, x_var = "agedays", y_var = "htcm", sex = "Female") {

  pnorm(who_value2zscore(x = x, y = y, x_var = x_var, y_var = y_var, sex = sex))
}

get_coef_idx <- function(x, coefx) {
  rng <- range(x, na.rm = TRUE)
  itr <- findInterval(coefx, rng)

  lower <- which(itr == 0)
  if(length(lower) == 0) {
    lower <- NULL
  } else {
    lower <- max(lower)
  }
  upper <- which(itr == 2)
  if(length(upper) == 0) {
    upper <- NULL
  } else {
    upper <- min(upper)
  }

  idx <- c(lower, which(itr == 1), upper)
}

check_single <- function(par, par_name) {
  if(length(par) != 1)
    stop("currently can only get z-scores for one ", par, " at a time")
}

check_pair <- function(pair) {
  if(! pair %in% c("wtkg_agedays", "htcm_agedays", "bmi_agedays", "hcircm_agedays", "muaccm_agedays", "ss_agedays", "tsftmm_agedays", "wtkg_htcm"))
    stop("x and y pairings must be one of
x_var   | y_var
--------|--------
agedays | wtkg
agedays | htcm
agedays | bmi
agedays | hcircm
agedays | muaccm
agedays | ss
agedays | tsftmm
htcm    | wtkg
")
}


