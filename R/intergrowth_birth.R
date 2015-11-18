#' Convert prenatal ultrasound measurements to Intergrowth z-scores/centiles (generic)
#'
#' @param gagebrth gestational age at birth in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm")
#' @param sex "Male" or "Female"
#' @references International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project
#' Villar, José et al.
#' The Lancet, Volume 384, Issue 9946, 857-868
#' @examples
#' # get 99th centile for Male birth weights across some gestational ages
#' igb_centile2value(232:300, 99, var = "wtkg", sex = "Male")
#' @importFrom gamlss.dist qST3 pST3
#' @rdname igb_zscore2value
#' @export
igb_centile2value <- function(gagebrth, p = 50, var = "lencm", sex = "Female") {

  dat <- data.frame(x = gagebrth, p = p, var = var, sex = sex,
    stringsAsFactors = FALSE)

  if(! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  ig_centile2value_single_pars <- function(x, y, var, sex) {
    coefs <- hbgd::ig_coefs[[var]][[sex]]
    # the best we can do is daily resolution
    x <- as.integer(round(x))
    idx <- match(x, coefs$ga)
    na_idx <- is.na(idx)
    res <- rep(NA, length(idx))
    res[!na_idx] <- gamlss.dist::qST3(y[!na_idx] / 100,
      coefs$mu[idx][!na_idx],
      coefs$sigma[idx][!na_idx],
      coefs$nu[idx][!na_idx],
      coefs$tau[idx][!na_idx])

    res
  }

  dat <- dat %>%
    dplyr::group_by(var, sex) %>%
    dplyr::mutate(res = ig_centile2value_single_pars(x, p, var[1], sex[1]))

  dat$res
}

#' @rdname igb_zscore2value
#' @export
igb_zscore2value <- function(gagebrth, z = 0, var = "lencm", sex = "Female") {
  igb_centile2value(gagebrth, p = 100 * pnorm(z), var = var, sex = sex)
}

#' Convert prenatal ultrasound measurements to Intergrowth z-scores/centiles (generic)
#'
#' @param gagebrth gestational age at birth in days
#' @param val the value(s) of the anthro measurement to convert
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm")
#' @param sex "Male" or "Female"
#' @references International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project
#' Villar, José et al.
#' The Lancet, Volume 384, Issue 9946, 857-868
#' @examples
#' # get Male birth length z-scores
#' # first we need just 1 record per subject with subject-level data
#' cppsubj <- get_subject_data(cpp)
#' cppsubj <- subset(cppsubj, sex == "Male")
#' igb_value2zscore(cpp$gagebrth, cpp$birthlen, var = "lencm", sex = "Male")
#' @rdname igb_value2zscore
#' @export
igb_value2centile <- function(gagebrth, val, var = "lencm", sex = "Female") {

  dat <- data.frame(x = gagebrth, y = val, var = var, sex = sex, stringsAsFactors = FALSE)

  if(! all(unique(dat$sex) %in% c("Male", "Female")))
    stop("sex must be 'Male' or 'Female'")

  # since coefficients are available only by pair/sex
  # we need to call this for each unique combination
  ig_value2centile_single_pars <- function(x, y, var, sex) {
    coefs <- hbgd::ig_coefs[[var]][[sex]]
    # the best we can do is daily resolution
    x <- as.integer(round(x))
    idx <- match(x, coefs$ga)
    na_idx <- is.na(idx)
    res <- rep(NA, length(idx))
    res[!na_idx] <- gamlss.dist::pST3(y[!na_idx],
      coefs$mu[idx][!na_idx],
      coefs$sigma[idx][!na_idx],
      coefs$nu[idx][!na_idx],
      coefs$tau[idx][!na_idx]) * 100

    res
  }

  dat <- dat %>%
    dplyr::group_by(var, sex) %>%
    dplyr::mutate(res = ig_value2centile_single_pars(x, y, var[1], sex[1]))

  dat$res
}

#' @rdname igb_value2zscore
#' @export
igb_value2zscore <- function(gagebrth, val, var = "lencm", sex = "Female") {
  qnorm(igb_value2centile(gagebrth, val, var = var, sex = sex) / 100)
}


## **2zscore
##---------------------------------------------------------

#' Convert prenatal ultrasound measurements to Intergrowth z-scores/centiles
#'
#' @param gagebrth gestational age at birth in days
#' @param wtkg weight (kg) measurement(s) to convert
#' @param lencm length(cm) measurement(s) to convert
#' @param hcircm head circumference (cm) measurement(s) to convert
#' @param sex "Male" or "Female"
#' @references International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project
#' Villar, José et al.
#' The Lancet, Volume 384, Issue 9946, 857-868
#' @examples
#' # get Male birth length z-scores
#' # first we need just 1 record per subject with subject-level data
#' cppsubj <- get_subject_data(cpp)
#' cppsubj <- subset(cppsubj, sex == "Male")
#' igb_lencm2zscore(cpp$gagebrth, cpp$birthlen, sex = "Male")
#' @rdname igb_var2zscore
#' @export
igb_lencm2zscore <- function(gagebrth, lencm, sex = "Female") {
  igb_value2zscore(gagebrth, lencm, var = "lencm", sex = sex)
}

#' @rdname igb_var2zscore
#' @export
igb_wtkg2zscore <- function(gagebrth, wtkg, sex = "Female") {
  igb_value2zscore(gagebrth, wtkg, var = "wtkg", sex = sex)
}

#' @rdname igb_var2zscore
#' @export
igb_hcircm2zscore <- function(gagebrth, hcircm, sex = "Female") {
  igb_value2zscore(gagebrth, hcircm, var = "hcircm", sex = sex)
}

## **2centile
##---------------------------------------------------------

#' @rdname igb_var2zscore
#' @export
igb_lencm2centile <- function(gagebrth, lencm, sex = "Female") {
  igb_value2centile(gagebrth, lencm, var = "lencm", sex = sex)
}

#' @rdname igb_var2zscore
#' @export
igb_wtkg2centile <- function(gagebrth, wtkg, sex = "Female") {
  igb_value2centile(gagebrth, wtkg, var = "wtkg", sex = sex)
}

#' @rdname igb_var2zscore
#' @export
igb_hcircm2centile <- function(gagebrth, hcircm, sex = "Female") {
  igb_value2centile(gagebrth, hcircm, var = "hcircm", sex = sex)
}

## zscore2**
##---------------------------------------------------------

#' Convert Intergrowth z-scores/centiles to prenatal ultrasound measurements
#'
#' @param gagebrth gestational age at birth in days
#' @param z z-score(s) to convert
#' @param p centile(s) to convert (must be between 0 and 100)
#' @param sex "Male" or "Female"
#' @references International standards for newborn weight, length, and head circumference by gestational age and sex: the Newborn Cross-Sectional Study of the INTERGROWTH-21st Project
#' Villar, José et al.
#' The Lancet, Volume 384, Issue 9946, 857-868
#' @examples
#' # get 99th centile for Male birth weights across some gestational ages
#' igb_centile2wtkg(232:300, 99, sex = "Male")
#' @rdname igb_zscore2var
#' @export
igb_zscore2lencm <- function(gagebrth, z = 0, sex = "Female") {
  igb_zscore2value(gagebrth, z, var = "lencm", sex = sex)
}

#' @rdname igb_zscore2var
#' @export
igb_zscore2wtkg <- function(gagebrth, z = 0, sex = "Female") {
  igb_zscore2value(gagebrth, z, var = "wtkg", sex = sex)
}

#' @rdname igb_zscore2var
#' @export
igb_zscore2hcircm <- function(gagebrth, z = 0, sex = "Female") {
  igb_zscore2value(gagebrth, z, var = "hcircm", sex = sex)
}

## centile2**
##---------------------------------------------------------

#' @rdname igb_zscore2var
#' @export
igb_centile2lencm <- function(gagebrth, p = 50, sex = "Female") {
  igb_centile2value(gagebrth, p, var = "lencm", sex = sex)
}

#' @rdname igb_zscore2var
#' @export
igb_centile2wtkg <- function(gagebrth, p = 50, sex = "Female") {
  igb_centile2value(gagebrth, p, var = "wtkg", sex = sex)
}

#' @rdname igb_zscore2var
#' @export
igb_centile2hcircm <- function(gagebrth, p = 50, sex = "Female") {
  igb_centile2value(gagebrth, p, var = "hcircm", sex = sex)
}


