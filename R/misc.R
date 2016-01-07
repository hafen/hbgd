#' Estimate derivative given a grid of points
#'
#' @param x x variable (should be a regularly-spaced grid of points)
#' @param y y variable
#' @importFrom numDeriv grad
#' @export
grid_deriv <- function(x, y) {
  idx <- which(!is.na(y))
  if(length(idx) == length(x))
    return(rep(NA, length(x)))
  idx2 <- 2:(length(idx) - 1)
  ff <- try(approxfun(x[idx], y[idx]), silent = TRUE)
  if(inherits(ff, "try-error"))
    return(rep(NA, length(x)))
  dres <- rep(NA, length(x))
  dres[idx] <- c(NA, numDeriv::grad(ff, x[idx][idx2]), NA)
  dres
}

#' Merge htcm and lencm into one variable
#'
#' @param dat data
#' @param height_var name of height variable
#' @param length_var name of length variable
#' @param target name of target variable to hold the merged variable
#' @export
fix_height <- function(dat, height_var = "htcm", length_var = "lencm", target = "htcm") {
  if(!target %in% names(dat))
    dat[[target]] <- NA

  idx1 <- which(!is.na(dat[[length_var]]))
  if(length(idx1) > 0)
    dat[[target]][idx1] <- dat[[length_var]][idx1]

  idx2 <- which(!is.na(dat[[height_var]]))
  if(length(idx2) > 0)
    dat[[target]][idx2] <- dat[[height_var]][idx2]

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
exp10_1 <- function(x) 10^(x) - 1


add_labels <- function(vars, missing = "no label") {
  unname(sapply(vars, function(x) {
    tmp <- hbgd::hbgd_labels[[x]]
    if(is.null(tmp))
      tmp <- missing
    paste0(x, " (", tmp, ")")
  }))
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


# update names of data when non-standard variable names are used
update_var_names <- function(names_list, dat) {
  dat_names <- names(dat)
  for(nm in names(names_list)) {
    if(!names_list[[nm]] %in% dat_names)
      stop(sprintf("argument %s='%s': variable '%s' not present in the data", nm, names_list[[nm]], names_list[[nm]]), call. =  FALSE)
    names(dat)[which(names_list[[nm]] == dat_names)] <- nm
  }
  dat
}

v_eval <- function(x, tryres, data) {
  if(!inherits(tryres, "try-error") && !inherits(x, "name"))
    return(x)

  res <- try(eval(x, data), silent = TRUE)

  if(inherits(res, "try-error")) {
    res <- try(eval(x), silent = TRUE)
    if(inherits(res, "try-error")) {
      stop("argument '", deparse(x), "' cannot be found")
    }
  }

  ## variable name could have been supplied in quotes
  if(length(res) == 1 && is.character(res) && nrow(data) > 1) {
    if(res %in% names(data)) {
      nm <- res
      res <- data[[res]]
      attr(res, "stringName") <- nm
    } else {
      res <- rep(res, nrow(data))
    }
  }

  res
}
