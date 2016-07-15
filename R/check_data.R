#' Check a dataset to ensure it will be compatible with hbgd methods
#'
#' @param dat a data frame
#' @param has_height does this dataset contain anthropometric height data?
#' @param has_weight does this dataset contain anthropometric weight data?
#' @param has_hcir does this dataset contain anthropometric head circumference data?
#' @examples
#' check_data(cpp, has_hcir = FALSE)
#'
#' smc <- brokenstick::smocc.hgtwgt
#' check_data(smc, has_hcir = FALSE)
#'
#' names(smc)[2] <- "subjid"
#' names(smc)[5] <- "agedays"
#' smc$sex <- as.character(smc$sex)
#' smc$sex[smc$sex == "male"] <- "Male"
#' smc$sex[smc$sex == "female"] <- "Female"
#' names(smc)[10] <- "htcm"
#' names(smc)[11] <- "wtkg"
#'
#' check_data(smc, has_hcir = FALSE)
#'
#' names(smc)[12] <- "haz"
#' smc$waz <- who_wtkg2zscore(smc$agedays, smc$wtkg, smc$sex)
#' smc$agedays <- smc$agedays * 365.25
#'
#' check_data(smc, has_hcir = FALSE)
#' @export
#' @importFrom crayon red green inverse bold
#' @importFrom stringdist stringdist
check_data <- function(dat, has_height = TRUE, has_weight = TRUE, has_hcir = TRUE) {
  nms <- tolower(names(dat))

  passed <- TRUE

  message("Checking if data is a data frame... ", appendLF = FALSE)
  if (!is.data.frame(dat)) {
    message(.exx)
    passed <- FALSE
  } else {
    message(.chk)
  }

  ## check for caps
  message("Checking variable name case... ", appendLF = FALSE)
  if (any(grepl("[A-Z]", names(dat)))) {
    message(.exx)
    message(.iv("  All variable names are expected to be lowercase. "))
    message(.iv("  Please fix with: "))
    message(.iv("  names(dat) <- tolower(names(dat)) "))
    message(.iv("  Continuing other checks assuming lowercase names... "))
    passed <- FALSE
  } else {
    message(.chk)
  }

  ## must have varibles: 'subjid', 'sex', 'agedays'
  psd <- sapply(c("subjid", "agedays", "sex"), function(nm)
    check_variable(nm, nms))
  passed <- passed && all(psd)

  ## 'sex' must be "Male", and "Female"
  if (psd["sex"]) {
    message("Checking values of variable 'sex'... ", appendLF = FALSE)
    if (!all(unique(dat$sex) %in% c("Male", "Female"))) {
      message(.exx)
      message(.iv("  All values of variable 'sex' must be 'Male' and 'Female'. "))
      passed <- FALSE
    } else {
      message(.chk)
    }
  }

  ## antro fields...
  anthro <- NULL
  if (has_height) anthro <- c(anthro, c("lencm", "htcm"))
  if (has_weight) anthro <- c(anthro, "wtkg")
  if (has_hcir) anthro <- c(anthro, "hcircm")
  sapply(anthro, function(nm)
    check_variable(nm, nms, req = FALSE))

  ## if both htcm and lencm, note that a good idea is to merge
  if (has_height) {
    message("Checking for both 'lencm' and 'htcm'... ")
    if (all(c("lencm", "htcm") %in% nms)) {
      if (length(which(complete.cases(dat[, c("lencm", "htcm")]))) == 0) {
        message(.iv(" Found both 'lencm' and 'htcm' in the data. "))
        message(.iv(" For modeling purposes, consider merging the two with: "))
        message(.iv(" dat <- fix_height(dat) "))
      }
    }
  }

  ## see if z-score fields exist
  if (has_height)
    check_zscore_var(c("htcm", "lencm"), "htcm", "haz", "height", nms)
  if (has_weight)
    check_zscore_var("wtkg", "wtkg", "waz", "weight", nms)
  if (has_hcir)
    check_zscore_var("hcircm", "hcircm", "hcaz", "head circumference", nms)

  ## check longitudinal
  if ("subjid" %in% nms) {
    message("Checking to see if data is longitudinal... ", appendLF = FALSE)
    tbl <- table(dat$subjid)
    if (any(tbl > 1)) {
      message(.chk)
    } else {
      message(.iv("  There is only one record per subject... "))
      message(.iv("  Much of the functionality relies on longitudinal data. "))
    }
  }

  ## see what other variables exist in the data that aren't found in hbgd_labels
  message("Checking names in data that are not standard 'hbgd' variables...")
  xnms <- setdiff(nms, names(hbgd::hbgd_labels))
  if (length(xnms) > 0) {
    message(.iv("  The following variables were found in the data: "))
    message(.iv(" ", paste0(crayon::bold(xnms), collapse = ", "), ""))
    message(.iv("  Run view_variables() to see if any of these can be mapped "))
    message(.iv("  to an 'hbgd' variable name. "))
  }

  if (passed) {
    message(crayon::green("All checks passed!"))
    message(crayon::green("As a final check, please ensure the units of measurement match"))
    message(
      crayon::green("the variable descriptions (e.g. age in days, height in centimeters, etc.).")
    )
  } else {
    message(crayon::red("Some checks did not pass - please take action accordingly."))
  }

  invisible(passed)
}

#' View known hbgd variables
#'
#' @export
#' @importFrom DT datatable
view_variables <- function() {
  tmp <- hbgd::hbgd_labels_df
  names(tmp) <- c("variable", "description")
  DT::datatable(tmp, rownames = FALSE)
}

.chk <- crayon::green("\u2713")
.exx <- crayon::red("\u2717")
.iv <- crayon::inverse

#' Get SMOCC data from brokenstick, transformed to be hbgd-compatible
#' @export
get_smocc_data <- function() {
  smc <- brokenstick::smocc.hgtwgt
  names(smc)[2] <- "subjid"
  names(smc)[5] <- "agedays"
  smc$sex <- as.character(smc$sex)
  smc$sex[smc$sex == "male"] <- "Male"
  smc$sex[smc$sex == "female"] <- "Female"
  names(smc)[10] <- "htcm"
  names(smc)[11] <- "wtkg"
  names(smc)[12] <- "haz"
  smc$waz <- who_wtkg2zscore(smc$agedays, smc$wtkg, smc$sex)
  smc$agedays <- smc$agedays * 365.25
  smc
}

get_closest_variables <- function(x, nms, length = 2, method = "jaccard") {
  dst <- stringdist::stringdist(x, nms, method = method)
  paste0(
    crayon::bold(head(nms[order(dst)], length)), " (", head(order(dst), length), ")",
    collapse = ", "
  )
}

# get_closest_variables("subjid", names(smc))

# dists <- c("osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
# res <- do.call(rbind, lapply(dists, function(dist) {
#   data.frame(
#     dist = stringdist("subjid", names(smc), method = dist),
#     name = dist, var = names(smc))
# }))
# xyplot(dist ~ var | name, data = res,
#   scales = list(y = list(relation = "free"), x = list(rot = 90)),
#   layout = c(9, 1))

check_variable <- function(nm, nms, req = TRUE) {
  message("Checking for variable '", nm, "'... ", appendLF = FALSE)
  if (!nm %in% nms) {
    if (req) {
      message(.exx)
    } else {
      message("")
    }
    close <- get_closest_variables(nm, nms)
    message(.iv(paste0("  Variable '", nm, "' was not found in the data. ")))
    message(.iv("  Closest matches (with index):", close, " "))
    message(.iv(crayon::bold("  Definition:"), hbgd::hbgd_labels[[nm]], " "))
    if (req) {
      message(.iv("  This variable is required. "))
      message(.iv("  Please create or rename the appropriate variable. "))
      message(.iv("  To rename, choose the appropriate index i and: "))
      message(.iv(paste0("  names(dat)[i] <- '", nm, "'")))
    } else {
      message(.iv("  This variable is not required but if it exists in the data "))
      message(.iv(paste0("  under a different name, please rename it to '", nm, "'. ")))
    }
    return(FALSE)
  } else {
    message(.chk)
    return(TRUE)
  }
}

check_zscore_var <- function(cand, varname, zvarname, varlab, nms) {
  if (any(cand %in% nms)) {
    message(paste0("Checking z-score variable '", zvarname, "' for ", varlab,
      "... "), appendLF = FALSE)
    if (zvarname %in% nms) {
      message(.chk)
    } else {
      message("")
      message(.iv(paste0("  Could not find ", varlab, " z-score variable '", zvarname, "'.")))
      message(.iv(paste0("  If it exists, rename it to '", zvarname, "'.")))
      message(.iv(paste0("  If it doesn't exist, create it with:")))
      message(.iv(paste0(
       "  dat$", zvarname, " <- who_", varname, "2zscore(dat$agedays, dat$", varname, ", dat$sex)"
      )))
    }
  }
}
