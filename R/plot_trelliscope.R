


# center <- FALSE
# who_range <- NULL
# width <- 500
# height <- 520
# name <- "trajectory_plot"
# group <- NULL
# vdbConn <- getOption("vdbConn")
# view <- TRUE

#' Create Trelliscope display of subject trajectories
#'
#' @param dat either a data frame or object created by \code{\link{by_subject}} or \code{\link{fit_all_trajectories}}
#' @param center should the trajectory be centered around the median WHO standard?
#' @param who_range a vector specifying the range (min, max) that the superposed WHO growth standard should span on the x-axis
#' @param width width of the plot
#' @param height height of the plot
#' @param name name of the Trelliscope display (if left NULL, will be implied by the vairables being plotted)
#' @param desc description of the Trelliscope display
#' @param group group in which to place the Trelliscope display
#' @param vdbConn an optional VDB connection
#' @param view should the plot be viewed upon creation?
#' @examples
#' \donttest{
#'   cppsubj <- by_subject(cpp)
#'   cppt <- fit_all_trajectories(cppsubj, method = "rlm")
#'   cppt <- trscope_trajectories(cppt)
#' }
#' @export
trscope_trajectories <- function(dat,
  center = FALSE, who_range = NULL, width = 500, height = 520,
  name = NULL, desc = "", group = NULL,
  vdbConn = getOption("vdbConn"), view = TRUE) {

  if(inherits(dat, "data.frame")) {
    message("* Dividing data by subject...")
    message("  Note that you may wish to do this with by_subject() prior to calling this function and pass the result in.")
    dat <- by_subject(dat)
  }

  # should be split by subject id
  check_ddo(dat)
  check_subj_split(dat)

  # if trajectories haven't been computed yet, we need to do it
  if(!inherits(dat[[1]]$value, "fittedTrajectory")) {
    check_ddf(dat)
    message("* Fitting trajectories with default parameters...")
    message("  Note that you can pre-compute trajectories with finer control using fit_all_trajectories() and passing the result of that in.")
    dat <- fit_all_trajectories(dat)
  }

  if(is.null(name)) {
    name <- paste0(dat[[1]]$value$y_var, "_vs_", dat[[1]]$value$x_var)
    if(center)
      name <- paste0(name, "_centered")
  }

  if(is.null(group))
    group <- "common"

  if(is.null(getOption("vdbConn"))) {
    message("* This function requires a vdb connection to be initialized")
    message("  (see ?trelliscope::vdbConn)")
    message("* You can exit and set up your own connection")
    message("  or answer yes to the prompt below to set up a temporary connection.")
    tempVdbConn()
  }

  ## get range of x-axis if not supplied
  if(is.null(who_range)) {
    message("Range for WHO growth standard not supplied... computing from all the data...")

    rngdat <- dat %>% addTransform(function(x) {
      tmp <- x$xy$x[!is.na(x$xy$x)]
      if(length(tmp) == 0) {
        rng <- c(NA, NA)
      } else {
        rng <- range(x$xy$x)
      }
      data.frame(min = rng[1], max = rng[2])
    })
    rng <- recombine(rngdat, combRbind)
    who_range <- c(min(rng$min, na.rm = TRUE), max(rng$max, na.rm = TRUE))
  }

  panel_fn <- function(x)
    suppressMessages(plot(x, center = center, who_range = who_range,
      width = width, height = height))

  var_summ <- attr(dat, "hbgd")$var_summ
  subjvars <- var_summ$variable[var_summ$type == "subject-level"]
  subjlabs <- as.list(var_summ$label[var_summ$type == "subject-level"])
  names(subjlabs) <- subjvars
  subjtypes <- var_summ$vtype[var_summ$type == "subject-level"]
  subjtypes[subjtypes == "num"] <- "numeric"
  subjtypes[subjtypes == "cat"] <- "factor"
  subjtypes <- as.list(subjtypes)
  names(subjtypes) <- subjvars

  cog_fn <- function(x) {

    ## get subject-level cognostics
    subjdat <- as.list(x$data[1, subjvars])
    subjcogs <- structure(lapply(names(subjdat), function(nm)
      cog(subjdat[[nm]], desc = subjlabs[[nm]], type = subjtypes[[nm]])), names = names(subjdat))

    ## get checkpoint cognostics
    if(is.null(x$checkpoint$z))
      x$checkpoint$z <- NA
    if(is.null(x$checkpoint$zcat))
      x$checkpoint$zcat <- NA

    x$checkpoint$z <- round(x$checkpoint$z, 2)
    cpzcogs <- lapply(seq_len(nrow(x$checkpoint)), function(ii) {
      cog(x$checkpoint$z[ii], desc = paste("z-score of", x$y_var, "at", x$checkpoints$x[ii], "days"), type = "numeric")
    })
    names(cpzcogs) <- paste0("z_day", round(x$checkpoint$x, 0))

    cpzcatcogs <- lapply(seq_len(nrow(x$checkpoint)), function(ii) {
      cog(x$checkpoint$zcat[ii], desc = paste("z-score category of", x$y_var, "at", x$checkpoints$x[ii], "days"), type = "factor")
    })
    names(cpzcatcogs) <- paste0("zc_day", round(x$checkpoint$x, 0))

    if(is.null(x$resid)) {
      n_out <- NA
    } else {
      n_out <- length(which(abs(x$resid) > (5 * mad(x$resid))))
    }

    c(subjcogs,
      cpzcogs,
      cpzcatcogs,
      list(
        n_obs = cog(nrow(x$data), desc = paste("number of non-NA measurements for", x$y_var, "vs.", x$x_var), type = "integer"),
        n_out = cog(n_out, desc = "number of outlier points with respect to the fit", type = "integer")
      )
    )
  }

  makeDisplay(
    name = name, desc = desc, group = group,
    width = width, height = height,
    dat, panelFn = panel_fn, cogFn = cog_fn)

  if(view) {
    view(name = name, group = group)
  } else {
    message("To view this display, type the following:\n")
    message("view(name = '", name, "', group = '", group, "')")
  }
}

# trscope_resid <- function() {

# }

# trscope_


tempVdbConn <- function() {
  loc <- tempfile(fileext = "", pattern = "vdb_")
  vdbConn(name = "ghap", path = loc)
}

check_ddo <- function(dat) {
  if(!is_ddo(dat))
    stop("Argument 'dat' must be a 'distributed data object'.", call. = FALSE)
}

check_ddf <- function(dat) {
  if(!is_ddf(dat))
    stop("Argument 'dat' must be a 'distributed data frame' object.", call. = FALSE)
}

check_subj_split <- function(dat) {
  if(!is_subj_split(dat))
    stop("Argument 'dat' must be a ddo/ddf split by 'subjid' - use by_subject() to get data in this form.")
}

is_ddo <- function(dat)
  inherits(dat, "ddo")

is_ddf <- function(dat)
  inherits(dat, "ddf")

is_subj_split <- function(dat) {
  sv <- names(getSplitVars(dat[[1]]))
  length(sv) == 1 && sv[1] == "subjid"
}
