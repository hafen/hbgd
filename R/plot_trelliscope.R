#' Create Trelliscope display of per-subject data and fitted trajectories
#'
#' @param dat either a data frame or object created by \code{\link{by_subject}} or \code{\link{fit_all_trajectories}}
#' @param z should the trajectory fit be plotted on the z-scale?
#' @param center should the trajectory be centered around the median WHO standard?
#' @param x_range a vector specifying the range (min, max) that the superposed WHO growth standard should span on the x-axis
#' @param width width of the plot
#' @param height height of the plot
#' @param name name of the Trelliscope display (if left NULL, will be implied by the variables being plotted and the method name)
#' @param desc description of the Trelliscope display
#' @param group group in which to place the Trelliscope display
#' @param vdbConn an optional VDB connection
#' @examples
#' \dontrun{
#' cppsubj <- by_subject(cpp)
#' cppfit  <- get_fit(cpp, method = "rlm")
#' cpptr   <- fit_all_trajectories(cppsubj, cppfit)
#' cppplot <- trscope_trajectories(cpptr)
#' }
#' @export
trscope_trajectories <- function(dat, z = FALSE,
  center = FALSE, x_range = NULL, width = 500, height = 520,
  name = NULL, desc = "", group = NULL,
  vdbConn = getOption("vdbConn")) {

  dat <- get_trscope_dat(dat)

  if(is.null(name)) {
    suffix <- "trajectory"
    if(z) {
      suffix <- paste0("z", suffix)
    } else if(center) {
      suffix <- paste0(suffix, "_centered")
    }
    name <- get_trscope_name(dat[[1]]$value, suffix)
  }

  if(is.null(group))
    group <- "common"

  if(is.null(getOption("vdbConn"))) {
    message("* This function requires a vdb connection to be initialized")
    message("  (see ?trelliscope::vdbConn)")
    message("* You can exit and set up your own connection")
    message("  or answer yes to the prompt below to set up a temporary connection.")
    temp_vdb_conn()
  }

  ## get range of x-axis if not supplied
  if(is.null(x_range)) {
    message("Range for x-axis not supplied... computing from all the data...")
    x_range <- get_x_range(dat)
  }

  if(z) {
    panel_fn <- function(x)
      suppressMessages(plot_z(x, x_range = x_range,
        width = width, height = height))
  } else {
    panel_fn <- function(x)
      suppressMessages(plot(x, center = center, x_range = x_range,
        width = width, height = height))
  }

  subj_meta <- get_subj_meta(dat)
  get_subj_cogs <- get_subj_cogs
  get_cp_cogs <- get_cp_cogs
  get_nadir_cogs <- get_nadir_cogs

  cog_fn <- function(x) {

    if(is.null(x$resid)) {
      n_out <- NA
    } else {
      n_out <- length(which(abs(x$resid) > (5 * mad(x$resid))))
    }

    c(
      list(
        n_obs = cog(nrow(x$data), desc = paste("number of non-NA measurements for", x$y_var, "vs.", x$x_var), type = "integer"),
        n_out = cog(n_out, desc = "number of outlier points with respect to the fit", type = "integer")
      ),
      get_cp_cogs(x),
      get_nadir_cogs(x),
      get_subj_cogs(x, subj_meta)
    )
  }

  res <- makeDisplay(
    name = name, desc = desc, group = group,
    width = width, height = height,
    dat, panelFn = panel_fn, cogFn = cog_fn)

  message("To view this display in the future, type the following:\n")
  message("view(name = '", name, "', group = '", group, "')")

  invisible(res)
}

# trscope_resid <- function() {

# }

#' Create Trelliscope display of the velocities of per-subject fitted trajectories
#'
#' @param dat either a data frame or object created by \code{\link{by_subject}} or \code{\link{fit_all_trajectories}}
#' @param z should velocities according to z-score scale or raw scale be plotted?
#' @param x_range a vector specifying the range (min, max) that the x-axis should span
#' @param width width of the plot
#' @param height height of the plot
#' @param name name of the Trelliscope display (if NULL, will be implied by the variables being plotted and the method name)
#' @param desc description of the Trelliscope display
#' @param group group in which to place the Trelliscope display
#' @param vdbConn an optional VDB connection
#' @examples
#' \dontrun{
#' cppsubj <- by_subject(cpp)
#' cppfit  <- get_fit(cpp, method = "rlm")
#' cpptr   <- fit_all_trajectories(cppsubj, cppfit)
#' cppplot <- trscope_velocities(cpptr)
#' }
#' @export
trscope_velocities <- function(dat, z = FALSE,
  x_range = NULL, width = 500, height = 520,
  name = NULL, desc = "", group = NULL,
  vdbConn = getOption("vdbConn")) {

  dat <- get_trscope_dat(dat)

  if(is.null(name)) {
    suffix <- ifelse(z, "_zvelocity", "_velocity")
    name <- get_trscope_name(dat[[1]]$value, suffix)
  }

  if(is.null(group))
    group <- "common"

  if(is.null(getOption("vdbConn"))) {
    message("* This function requires a vdb connection to be initialized")
    message("  (see ?trelliscope::vdbConn)")
    message("* You can exit and set up your own connection")
    message("  or answer yes to the prompt below to set up a temporary connection.")
    temp_vdb_conn()
  }

  ## get range of x-axis if not supplied
  if(is.null(x_range)) {
    message("Range for x-axis not supplied... computing from all the data...")
    x_range <- get_x_range(dat)
  }

  if(z) {
    panel_fn <- function(x)
      suppressMessages(plot_zvelocity(x,
        width = width, height = height, xlim = x_range))
  } else {
    panel_fn <- function(x)
      suppressMessages(plot_velocity(x,
        width = width, height = height, xlim = x_range))
  }

  subj_meta <- get_subj_meta(dat)
  get_subj_cogs <- get_subj_cogs
  get_cp_cogs <- get_cp_cogs
  get_nadir_cogs <- get_nadir_cogs

  cog_fn <- function(x) {

    if(is.null(x$resid)) {
      n_out <- NA
    } else {
      n_out <- length(which(abs(x$resid) > (5 * mad(x$resid))))
    }

    c(
      list(
        n_obs = cog(nrow(x$data), desc = paste("number of non-NA measurements for", x$y_var, "vs.", x$x_var), type = "integer"),
        n_out = cog(n_out, desc = "number of outlier points with respect to the fit", type = "integer")
      ),
      get_cp_cogs(x),
      get_nadir_cogs(x),
      get_subj_cogs(x, subj_meta)
    )
  }

  res <- makeDisplay(
    name = name, desc = desc, group = group,
    width = width, height = height,
    dat, panelFn = panel_fn, cogFn = cog_fn)

  message("To view this display in the future, type the following:\n")
  message("view(name = '", name, "', group = '", group, "')")

  invisible(res)
}

#' Get the x-axis range across all subjects
#'
#' @param dat object obtained from \code{\link{fit_all_trajectories}}
#' @param pad padding factor - this much space as a multiple of the span of the x range will be added to the min and max
#' @export
get_x_range <- function(dat, pad = 0.07) {
  if(!inherits(dat, "ddo") || !inherits(dat[[1]]$value, "fittedTrajectory"))
    stop("dat must be an output of fit_all_trajectories()")

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

  rng <- c(min(rng$min, na.rm = TRUE), max(rng$max, na.rm = TRUE))
  rng + c(-1, 1) * pad * diff(rng)
}

get_subj_meta <- function(dat) {
  var_summ <- attr(dat, "hbgd")$var_summ
  subjvars <- var_summ$variable[var_summ$type == "subject-level"]
  subjlabs <- as.list(var_summ$label[var_summ$type == "subject-level"])
  names(subjlabs) <- subjvars
  subjtypes <- var_summ$vtype[var_summ$type == "subject-level"]
  subjtypes[subjtypes == "num"] <- "numeric"
  subjtypes[subjtypes == "cat"] <- "factor"
  subjtypes <- as.list(subjtypes)
  names(subjtypes) <- subjvars
  list(
    vars = subjvars,
    labs = subjlabs,
    types = subjtypes
  )
}

# get subject-level cognostics
get_subj_cogs <- function(x, subj) {
  subjdat <- as.list(x$data[1, subj$vars])
  structure(
    lapply(names(subjdat), function(nm)
      cog(subjdat[[nm]], desc = subj$labs[[nm]], type = subj$types[[nm]])),
        names = names(subjdat))
}

get_nadir_cogs <- function(x) {
  nadir <- get_nadir(x)
  list(
    nadir_at = cog(nadir$at, desc = "age at nadir", type = "numeric"),
    nadir_mag = cog(nadir$mag, desc = "magnitude of nadir", type = "numeric")
  )
}

get_cp_cogs <- function(x) {
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

  c(cpzcogs, cpzcatcogs)
}

get_trscope_name <- function(dat, suffix = "") {
  paste(dat$y_var, "vs", dat$x_var, dat$method, suffix, sep = "_")
}

get_trscope_dat <- function(dat) {
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

  dat
}

temp_vdb_conn <- function() {
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
