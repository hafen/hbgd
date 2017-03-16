
# library(dplyr)
# library(trelliscopejs)

# cpph <- add_holdout_ind(cpp)
# cppfit <- get_fit(cpph, method = "rlm", holdout = TRUE)
# cppsubj <- by_subject(cpph)
# dat <- fit_all_trajectories(cppsubj, cppfit)
# get_fit_holdout_errors(dat)
# get_fit_holdout_mse(dat)

# dat <- dat %>%
#   add_all_cogs() %>%
#   add_trajectory_plot() %>%
#   trelliscope(name = "test")



# pb <- progress::progress_bar$new(
#   format = paste0("adding column '", .to, "' [:bar] :percent :current/:total eta::eta"),
#   total = nrow(.d), width = getOption("width") - 8)



#' Add a column to a data frame containing trajectory / velocity plots
#'
#' @param dat a data frame nested by subject and containing fits from \code{\link{fit_all_trajectories}}
#' @param z should the trajectory fit be plotted on the z-scale?
#' @param center should the trajectory be centered around the median WHO standard?
#' @param x_range a vector specifying the range (min, max) that the superposed WHO growth standard should span on the x-axis
#' @param nadir should a guide be added to the plot showing the location of the nadir? (only valid when z = TRUE)
#' @param recovery age in days at which to plot recovery from nadir (only valid when z = TRUE) - if NULL (default), will not be plotted
#' @param x_units units of age x-axis ("days", "months", or "years")
#' @param width width of the plot
#' @param height height of the plot
#' @examples
#' \dontrun{
#' cppsubj  <- by_subject(cpp)
#' cppfit   <- get_fit(cpp, method = "rlm")
#' cpptr    <- fit_all_trajectories(cppsubj, cppfit)
#' cppplot  <- trscope_trajectories(cpptr)
#' cppzplot <- trscope_trajectories(cpptr, z = TRUE, nadir = TRUE, x_units = "months")
#' }
#' @export
add_trajectory_plot <- function(dat, z = FALSE, center = FALSE, y_var = "htcm",
  x_range = NULL, nadir = FALSE, recovery = NULL, x_units = "days",
  width = 500, height = 520) {

  if (is.null(x_range)) {
    message("Range for x-axis not supplied... computing from all the data... ",
      appendLF = FALSE)
    x_range <- get_x_range(dat)
    message("done")
  }

  pb <- progress::progress_bar$new(
    format = "adding panels [:bar] :percent :current/:total eta::eta",
    total = nrow(dat), width = getOption("width") - 8)

  if ("fit" %in% names(dat)) {
    if (z) {
      panel_fn <- function(x) {
        pb$tick()
        suppressMessages(plot_z(x, x_range = x_range,
          width = width, height = height, x_units = x_units,
          nadir = nadir, recovery = recovery))
      }
    } else {
      panel_fn <- function(x) {
        pb$tick()
        suppressMessages(plot(x, center = center, x_range = x_range,
          width = width, height = height, x_units = x_units))
      }
    }
    dat$panel <- trelliscopejs::map_plot(dat$fit, panel_fn)
  } else {
    panel_fn <- function(x) {
      pb$tick()
      suppressMessages(plot(dat, subjid = x, y_var = y_var, center = center,
        x_range = x_range, width = width, height = height))
    }
    dat$panel <- trelliscopejs::map_plot(dat$subjid, panel_fn)
  }

  dat
}

#' @rdname add_trajectory_plot
#' @export
add_velocity_plot <- function(dat, z = FALSE, center = FALSE,
  x_range = NULL, nadir = FALSE, recovery = NULL, x_units = "days",
  width = 500, height = 520) {

  if (is.null(x_range)) {
    message("Range for x-axis not supplied... computing from all the data... ",
      appendLF = FALSE)
    x_range <- get_x_range(dat)
    message("done")
  }

  pb <- progress::progress_bar$new(
    format = "adding panels [:bar] :percent :current/:total eta::eta",
    total = nrow(dat), width = getOption("width") - 8)

  if (z) {
    panel_fn <- function(x) {
      pb$tick()
      suppressMessages(plot_zvelocity(x,
        width = width, height = height, xlim = x_range, x_units = x_units))
    }
  } else {
    panel_fn <- function(x) {
      pb$tick()
      suppressMessages(plot_velocity(x,
        width = width, height = height, xlim = x_range, x_units = x_units))
    }
  }

  dat$panel <- trelliscopejs::map_plot(dat$fit, panel_fn)
  dat
}

# trscope_resid <- function() {

# }


#' Get the x-axis range across all subjects
#'
#' @param dat object obtained from \code{\link{fit_all_trajectories}}
#' @param pad padding factor - this much space as a multiple of the span of the x range will be added to the min and max
#' @export
get_x_range <- function(dat, pad = 0.07) {
  if ("fit" %in% names(dat)) {
    rngdat <- dat$fit %>%
      purrr::map_df(function(x) {
        rng <- c(NA, NA)
        if (!is.null(x$xy)) {
          tmp <- x$xy$x[!is.na(x$xy$x)]
          if (length(tmp) > 0)
            rng <- range(x$xy$x)
        }
        dplyr::data_frame(min = rng[1], max = rng[2])
      }) %>%
      dplyr::bind_rows()
    rng <- c(min(rngdat$min, na.rm = TRUE), max(rngdat$max, na.rm = TRUE))
  } else {
    rng <- c(
      min(purrr::map_dbl(dat$longi, ~ min(.x$agedays, na.rm = TRUE)), na.rm = TRUE),
      max(purrr::map_dbl(dat$longi, ~ max(.x$agedays, na.rm = TRUE)), na.rm = TRUE)
    )
  }
  rng + c(-1, 1) * pad * diff(rng)
}


# subj_meta <- get_subj_meta(dat)
# get_subj_cogs <- get_subj_cogs
# get_cp_cogs <- get_cp_cogs
# get_nadir_cogs <- get_nadir_cogs


#' Add a column to a data frame containing cognostics for fitted trajectories and set longitudinal cognostics
#'
#' @param dat a data frame nested by subject and containing fits from \code{\link{fit_all_trajectories}}
#' @param x_units units of age variable ("days", "months", or "years")
#' @param recovery age in days at which to plot recovery from nadir
#' @export
add_all_cogs <- function(dat, x_units = "days", recovery = NULL) {
  if ("longi" %in% names(dat)) {
    dat <- add_longi_cogs(dat)
  }
  if ("fit" %in% names(dat)) {
    dat <- add_fit_cogs(dat, x_units = x_units, recovery = recovery)
  }
  dat
}

#' Set longitudinal cognostics for subject-nested data frames
#'
#' @param dat a data frame nested by subject and containing fits from
#' @export
add_longi_cogs <- function(dat) {
  if (! "longi" %in% names(dat))
    stop("Input to add_trajectory_cogs() must have a column 'longi'.", call. = FALSE)

  # make subject-level variables have nice cog descriptions
  subj_meta <- get_subj_meta(dat)
  for (nm in subj_meta$vars)
    dat[[nm]] <- trelliscopejs::cog(dat[[nm]],
      desc = subj_meta$labs[[nm]],
      type = subj_meta$types[[nm]])

  # add min and max haz and waz (if present)
  if ("haz" %in% names(dat$longi[[1]])) {
    dat$min_haz <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        min(lng$haz, na.rm = TRUE)
      }),
      desc = "lowest observed HAZ",
      type = "numeric")

    dat$min_haz_age <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        lng$agedays[which.min(lng$haz)]
      }),
      desc = "age at lowest observed HAZ",
      type = "numeric")

    dat$max_haz <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        max(lng$haz, na.rm = TRUE)
      }),
      desc = "highest observed HAZ",
      type = "numeric")

    dat$max_haz_age <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        lng$agedays[which.max(lng$haz)]
      }),
      desc = "age at highest observed HAZ",
      type = "numeric")
  }

  if ("waz" %in% names(dat$longi[[1]])) {
    dat$min_waz <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        min(lng$waz, na.rm = TRUE)
      }),
      desc = "lowest observed WAZ",
      type = "numeric")

    dat$min_waz_age <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        lng$agedays[which.min(lng$waz)]
      }),
      desc = "age at lowest observed WAZ",
      type = "numeric")

    dat$max_waz <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        max(lng$waz, na.rm = TRUE)
      }),
      desc = "highest observed WAZ",
      type = "numeric")

    dat$max_waz_age <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) {
        lng$agedays[which.max(lng$waz)]
      }),
      desc = "age at highest observed WAZ",
      type = "numeric")
  }

  get_haz_for_age <- function(lng, days) {
    tmp <- lng %>%
      filter(!is.na(haz)) %>%
      mutate(diff = abs(agedays - days)) %>%
      filter(diff < 30) %>%
      arrange(diff)

    if (nrow(tmp) == 0) {
      res <- NA
    } else {
      res <- tmp$haz[1]
    }
    res
  }

  # add haz at closest to 6, 12, 24 months - if within 30 days
  if ("haz" %in% names(dat$longi[[1]])) {
    dat$haz_1day <- trelliscopejs::cog(
      map_dbl(dat$longi, function(lng) get_haz_for_age(lng, 1)),
      desc = "Observed HAZ closest to 1 day",
      type = "numeric")

    for (mo in c(6, 12, 24)) {
      dat[[sprintf("haz_%dmonths", mo)]] <- trelliscopejs::cog(
        map_dbl(dat$longi, function(lng) get_haz_for_age(lng, months2days(mo))),
        desc = sprintf("Observed HAZ closest to %d months", mo),
        type = "numeric")
    }
  }

  dat
}

#' Add a column to a data frame containing cognostics for fitted trajectories
#'
#' @param dat a data frame nested by subject and containing fits from \code{\link{fit_all_trajectories}}
#' @param x_units units of age variable ("days", "months", or "years")
#' @param recovery age in days at which to plot recovery from nadir
#' @export
add_fit_cogs <- function(dat, x_units = "days", recovery = NULL) {
  if (! "fit" %in% names(dat) || !inherits(dat$fit[[1]], "fittedTrajectory"))
    stop("Column 'fit' is missing, which is required for these displays.\n",
      "Please make sure you pass in an object obtained from fit_all_trajectories().")

  dat$cog <- trelliscopejs::map2_cog(dat$fit, dat$longi, function(fit, longi) {
    x <- fit
    if (is.null(x$resid)) {
      n_out <- NA
    } else {
      n_out <- length(which(abs(x$resid) > (5 * mad(x$resid))))
    }
    extra <- list(
      n_out = trelliscopejs::cog(
        n_out,
        desc = "number of outlier points with respect to the fit",
        type = "integer"
      ),
      n_obs = trelliscopejs::cog(
        length(which(!is.na(longi[[x$y_var]]))),
        desc = paste("number of non-NA measurements for", x$y_var, "vs.", x$x_var),
        type = "integer"
      )
    )

    cp <- get_cp_cogs(x)
    nd <- get_nadir_cogs(x, recov_at = recovery, x_units)
    do.call(dplyr::data_frame, c(extra, cp, nd))
  })

  dat
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

# # get subject-level cognostics
# get_subj_cogs <- function(x, subj) {
#   subjdat <- as.list(x$data[1, subj$vars])
#   structure(
#     lapply(names(subjdat), function(nm)
#       trelliscopejs::cog(subjdat[[nm]],
#         desc = subj$labs[[nm]], type = subj$types[[nm]])),
#         names = names(subjdat))
# }

get_nadir_cogs <- function(x, recov_at = NULL,
  x_units = c("days", "months", "years")) {

  x_units <- match.arg(x_units)
  x_denom <- switch(x_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  nadir <- get_nadir(x)
  res <- list(
    nadir_at = trelliscopejs::cog(nadir$at / x_denom,
      desc = "age at nadir", type = "numeric"),
    nadir_mag = trelliscopejs::cog(nadir$mag,
      desc = "magnitude of nadir", type = "numeric")
  )
  if (!is.null(recov_at)) {
    recov <- get_recovery(x, nadir, recov_at)
    tmp <- list(
      a = trelliscopejs::cog(recov$recov, desc = paste0("recovery at day ", recov_at)))
    names(tmp)[1] <- paste0("recov_day", recov_at)
    res <- c(res, tmp)
  }
  res
}

get_cp_cogs <- function(x) {
  if (is.null(x$checkpoint$z))
    x$checkpoint$z <- NA
  if (is.null(x$checkpoint$zcat))
    x$checkpoint$zcat <- NA

  x$checkpoint$z <- round(x$checkpoint$z, 2)
  cpzcogs <- lapply(seq_len(nrow(x$checkpoint)), function(ii) {
    trelliscopejs::cog(
      x$checkpoint$z[ii],
      desc = paste("z-score of", x$y_var, "at", x$checkpoints$x[ii], "days"),
      type = "numeric"
    )
  })
  names(cpzcogs) <- paste0("z_day", round(x$checkpoint$x, 0))

  cpzcatcogs <- lapply(seq_len(nrow(x$checkpoint)), function(ii) {
    trelliscopejs::cog(
      x$checkpoint$zcat[ii],
      desc = paste("z-score category of", x$y_var, "at", x$checkpoints$x[ii], "days"),
      type = "factor"
    )
  })
  names(cpzcatcogs) <- paste0("zc_day", round(x$checkpoint$x, 0))

  c(cpzcogs, cpzcatcogs)
}

get_trscope_name <- function(dat, suffix = "") {
  paste(dat$y_var, "vs", dat$x_var, dat$method, suffix, sep = "_")
}

check_by_subject <- function(dat) {
  if (!is_by_subject(dat))
    stop("Argument 'dat' must be a data frame with one row for each 'subjid' - use by_subject() to get data in this form.") # nolint
}

is_by_subject <- function(dat)
  "longi" %in% names(dat) && inherits(dat$longi, "list")
