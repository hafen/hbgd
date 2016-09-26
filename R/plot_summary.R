
#' Make a grid of univariate summary plots
#'
#' @param dat data frame
#' @param ncol number of columns in the grid
#' @param width width of each plot in pixels
#' @param height height of each plot in pixels
#' @template par-subject
#' @examples
#' plot_univar(cpp, subject = TRUE)
#' plot_univar(cpp)
#' @export
plot_univar <- function(dat, subject = FALSE, ncol = 3, width = 300, height = 300) {

  # subset columns to subject-level or non-subject-level
  if (subject) {
    dat <- get_subject_data(dat)
  } else {
    dat <- get_time_data(dat)
  }
  if (is.null(nrow(dat)))
    return(NULL)

  var_summ <- attr(dat, "hbgd")$var_summ

  # remove columns that are all NA
  idx <- sapply(dat, function(x) !all(is.na(x)))
  dat <- dat[, idx]
  var_summ <- var_summ[idx, ]
  if (ncol(dat) == 0)
    return(NULL)

  nn <- nrow(var_summ)
  # count <- rep(1, nn)

  res <- lapply(seq_len(nn), function(ii) {
    if (var_summ$vtype[ii] == "num") {
      figure(xlab = var_summ$label[ii],
        width = width, height = height, logo = NULL) %>%
        ly_hist(dat[[var_summ$variable[ii]]]) %>%
        theme_axis(major_label_text_font_size = "8pt")
    } else {
      x <- as.character(dat[[var_summ$variable[ii]]])
      ind <- which(nchar(x) > 15)
      x[ind] <- paste0(substr(x[ind], 1, 15), "...")
      figure(xlab = var_summ$label[ii],
        width = width, height = height, logo = NULL) %>%
        ly_bar(x, color = pal_tableau("Tableau10")(2)[2]) %>%
        theme_axis("x", major_label_orientation = 90,
          major_label_text_font_size = "8pt") %>%
        theme_axis("y", major_label_text_font_size = "8pt")
    }
  })
  names(res) <- var_summ$variable
  grid_plot(res, ncol = ncol)
}

# plot_timevars <- function(dat, ) {

# }

#' Plot a stacked bar chart indicating NAs for each variable in a dataset
#'
#' @param dat data frame
#' @param width width of plot in pixels
#' @param height height of plot in pixels
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @template par-subject
#' @examples
#' plot_missing(cpp)
#' plot_missing(cpp, subject = TRUE)
#' @export
plot_missing <- function(dat, subject = FALSE, width = 800, height = 500, ...) {

  # subset columns to subject-level or non-subject-level
  if (subject) {
    dat <- get_subject_data(dat)
  } else {
    dat <- get_time_data(dat)
  }
  if (is.null(nrow(dat)))
    return(NULL)

  if (subject) {
    xlab <- "subject-level variables"
  } else {
    xlab <- "time-varying variables"
  }

  na_tab <- lapply(dat, function(x) length(which(is.na(x))))
  na_tab <- data.frame(var = names(na_tab), count = unname(unlist(na_tab)))
  nna_tab <- na_tab
  nna_tab$count <- nrow(dat) - nna_tab$count
  na_tab$type <- "NA"
  nna_tab$type <- "non-NA"
  tab <- rbind(na_tab, nna_tab)

  figure(width = width, height = height, xlab = xlab, logo = NULL, ...) %>%
    ly_bar(var, count, color = type, data = tab, width = 1) %>%
    theme_axis("x", major_label_orientation = 90) %>%
    theme_grid("x", grid_line_alpha = 0.3)
}

#' Plot a heat map of frequency of "complete" (both non-NA) pairs of variables
#'
#' @param dat data frame
#' @param width width of plot in pixels
#' @param height height of plot in pixels
#' @param thresh percentage NA threshold above which variables will be ignored (to help deal with cases involving many variables)
#' @param \ldots additional parameters passed to \code{\link{figure}}
#' @template par-subject
#' @examples
#' plot_complete_pairs(cpp)
#' plot_complete_pairs(cpp, subject = TRUE)
#' @export
plot_complete_pairs <- function(
  dat,
  subject = FALSE,
  width = 700, height = 700,
  thresh = 0.95,
  ...
) {

  # subset columns to subject-level or non-subject-level
  if (subject) {
    dat <- get_subject_data(dat)
  } else {
    dat <- get_time_data(dat)
  }
  if (is.null(nrow(dat)))
    return(NULL)

  nna_mat <- !is.na(dat)
  na_col <- apply(nna_mat, 2, function(x) length(which(!x)))
  na_pct <- na_col / nrow(nna_mat)
  ind <- na_pct < 0.95

  if (length(which(ind)) == 0) {
    message("Not enough non-NA columns to plot complete pairs heat map...")
    return(NULL)
  }

  nna_mat <- nna_mat[, ind]
  dat <- dat[, ind]
  nn <- ncol(nna_mat)
  if (nn > 75) {
    message("Too many columns in the data to plot complete pairs heat map...")
    return(NULL)
  }

  combns <- t(combn(nn, 2))
  res <- matrix(nrow = nn, ncol = nn)
  for (rr in seq_len(nrow(combns))) {
    ii <- combns[rr, 1]
    jj <- combns[rr, 2]
    res[ii, jj] <- res[jj, ii] <- length(which(nna_mat[, ii] & nna_mat[, jj]))
  }
  diag(res) <- sapply(dat, function(x) length(which(!is.na(x))))

  rownames(res) <- names(dat)
  colnames(res) <- names(dat)
  res <- as.data.frame(as.table(res))
  names(res)[3] <- "CompleteCases"
  res$Var1h <- res$Var1
  res$Var2h <- res$Var2
  levels(res$Var1) <- add_labels(levels(res$Var1))
  levels(res$Var2) <- add_labels(levels(res$Var2))

  pal <- rbokeh:::bk_gradient_palettes$YlOrRd9 # nolint
  res$col <- colorRampPalette(pal)(1000)[
    ceiling(res$CompleteCases / max(res$CompleteCases) * 999) + 1 # nolint
  ]

  figure(width = 700, height = 700,
    xlab = "Var1", ylab = "Var2", logo = NULL, ...) %>%
    ly_crect(Var1h, Var2h, color = col, data = res,
      line_alpha = 0, fill_alpha = 0.65,
      hover = c(Var1, Var2, CompleteCases)) %>% # nolint
    theme_axis("x", major_label_orientation = 90) %>%
    theme_grid(grid_line_alpha = 0.3)
}

#' Plot histogram and quantile plot of number of "visits" for each subject
#'
#' @param dat a longitudinal growth study dataset
#' @param width the width of each plot in pixels
#' @param height the height of each plot in pixels
#' @examples
#' \donttest{
#' plot_visit_distn(cpp)
#' }
#' @export
plot_visit_distn <- function(dat, width = 450, height = 450) {

  p1 <- figure(ylab = "count", xlab = "# visits",
    width = width, height = height, logo = NULL) %>%
    ly_hist(table(dat$subjid))
  p2 <- figure(ylab = "# visits", xlab = "proportion",
    width = width, height = height, logo = NULL) %>%
    ly_quantile(table(dat$subjid), glyph = 1)

  grid_plot(list(p1, p2), nrow = 1)
}

#' Plot histogram and quantile plot of age at first visit
#'
#' @param dat a longitudinal growth study dataset
#' @param agelab label of the age axis
#' @param width the width of each plot in pixels
#' @param height the height of each plot in pixels
#' @export
#' @examples
#' \donttest{
#' plot_first_visit_age(cpp)
#' }
plot_first_visit_age <- function(dat,
  agelab = "first visit age (days)", width = 450, height = 450) {

  first_visit_age <- dat %>%
    group_by(subjid) %>%
    summarise(day = min(agedays), n = n())

  p1 <- figure(ylab = "count", xlab = agelab,
    width = width, height = height, logo = NULL) %>%
    ly_hist(day, data = first_visit_age)
  p2 <- figure(ylab = agelab, xlab = "proportion of subjects",
    width = width, height = height, logo = NULL) %>%
    ly_quantile(day, data = first_visit_age)

  grid_plot(list(p1, p2), nrow = 1)
}

#' Get age frequency
#'
#' @param dat a longitudinal growth study dataset
#' @param age_range optional range to ....
#' @export
#' @examples
#' \donttest{
#' agefreq <- get_agefreq(cpp)
#' plot_agefreq(agefreq)
#' }
get_agefreq <- function(dat, age_range = NULL) {

  agefreq <- dat %>%
    group_by(agedays) %>%
    summarise(freq = n())
  names(agefreq)[1] <- "timeunits"

  if (is.null(age_range))
    age_range <- range(agefreq$timeunits, na.rm = TRUE)

  empty_timeunits <- setdiff(c(age_range[1]:age_range[2]), agefreq$timeunits)
  if (length(empty_timeunits) > 0) {
    agefreq <- rbind(agefreq, data.frame(timeunits = empty_timeunits, freq = 0))
    agefreq <- agefreq[order(agefreq$timeunits), ]
  }

  structure(data.frame(agefreq), class = c("data.frame", "agefreq"))
}

#' Plot age frequency
#'
#' @param x a data frame of raw data or an object returned from \code{\link{get_agefreq}}
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param width width of plot in pixels
#' @param height height of plot in pixels
#' @param age_units units of age x-axis (days, months, or years)
#' @export
#' @examples
#' \donttest{
#' agefreq <- get_agefreq(cpp)
#' plot_agefreq(agefreq)
#' }
plot_agefreq <- function(x, xlab = "Age since birth at examination (days)",
  ylab = "# examinations", width = 700, height = 350,
  age_units = c("days", "months", "years")) {

  age_units <- match.arg(age_units)
  age_denom <- switch(age_units,
    days = 1,
    months = 365.25 / 12,
    years = 365.25)

  if (age_units == "months")
    xlab <- gsub("\\(days\\)", "(months)", xlab)
  if (age_units == "years")
    xlab <- gsub("\\(days\\)", "(years)", xlab)

  if (!inherits(x, "agefreq"))
    x <- get_agefreq(x)

  figure(width = width, height = height,
    xlab = xlab, ylab = ylab, logo = NULL) %>%
    ly_lines(timeunits / age_denom, freq, data = x, color = NULL)
}


#' Get subject-level or time-varying variables and rows of longitudinal data
#'
#' @param dat data frame with longitudinal data
#' @rdname subjecttime
#' @export
get_subject_data <- function(dat) {
  if (!has_data_attributes(dat))
    dat <- get_data_attributes(dat)

  var_summ <- attr(dat, "hbgd")$var_summ
  subj_vars <- c("subject-level", "subject id")
  ind <- which(var_summ$type %in% subj_vars)
  if (length(ind) == 0)
    return(NULL)

  dat <- dat[!duplicated(dat$subjid), var_summ$variable[ind]]

  var_summ <- subset(var_summ, variable %in% names(dat))
  attr(dat, "hbgd")$var_summ <- var_summ
  dat
}

#' @rdname subjecttime
#' @export
get_time_data <- function(dat) {
  if (!has_data_attributes(dat))
    dat <- get_data_attributes(dat)

  var_summ <- attr(dat, "hbgd")$var_summ
  subj_vars <- c("subject-level", "subject id")
  ind <- which(!var_summ$type %in% subj_vars)
  if (length(ind) == 0)
    return(NULL)
  dat <- dat[, var_summ$variable[ind]]

  var_summ <- subset(var_summ, variable %in% names(dat))
  attr(dat, "hbgd")$var_summ <- var_summ
  dat
}
