# dat_list <- list(
#   cpp1 = cpp[,c(1:5, 7:9, 14:19, 23:32)],
#   cpp2 = cpp[,c(1:5, 11:24)],
#   cpp3 = cpp
# )
# dat_list <- lapply(dat_list, function(x) {
#   attr(get_data_attributes(x), "hbgd")$var_summ
# })
# plot_var_matrix(dat_list)

#' Plot a matrix comparing variables present in a list of studies
#'
#' @param dat_list a list of data frames containing study data
#' @param width width of the plot in pixels
#' @param h_padding extra height to add to the plot to account for long variable names
#' @param subjid variable name in \code{dat} that contains the subject's identifier
#' @param agevar variable name in \code{dat} that contains a measure of the subject's age
#' @examples
#' dat_list <- list(
#'   cpp1 = cpp[,c(1:5, 7:9, 14:19, 23:32)],
#'   cpp2 = cpp[,c(1:5, 11:24)],
#'   cpp3 = cpp
#' )
#' plot_var_matrix(dat_list)
#' @export
plot_var_matrix <- function(dat_list, width = 845, h_padding = 0, subjid = "subjid", agevar = "agedays") {

  if(length(names(dat_list)) == 0)
    names(dat_list) <- paste0("data", seq_along(dat_list))

  if(inherits(dat_list[[1]], "var_summ")) {
    # using attributes
  } else {
    # if it doesn't have attributes, get them
    for(ii in seq_along(dat_list)) {
      if(! "var_summ" %in% names(attributes(dat_list[[ii]])))
        tmp <- get_data_attributes(dat_list[[ii]], subjid = subjid, agevars = agevar)
        dat_list[[ii]] <- attr(tmp, "hbgd")$var_summ
    }
  }

  dat_vars <- do.call(rbind, lapply(names(dat_list), function(x) {
    dat_list[[x]]$short_id <- x
    dat_list[[x]]
  }))

  var_tab <- dat_vars %>%
    dplyr::filter(!variable %in% c(subjid, agevar)) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(n = n(), type = type[1], short_id = short_id[1]) %>%
    dplyr::arrange(dplyr::desc(n), dplyr::desc(short_id), type, variable)
  var_order <- as.character(var_tab$variable)
  var_order <- c(subjid, agevar, var_order)

  n_vars <- length(var_order)

  study_tab2 <- var_tab %>%
    dplyr::filter(n == 1) %>%
    dplyr::group_by(short_id) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(dplyr::desc(n))
  study_order <- study_tab2$short_id
  study_order <- c(study_order, setdiff(unique(dat_vars$short_id), study_order))

  n_studies <- length(study_order)

  dat_vars$type[dat_vars$type == "subject-level"] <- " subject-level"
  dat_vars$type[dat_vars$type == "time-varying"] <- " time-varying"

  tmp_order <- var_order
  tmp <- dat_vars
  a <- tmp[1:2,]
  a$short_id <- "1"
  a$variable <- "1"
  a$type <- c("subject id", "time indicator")
  tmp <- rbind(tmp, a)

  height <- max(220, 100 + length(study_order) * 18 + h_padding)

  rbokeh::figure(xlim = tmp_order, ylim = study_order,
    width = width, height = height, tools = NULL,
    xlab = NULL, ylab = "Study Abbreviation") %>%
    rbokeh::ly_crect(variable, short_id, data = tmp, line_alpha = 0.7, fill_alpha = 0.7,
      line_width = 2,
      # hover = list(study, short_id, variable, label, n_unique), color = type,
      # legend = TRUE) %>%
      hover = list(short_id, variable, label, n_unique), color = type) %>%
    rbokeh::theme_axis("x", major_label_orientation = 60, major_label_text_font_size = "10pt",
      axis_label_text_font_size = "13pt") %>%
    rbokeh::tool_pan(dimensions = "width") %>%
    rbokeh::tool_wheel_zoom(dimensions = "width") %>%
    rbokeh::theme_plot(min_border_bottom = h_padding)
}

## plot counts by agedays for each
##---------------------------------------------------------

# add agevar

# dat_list <- list(
#   cpp1 = cpp,
#   cpp2 = cpp,
#   cpp3 = cpp
# )
# dat_list <- lapply(dat_list, function(x) {
#   attr(get_data_attributes(x), "hbgd")$ad_tab
# })
# plot_time_count_grid(dat_list)

#' Plot counts by age for a list of studies
#'
#' @param dat_list a list of data frames containing study data
#' @param agevar variable name in \code{dat} that contains a measure of the subject's age
#' @param xlab label for x axis
#' @param width width of the plot in pixels
#' @param height height of each panel of the plot in pixels
#' @examples
#' dat_list <- list(
#'   cpp1 = cpp,
#'   cpp2 = cpp,
#'   cpp3 = cpp
#' )
#' plot_time_count_grid(dat_list)
#' @export
plot_time_count_grid <- function(dat_list, agevar = "agedays", xlab = "Age since birth at examination (days)", width = 845, height = 80) {

  if(inherits(dat_list[[1]], "ad_tab")) {
    ad_tab <- dat_list
  } else {
    ad_tab <- lapply(dat_list, function(x) {
      # x <- update_var_names(list(agevar = agevar), x)

      x %>%
        dplyr::group_by(agedays) %>%
        dplyr::summarise(n = n())
    })
  }

  last <- tail(names(ad_tab), 1)

  ad_hists <- lapply(names(ad_tab), function(nm) {
    mbb <- 2
    if(nm == last)
      mbb <- 62
    ht <- height + mbb
    p <- rbokeh::figure(xaxes = FALSE, width = width, height = ht,
      min_border_bottom = mbb, min_border_top = 2, min_border_left = 80,
      xlab = xlab, ylab = nm) %>%
      rbokeh::ly_rect((agedays - 0.5), 0, (agedays + 0.5), n, data = ad_tab[[nm]],
        hover = list(agedays, n)) %>%
      rbokeh::tool_pan(dimensions = "width") %>%
      rbokeh::tool_wheel_zoom(dimensions = "width")
    if(nm == last)
      p <- p %>% rbokeh::x_axis()
    p
  })

  rbokeh::grid_plot(ad_hists, ncol = 1, same_axes = c(TRUE, FALSE))
}

#' Plot boxplots of distrubutions of number of records per subject for a list of studies
#'
#' @param dat_list a list of data frames containing study data
#' @param subjid variable name in \code{dat} that contains the subject's identifier
#' @param width width of the plot in pixels
#' @param height height of the plot in pixels
#' @examples
#' dat_list <- list(
#'   cpp1 = cpp,
#'   cpp2 = cpp,
#'   cpp3 = cpp
#' )
#' plot_multi_subj_boxplot(dat_list)
#' @export
plot_multi_subj_boxplot <- function(dat_list, subjid = "subjid", width = 800, height = 500) {
  a <- lapply(names(dat_list), function(nm) {
    x <- update_var_names(list(subjid = subjid), dat_list[[nm]])
    data.frame(table(x$subjid), study = nm)
  })

  b <- do.call(rbind, a)

  std_ord <- names(dat_list)[order(sapply(a, function(x) median(x$Freq)), decreasing = TRUE)]

  rbokeh::figure(xlim = std_ord, ylab = "log base 2 # records for a subject",
    width = width, height = height) %>%
    rbokeh::ly_boxplot(log2(Freq), study, data = b) %>%
    rbokeh::tool_wheel_zoom(dimensions = "height") %>%
    rbokeh::tool_pan(dimensions = "height")
}

