
#' WHO growth standard panel function for lattice
#'
#' @template par-who
#' @importFrom lattice panel.polygon panel.lines
#' @examples
#' library(lattice)
#' xyplot(wtkg ~ agedays, data = subset(cpp, subjid == 8),
#'   panel = function(x, y, ...) {
#'     panel.who(x = seq(0, 2558, by = 30),
#'       sex = "Male", y_var = "wtkg", quants = pnorm(-3:0))
#'     panel.xyplot(x, y, ...)
#'   },
#'   col = "black"
#' )
#' @export
panel.who <- function(x, x_var = "agedays", y_var = "htcm", sex = "Female",
  quants = c(0.01, 0.05, 0.25, 0.5), alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  poly_col <- ifelse(sex == "Male", "blue", "red")

  dat <- get_who_band_data(x, x_var, y_var, sex, quants, center, x_trans, y_trans)

  for(dd in dat$quants)
    lattice::panel.polygon(dd$x, dd$y, col = poly_col, alpha = alpha, border = poly_col)

  if(!is.null(dat$med))
    lattice::panel.lines(dat$med$x, dat$med$y, col = poly_col, alpha = alpha)
}

#' Add WHO standard to an ggplot2 plot
#'
#' @param p ggplot2 object to add WHO standard to
#' @template par-who
#' @importFrom ggplot2 geom_polygon geom_path aes
#' @export
geom_who <- function(p, x, x_var = "agedays", y_var = "htcm", sex = "Female",
  quants = c(0.01, 0.05, 0.25, 0.5), alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  poly_col <- ifelse(sex == "Male", "blue", "red")

  dat <- get_who_band_data(x, x_var, y_var, sex, quants, center, x_trans, y_trans)

  for(dd in dat$quants)
    p <- p +
      ggplot2::geom_polygon(data = dd, ggplot2::aes(x = x, y = y), color = poly_col, fill = poly_col, alpha = alpha, size = 0) +
      ggplot2::geom_path(data = dd, ggplot2::aes(x = x, y = y), color = poly_col, alpha = alpha)

  if(!is.null(dat$med))
    p <- p +
      ggplot2::geom_path(data = dat$med, ggplot2::aes(x = x, y = y), color = poly_col, alpha = alpha)

  p
}


#' Add WHO standard to an rbokeh figure
#'
#' @param fig rbokeh figure to add WHO standard to
#' @template par-who
#' @examples
#' \donttest{
#' figure() %>%
#'   ly_who(x = seq(0, 2558, by = 30), y_var = "wtkg",
#'     sex = "Male", x_trans = days2years) %>%
#'   ly_points(days2years(agedays), wtkg,
#'     data = subset(cpp, subjid == 8), col = "black",
#'     hover = c(agedays, wtkg, lencm, htcm, bmi, geniq, sysbp, diabp))
#'
#' cpp$wtkg50 <- who_quantile2value(cpp$agedays, y_var = "wtkg")
#' figure() %>%
#'   ly_who(x = seq(0, 2558, by = 30), y_var = "wtkg", sex = "Male",
#'     x_trans = days2years, center = TRUE) %>%
#'   ly_points(days2years(agedays), wtkg - wtkg50, color = "black",
#'     data = subset(cpp, subjid == 8))
#' }
#' @export
ly_who <- function(fig, x, x_var = "agedays", y_var = "htcm", sex = "Female",
  quants = c(0.01, 0.05, 0.25, 0.5), alpha = 0.15, center = FALSE, labels = TRUE,
  x_trans = identity, y_trans = identity) {

  poly_col <- ifelse(sex == "Male", "blue", "red")

  dat <- get_who_band_data(x, x_var, y_var, sex, quants, center, x_trans, y_trans)

  for(dd in dat$quants)
    fig <- fig %>%
      rbokeh::ly_polygons(dd$x, dd$y, color = poly_col, alpha = alpha)

  if(!is.null(dat$med))
    fig <- fig %>%
      rbokeh::ly_lines(dat$med$x, dat$med$y, color = poly_col, alpha = alpha)

  # if(labels)

  fig
}

# generic function to get data for lattice, ggplot, rbokeh
get_who_band_data <- function(x, x_var = "agedays", y_var = "htcm", sex = "Female",
  quants = c(0.01, 0.05, 0.25, 0.5), center = FALSE,
  x_trans = identity, y_trans = identity) {

  if(any(quants > 0.5)) {
    warning("ignoring 'quants' values that are greater than 0.5")
    quants <- quants[quants <= 0.5]
  }

  has_median <- 0.5 %in% quants
  if(has_median)
    quants <- setdiff(quants, 0.5)

  quants <- sort(quants)

  if(has_median || center)
    med <- who_quantile2value(x, q = 0.5, x_var = x_var, y_var = y_var, sex = sex)

  res <- list()
  res$quants <- lapply(quants, function (qq) {
    val1 <- who_quantile2value(x, q = qq, x_var = x_var, y_var = y_var, sex = sex)
    val2 <- who_quantile2value(x, q = 1 - qq, x_var = x_var, y_var = y_var, sex = sex)
    if(center) {
      val1 <- val1 - med
      val2 <- val2 - med
    }
    data.frame(x = x_trans(c(x, rev(x))), y = y_trans(c(val1, rev(val2))))
  })

  if(has_median) {
    if(center)
      med <- med - med
    res$med <- data.frame(x = x_trans(x), y = y_trans(med))
  }

  res
}



