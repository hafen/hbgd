#' @param x value or vector of values that correspond to a measure defined by \code{x_var}
#' @param x_var x variable name (typically "agedays") - see details
#' @param y_var y variable name (typically "htcm" or "wtkg") - see details
#' @param sex "Male" or "Female"
#' @param quants quantiles at which to draw the WHO polygons
#' @param alpha transparency of the polygons
#' @param center should the polygons be centered around the median?
#' @param labels should the quantiles be labeled? (not implemented)
#' @param x_trans transformation function to be applied to x-axis
#' @param y_trans transformation function to be applied to y-axis
