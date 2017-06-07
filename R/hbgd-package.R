utils::globalVariables(c(
  "x", "y", "subjid", "who", "timeunits", "freq", "Freq", "grid_plot", "day", "study", "type",
  "short_id", "label", "n_unique", "variable", "var", "count", "Var1", "Var2", "agedays", "zcat",
  "Var1h", "Var2h", "CompleteCases", "dy", "dz", "yfit", "zfit", "hold", "fit", "haz"
))

#' hbgd: Healthy Birth, Growth & Development
#'
#' A package for visual and analytical methods for the analysis of
#' longitudinal growth data.
#'
#' \url{http://hbgdki.github.io/hbgd/}
#' @name hbgd-package
#' @aliases hbgd
#' @docType package
#' @importFrom rbokeh figure ly_lines ly_points ly_rect ly_crect ly_hist ly_quantile grid_plot theme_axis x_axis ly_bar theme_grid tool_wheel_zoom tool_pan theme_plot ly_text pal_tableau
#' @examples
#' help(package = "hbgd")
#' @import dplyr
#' @import growthstandards
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot
#' @importFrom stats approx approxfun complete.cases loess mad median model.matrix optimize pnorm predict qnorm quantile update
#' @importFrom utils combn getFromNamespace head methods tail
NULL

# importFrom magrittr "%>%"

#' Labels for common variable names in hbgd data
#'
#' @name hbgd_labels
#' @docType data
#' @description
#' Labels for common variable names in hbgd data, used in \code{\link{get_data_attributes}} if labels are not explicitly provided.
#' @usage hbgd_labels
#' @seealso \code{\link{get_data_attributes}}
#' @keywords data
NULL

#' Labels for common variable names in hbgd data
#'
#' @name hbgd_labels_df
#' @docType data
#' @description
#' Labels for common variable names in hbgd data, used in \code{\link{get_data_attributes}} if labels are not explicitly provided.
#' @usage hbgd_labels_df
#' @keywords data
NULL

#' Subset of growth data from the collaborative perinatal project (CPP)
#'
#' @name cpp
#' @docType data
#' @description
#' Subset of growth data from the collaborative perinatal project (CPP).
#' @source
#' \url{https://catalog.archives.gov/id/606622}
#'
#' Broman, Sarah. "The collaborative perinatal project: an overview." Handbook of longitudinal research 1 (1984): 185-227.
#' @usage cpp
#' @keywords data
#' @examples
#' head(cpp)
NULL
