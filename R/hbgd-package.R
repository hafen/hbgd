utils::globalVariables(c("who", "x", "y", "who", "timeunits", "freq", "Freq", "grid_plot", "day", "study", "type", "short_id", "label", "n_unique", "variable", "var", "count", "Var1", "Var2", "agedays", "zcat", "Var1h", "Var2h", "CompleteCases"))

#' hbgd: Healthy Birth, Growth & Development
#'
#' @name hbgd-package
#' @aliases hbgd
#' @docType package
#' @import datadr
#' @import trelliscope
#' @importFrom rbokeh figure ly_lines ly_points ly_rect ly_crect ly_hist ly_quantile grid_plot theme_axis x_axis ly_bar theme_grid tool_wheel_zoom tool_pan theme_plot ly_text pal_tableau
#' @importFrom dplyr group_by summarise mutate arrange filter desc summarise_each group_by_ n_distinct n funs
NULL

# importFrom magrittr "%>%"

#' List of WHO Child Growth Standards
#'
#' @name who
#' @docType data
#' @description
#' A list of coefficients from the WHO for various pairs of growth standards that are used to compute quantiles and z-scores.  The format is a list, where each element is a pairing of variables, e.g. "wtkg_agedays", "htcm_agedays", "bmi_agedays", "hcircm_agedays", "muaccm_agedays", "ss_agedays", "tsftmm_agedays", "wtkg_lencm""wtkg_htcm".  Within each of these elements is a list for sex with names "Female" and "Male".
#' @examples
#' head(who$htcm_age$Female$data)
#' @seealso \code{\link{who_quantile2value}}, \code{\link{who_value2quantile}}, \code{\link{who_zscore2value}}, \code{\link{who_value2zscore}}
#'
#' @source
#' 0-5 years: \url{http://www.who.int/childgrowth/software/en/}
#'
#' 5-15 years: \url{http://www.who.int/growthref/tools/en/}
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


