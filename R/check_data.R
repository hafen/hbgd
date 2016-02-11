


# look for:
# make sure it is a data frame
# subjid field
# anthropometric variables
# is it longitudinal?
# list variable names that don't match that found in hbgd_labels

# "`subjid`, `agedays`, `sex`, `sexn`, `siteid`"

# check_data <- function(dat) {

#   chk <- crayon::green("\u2713")
#   exx <- crayon::red("\u2717")
# {
#   message("Checking if data is a data frame... ", appendLF = FALSE)
#   if(!is.data.frame(dat)) {
#     message(exx)
#     return(invisible(FALSE))
#   } else {
#     message(chk)
#   }

#   message("Looking for field 'subjid'... ", appendLF = FALSE)
#   if(!"subjid" %in% names(dat)) {
#     message(exx)
#     message("A 'subjid' field that indicates which subjects ")
#     return(invisible(FALSE))
#   } else {
#     message(chk)
#   }

#   message("Looking for anthropometric fields... ", appendLF = FALSE)
#   anthro <- c("lencm", "htcm", "")
#   if(!"subjid" %in% names(dat)) {
#     message(exx)
#     message("A 'subjid' field that indicates which subjects ")
#     return(invisible(FALSE))
#   } else {
#     message(chk)
#   }

# }

# }

#

# fix_data <- function() {

# }

#' View known hbgd variables
#'
#' @export
#' @importFrom DT datatable
view_variables <- function() {
  tmp <- hbgd::hbgd_labels_df
  names(tmp) <- c("variable", "description")
  DT::datatable(hbgd_labels_df, rownames = FALSE)
}




# # update names of data when non-standard variable names are used
# update_var_names <- function(names_list, dat) {
#   dat_names <- names(dat)
#   for(nm in names(names_list)) {
#     if(!names_list[[nm]] %in% dat_names)
#       stop(sprintf("argument %s='%s': variable '%s' not present in the data", nm, names_list[[nm]], names_list[[nm]]), call. =  FALSE)
#     names(dat)[which(names_list[[nm]] == dat_names)] <- nm
#   }
#   dat
# }
