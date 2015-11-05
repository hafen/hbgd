#' @param dat data frame specifying x and y coordinates of data to fit
#' @param xg x grid across which to compute fits for plotting (default is sequence along the range of \code{x})
#' @param cpx x values at which to compute "checkpoints" of the fitted subjects's growth trajectory to compare to other subjects
#' @note
#' The trajectory fitting functions are most easily accessed through calling \code{\link{fit_trajectory}} with the \code{method} argument to specify the modeling approach to use.
#'
#' These fitting functions can easily be replaced by simply calling the associated R methods, but are provided for convenience to standardize input/output to simplify swapping fitting methods.
