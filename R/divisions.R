
#' Divide a data set into subsets by subject
#'
#' @param dat data set to divide by subject
#' @param subjid variable name in \code{dat} that contains the subject's identifier
#' @examples
#' cppsubj <- by_subject(cpp)
#' @export
by_subject <- function(dat, subjid = "subjid") {
  if(!has_data_attributes(dat))
    dat <- get_data_attributes(dat)

  dat <- update_var_names(list(subjid = subjid), dat)

  res <- datadr::divide(dat, by = "subjid")
  attr(res, "hbgd") <- attr(dat, "hbgd")

  res
}

#' Split by-subject trajectory-fitted data by checkpoint categorizations
#'
#' @param dat a data object returned by \code{\link{fit_all_trajectories}}
#' @param complete subset only to those that have fitted checkpoints
#' @examples
#' \donttest{
#' cppsubj <- by_subject(cpp)
#' cppt <- fit_all_trajectories(cppsubj, method = "rlm")
#' cppcp <- by_trajectory_checkpoints(cppt)
#' }
#' @export
by_trajectory_checkpoints <- function(dat, complete = TRUE) {

  # should be split by subject id
  check_ddo(dat)
  check_subj_split(dat)

  n_rec <- length(dat)
  message("* Filtering out subjects that didn't have enough data to fit at checkpoints...")

  # subset only to those that have fitted checkpoints
  dat <- drFilter(dat, function(x) {
    if(is.null(x$checkpoint))
      return(FALSE)
    if(complete && any(is.na(x$checkpoint$zcat)))
      return(FALSE)
    TRUE
  }, params = list(complete = complete))

  n_rec2 <- length(dat)

  if(n_rec != n_rec2)
    message("* Went from ", n_rec, " to ", n_rec2, " subjects.")

  res <- dat %>% addTransform(function(x) {
    v <- x$data
    v$subjid <- getSplitVar(x, "subjid")

    keys <- paste0(x$checkpoint$x, "=", x$checkpoint$zcat)
    splitdf <- data.frame(t(keys), stringsAsFactors = FALSE)
    names(splitdf) <- paste0("checkpoint", seq_along(keys))
    attr(v, "split") <- splitdf

    kvPair(
      k = paste(keys, collapse = "|"),
      v = v
    )
  })
  recombine(res, combDdf)
}
