#' Exclude rows from an ATO slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to exclude by. The argument's name must match
#'   one of the column names in the target slot. If only one value is provided,
#'   data is excluded by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match. If no arguments
#'   are provided, the entire slot is removed.
#' @param slt The slot to be filtered.
#' @param hard If false (the default), animal rows are flagged as invalid
#'   through the valid column, but kept in the dataset. Switch to true to drop
#'   the rows that do not fit the filtering criteria (useful to improve 
#'   performance when handling very large datasets).
#' @param silent Logical: Supress summary messages
#'
#' @return the updated ATO
#'
#' @export
#'
exclude <- function(object, slt = c("ani", "dep", "det", "tag", "obs"),
                    ..., hard = FALSE, silent = FALSE) {
  slt <- match.arg(slt)
  is_ato(object)
  has(object, slt, error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    to_run <- paste0("object@", slt, " <- ATO::.ATO_", slt)
    eval(parse(text = to_run))
  } else {
    filter_vec <- create_filter_vec(object, slt = slt, checks = checks)
    object <- .apply_filter(object, filter_vec, exclude = TRUE,
                            slt = slt, hard = hard)
  }

  if (getOption("ATO_match_immediate", default = TRUE)) {
    object <- match_update(object, silent = TRUE)
  }
  return(object)
}

#' Wrapper to exclude detections that to not match the deployments
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param hard If false (the default), detections are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to completely
#'   remove the detections from the dataset (useful to improve performance when
#'   handling very large datasets).
#' 
#' @return the updated ATO
#'
#' @export
#'
exclude_orphan_dets <- function(object, hard = FALSE) {
  is_ato(object)
  has(object, "det", error = TRUE)

  object <- exclude(object, "det", dep_match = NA,
                    hard = hard)

  return(object)
}

