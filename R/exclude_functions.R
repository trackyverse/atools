#' Exclude rows from the @det slot
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the detections. If only one value is provided,
#'   data is excluded by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match. If no arguments
#'   are provided, the entire slot is removed.
#' @param hard If false (the default), detections are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to completely
#'   remove the detections from the dataset (useful to improve performance when
#'   handling very large datasets). Ignored if no exclusion arguments
#'   are provided.
#'
#' @return the updated ATO
#'
#' @export
#'
exclude_det <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "det", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    object@det <- ATO::.ATO_det
    return(object)
  }

  filter_vec <- .create_filter_vec(object, checks, "det")
  object <- .apply_filter(
    object,
    filter_vec,
    exclude = TRUE,
    slt = "det",
    hard = hard
  )

  return(object)
}

#' Exclude rows from the @dep slot
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the deployments. If only one value is provided,
#'   data is filtered by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match.
#'
#' @return the updated ATO
#'
#' @export
#'
exclude_dep <- function(object, ...) {
  is_ato(object)
  has(object, "dep", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    object@dep <- ATO::.ATO_dep
    return(object)
  }

  filter_vec <- .create_filter_vec(object, checks, "dep")
  object <- .apply_filter(
    object,
    filter_vec,
    exclude = TRUE,
    slt = "dep",
    hard = TRUE
  )

  return(object)
}
