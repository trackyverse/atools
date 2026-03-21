#' Exclude rows from the @ani slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to exclude by. The argument's name must match
#'   one of the column names in the animals slot. If only one value is provided,
#'   data is excluded by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match. If no arguments
#'   are provided, the entire slot is removed.
#' @param hard If false (the default), animals are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to completely
#'   remove the animals from the dataset (useful to improve performance when
#'   handling very large datasets). Ignored if no exclusion arguments
#'   are provided.
#'
#' @return the updated ATO
#'
#' @export
#'
exclude_ani <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "ani", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    object@ani <- ATO::.ATO_ani
    if (has(object, "det")) {
      object@det$animal <- NULL
    }
    if (has(object, "tag")) {
      object@tag$ani_match <- NULL
    }
    if (has(object, "obs")) {
      object@obs$animal <- NULL
    }
    return(object)
  }

  filter_vec <- .create_filter_vec(object, checks, "ani")
  object <- .apply_filter(
    object,
    filter_vec,
    exclude = TRUE,
    slt = "ani",
    hard = hard
  )
  if (has(object, "det")) {
    object <- match_det_ani(object, silent = TRUE)
  }
  if (has(object, "tag")) {
    object <- match_tag_ani(object, silent = TRUE)
  }
  if (has(object, "obs")) {
    object <- match_obs_ani(object, silent = TRUE)
  }
  return(object)
}

#' Exclude rows from the @dep slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
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
    if (has(object, "det")) {
      # reset tag match. There's probably a 
      # more efficient way of doing this
      object@det$tag_match <- NULL
      if (has(object, "tag")) {
        object <- match_det_tag(object, silent = TRUE)
      }
    }
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

  if (has(object, "det")) {
    # reset tag match. There's probably a 
    # more efficient way of doing this
    object@det$tag_match <- NULL
    object <- match_det_dep(object, silent = TRUE)
    if (has(object, "tag")) {
      object <- match_det_tag(object, silent = TRUE)
    }
  }
  return(object)
}

#' Exclude rows from the @det slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
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
    if (has(object, "ani")) {
      object@tag$det_match <- NULL
    }
    if (has(object, "dep")) {
      object@dep$n_detections <- NULL
    }
    if (has(object, "tag")) {
      object@tag$det_match <- NULL
    }
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

  if (has(object, "ani")) {
    object <- match_det_ani(object, silent = TRUE)
  }
  if (has(object, "tag")) {
    object <- match_det_tag(object, silent = TRUE)
  }
  if (has(object, "dep")) {
    object <- match_det_dep(object, silent = TRUE)
  }
  return(object)
}

#' Exclude rows from the @obs slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the observations. If only one value is provided,
#'   data is excluded by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match. If no arguments
#'   are provided, the entire slot is removed.
#' @param hard If false (the default), observations are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to completely
#'   remove the detections from the dataset (useful to improve performance when
#'   handling very large datasets). Ignored if no exclusion arguments
#'   are provided.
#'
#' @return the updated ATO
#'
#' @export
#'
exclude_obs <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "obs", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    object@obs <- ATO::.ATO_obs
    if (has(object, "ani")) {
      object@tag$obs_match <- NULL
    }
    if (has(object, "tag")) {
      object@tag$obs_match <- NULL
    }
    return(object)
  }

  filter_vec <- .create_filter_vec(object, checks, "obs")
  object <- .apply_filter(
    object,
    filter_vec,
    exclude = TRUE,
    slt = "obs",
    hard = hard
  )

  if (has(object, "tag")) {
    object <- match_obs_tag(object, silent = TRUE)
  }
  if (has(object, "ani")) {
    object <- match_obs_ani(object, silent = TRUE)
  }
  return(object)
}

#' Exclude rows from the @tag slot
#'
#' If exclusion parameters are not provided, removes the entire slot.
#'
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the tags slot If only one value is provided,
#'   data is excluded by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match. If no arguments
#'   are provided, the entire slot is removed.
#' @param hard If false (the default), tags are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to completely
#'   remove the detections from the dataset (useful to improve performance when
#'   handling very large datasets). Ignored if no exclusion arguments
#'   are provided.
#'
#' @return the updated ATO
#'
#' @export
#'
exclude_tag <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "tag", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    object@tag <- ATO::.ATO_tag
    if (has(object, "tag")) {
      object@det$tag_match <- NULL
    }
    if (has(object, "obs")) {
      object@dep$tag_match <- NULL
    }
    if (has(object, "ani")) {
      object@ani$tag_match <- NULL
    }
    return(object)
  }

  filter_vec <- .create_filter_vec(object, checks, "tag")
  object <- .apply_filter(
    object,
    filter_vec,
    exclude = TRUE,
    slt = "tag",
    hard = hard
  )

  if (has(object, "det")) {
    object <- match_det_tag(object, silent = TRUE)
  }
  if (has(object, "obs")) {
    object <- match_obs_tag(object, silent = TRUE)
  }
  if (has(object, "ani")) {
    object <- match_tag_ani(object, silent = TRUE)
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

  object <- exclude_det(object, dep_match = NA,
                        hard = hard)

  return(object)
}

