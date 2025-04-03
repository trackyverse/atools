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
    object@det <- ATO:::.ATO_det
    return(object)
  }

  # for each requested variable, check which rows would be excluded
  exclude_these <- lapply(names(checks), function(i) {
    if (!(i %in% colnames(object@det))) {
      warning("Variable ", i, " not found in @det. Skipping.",
              immediate. = TRUE, call. = FALSE)
      output <- NULL
    } else {
      if (length(checks[[i]]) == 2 & is.numeric(checks[[i]])) {
        output <- object@det[, i, with = FALSE] >= checks[[i]][1] &
                  object@det[, i, with = FALSE] <= checks[[i]][2]
      } else {
        output <- object@det[, i, with = FALSE] %in% checks[[i]]
      }
    }
    return(output)
  })
  exlude_these <- exclude_these[!sapply(exclude_these, is.null)]

  if (length(exclude_these) == 0) {
    stop("None of the requested variables exist in @det.",
         call. = FALSE)
  } 
  
  # only the rows that would be excluded by all variables
  # should be excluded.
  exclude_these <- data.table::as.data.table(exclude_these)
  to_exclude_check <- NULL # to avoid rcmdcheck note
  exclude_these[, to_exclude_check := all(.SD), by = .I]
  exclude_these <- exclude_these$to_exclude_check

  if (all(!exclude_these)) {
    message("M: No detections match the exclusion criteria.")
  } else {
    new_invalid <- object@det$valid & exclude_these
    aux1 <- min(sum(new_invalid), sum(exclude_these))
    aux2 <- nrow(object@det)
    if (hard) {
      new_dets <- object@det[!exclude_these, ]
      message("M: Removed ", aux1,
              " detections (",
              .dyn_round(aux1 / aux2 * 100),
              "%) that matched the exclusion criteria.")
      # using add is an easy way to trigger the match checks again
      object <- add(object, new_dets)
    } else {
      object@det$valid[!exclude_these] <- FALSE
      message("M: Rendered ", aux1,
              " detections (",
              .dyn_round(aux1 / aux2 * 100),
              "%) that matched the exclusion criteria invalid.")
    }
  }
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
    object@dep <- ATO:::.ATO_dep
    return(object)
  }

  # for each requested variable, check which rows would be excluded
  exclude_these <- lapply(names(checks), function(i) {
    if (!(i %in% colnames(object@dep))) {
      warning("Variable ", i, " not found in @dep. Skipping.",
              immediate. = TRUE, call. = FALSE)
      output <- NULL
    } else {
      if (length(checks[[i]]) == 2 & is.numeric(checks[[i]])) {
        output <- object@dep[, i, with = FALSE] >= checks[[i]][1] &
                  object@dep[, i, with = FALSE] <= checks[[i]][2]
      } else {
        output <- object@dep[, i, with = FALSE] %in% checks[[i]]
      }
    }
    return(output)
  })
  exlude_these <- exclude_these[!sapply(exclude_these, is.null)]

  if (length(exclude_these) == 0) {
    stop("None of the requested variables exist in @dep.",
         call. = FALSE)
  } 
  
  # only the rows that would be excluded by all variables
  # should be excluded.
  exclude_these <- data.table::as.data.table(exclude_these)
  to_exclude_check <- NULL # to avoid rcmdcheck note
  exclude_these[, to_exclude_check := all(.SD), by = .I]
  exclude_these <- exclude_these$to_exclude_check
  
  if (all(!exclude_these)) {
    message("M: No deployments match the exclusion criteria.")
  } else {
    aux <- sum(exclude_these)
    message("M: Removed ", aux,
            ifelse(aux > 1,
                   " deployments.",
                   " deployment."))
    new_deps <- object@dep[!exclude_these, ]
    # using add is an easy way to trigger the match checks again
    object <- add(object, new_deps)
  }

  return(object)
}
