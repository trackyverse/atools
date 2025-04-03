#' wrapper to filter detections by ping deviation bands
#' 
#' @inheritParams filter_det
#' @param bands a vector of deviation bands to filter
#' @param grace width of the bands to extract
#' 
#' @return the updated ATO
#' 
filter_ping_dev <- function(object, bands, grace, hard = FALSE) {
  band_list <- lapply(bands, function(i) c(i - grace, i + grace))
  if (!has(object, "det")) {
    message("M: No detections to filter.")
  }
  for (i in 1:length(band_list)) {
    message("M: Filtering band ", bands[i])
    if (has(object, "det")) {
      object <- filter_det(object, ping_dev = band_list[[i]], hard = hard)
    }
  }
  return(object)
}

#' Filter rows from the @det slot
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the detections. If only one value is provided,
#'   data is filtered by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match.
#' @param hard If false (the default), detections are flagged as invalid through
#'   the invalid column, but kept in the dataset. Switch to true to drop
#'   detections that do not fit the filtering criteria (useful to improve 
#'   performance when handling very large datasets).
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_det <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "det", error = TRUE)
  
  checks <- list(...)
  if (length(checks) == 0) {
    stop("No filtering arguments provided.", call. = FALSE)
  }

  # for each requested variable, check which rows would be kept
  keep_these <- lapply(names(checks), function(i) {
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
  keep_these <- keep_these[!sapply(keep_these, is.null)]

  if (length(keep_these) == 0) {
    stop("None of the requested variables exist in @det.",
         call. = FALSE)
  } 
  
  # only the rows that would be kept by all variables
  # should be kept.
  keep_these <- data.table::as.data.table(keep_these)
  to_keep_check <- NULL # to avoid rcmdcheck note
  keep_these[, to_keep_check := all(.SD), by = .I]
  keep_these <- keep_these$to_keep_check

  if (all(keep_these)) {
    message("M: All detections match the filtering criteria.")
  } else {
    aux1 <- sum(keep_these)
    aux2 <- nrow(object@det)
    if (hard) {
      new_dets <- object@det[keep_these, ]
      message("M: Subset ", aux1,
              " detections (",
              .dyn_round(aux1 / aux2 * 100),
              "%) that match the filtering criteria.")
      # using add is an easy way to trigger the match checks again
      object <- add(object, new_dets)
    } else {
      object@det$valid[!keep_these] <- FALSE
      message("M: ", aux1,
              " detections (",
              .dyn_round(aux1 / aux2 * 100),
              "%) remain valid after applying the filtering criteria.")
    }
  }
  return(object)
}

#' Filter rows from the @dep slot
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the deployments If only one value is provided,
#'   data is filtered by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match.
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_dep <- function(object, ...) {
  is_ato(object)
  has(object, "dep", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    stop("No filtering arguments provided.", call. = FALSE)
  }

  # for each requested variable, check which rows would be kept
  keep_these <- lapply(names(checks), function(i) {
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
  keep_these <- keep_these[!sapply(keep_these, is.null)]

  if (length(keep_these) == 0) {
    stop("None of the requested variables exist in @dep.",
         call. = FALSE)
  } 
  
  # only the rows that would be kept by all variables
  # should be ketp
  keep_these <- data.table::as.data.table(keep_these)
  to_keep_check <- NULL # to avoid rcmdcheck note
  keep_these[, to_keep_check := all(.SD), by = .I]
  keep_these <- keep_these$to_keep_check

  if (all(keep_these)) {
    message("M: All deployments match the filtering criteria.")
  } else {
    aux <- sum(keep_these)
    message("M: Filtered ", aux,
            ifelse(aux > 1,
                   " deployments.",
                   " deployment."))
    new_deps <- object@dep[keep_these, ]
    # using add is an easy way to trigger the match checks again
    object <- add(object, new_deps) 
  }
  return(object)
}

#' Filter rows from the @obs slot
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the observations. If only one value is provided,
#'   data is filtered by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match.
#' @param hard If false (the default), observations are flagged as invalid
#'   through the valid column, but kept in the dataset. Switch to true to drop
#'   observations that do not fit the filtering criteria (useful to improve 
#'   performance when handling very large datasets).
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_obs <- function(object, ..., hard = FALSE) {
  is_ato(object)
  has(object, "obs", error = TRUE)

  checks <- list(...)
  if (length(checks) == 0) {
    stop("No filtering arguments provided.", call. = FALSE)
  }

  # for each requested variable, check which rows would be kept
  keep_these <- lapply(names(checks), function(i) {
    if (!(i %in% colnames(object@obs))) {
      warning("Variable ", i, " not found in @obs. Skipping.",
              immediate. = TRUE, call. = FALSE)
      output <- NULL
    } else {
      if (length(checks[[i]]) == 2 & is.numeric(checks[[i]])) {
        output <- object@obs[, i, with = FALSE] >= checks[[i]][1] &
                  object@obs[, i, with = FALSE] <= checks[[i]][2]
      } else {
        output <- object@obs[, i, with = FALSE] %in% checks[[i]]
      }
    }
    return(output)
  })
  keep_these <- keep_these[!sapply(keep_these, is.null)]

  if (length(keep_these) == 0) {
    stop("None of the requested variables exist in @obs.",
         call. = FALSE)
  } 
  
  # only the rows that would be kept by all variables
  # should be kept.
  keep_these <- data.table::as.data.table(keep_these)
  to_keep_check <- NULL # to avoid rcmdcheck note
  keep_these[, to_keep_check := all(.SD), by = .I]
  keep_these <- keep_these$to_keep_check

  if (all(keep_these)) {
    message("M: All observations match the filtering criteria.")
  } else {
    aux1 <- sum(keep_these)
    aux2 <- nrow(object@obs)
    if (hard) {
      new_obs <- object@obs[keep_these, ]
      message("M: Subset ", aux1,
              " observations (",
              .dyn_round(aux1 / aux2 * 100),
              "%) that match the filtering criteria.")
      # using add is an easy way to trigger the match checks again
      object <- add(object, new_obs)
    } else {
      object@obs$valid[!keep_these] <- FALSE
      message("M: ", aux1,
              " observations (",
              .dyn_round(aux1 / aux2 * 100),
              "%) remain valid after applying the filtering criteria.")
    }
  }
  return(object)
}

#' Wrapper to filter detections and observations that match the
#' transmitters in the tag slot.
#' 
#' @inheritParams filter_det
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_targeted <- function(object, hard = FALSE) {
  if (!has(object, "tag")) {
    stop("The ATO's tag slot is empty.", call. = FALSE)
  }
  if (has(object, "det")) {
    object <- filter_det(object, transmitter = object@tag$transmitter)
  } else {
    message("M: No detections to filter.")
  }

  if (has(object, "obs")) {
    object <- filter_obs(object, transmitter = object@tag$transmitter)
  } else {
    message("M: No observations to filter.")
  }
  return(object)
}

#' Wrapper to filter detections and observations within the desired time period
#' 
#' @inheritParams filter_det
#' @param from Filter data from this timestamp (included). Can be posixct or character.
#' @param to Filter data up to this timestamp (included). Can be posixct or character.
#' 
#' @return the updated ATO
#' 
#' @export
#'
filter_datetime <- function(object, from = "1970-01-01", to = "3000-01-01",
                            hard = FALSE) {
  if (has(object, "det")) {
    tz <- attributes(object@det$datetime)$tzone
    if (is(from, "character")) {
      from <- as.POSIXct(from, tz = tz)
    }
    if (is(to, "character")) {
      to <- as.POSIXct(to, tz = tz)
    }
    object <- filter_det(object, datetime = c(from, to))
  } else {
    message("M: No detections to filter.")
  }

  if (has(object, "obs")) {
    tz <- attributes(object@obs$datetime)$tzone
    if (is(from, "character")) {
      from <- as.POSIXct(from, tz = tz)
    }
    if (is(to, "character")) {
      to <- as.POSIXct(to, tz = tz)
    }
    object <- filter_obs(object, datetime = c(from, to))
  } else {
    message("M: No observations to filter.")
  }
  return(object)
}
