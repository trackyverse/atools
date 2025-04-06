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

  filter_vec <- .create_filter_vec(object, checks, "det")
  object <- .apply_filter(object, filter_vec, exclude = FALSE,
                          slt = "det", hard = hard)
  
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

  filter_vec <- .create_filter_vec(object, checks, "dep")
  object <- .apply_filter(object, filter_vec, exclude = FALSE,
                          slt = "dep", hard = TRUE)

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

  filter_vec <- .create_filter_vec(object, checks, "obs")
  object <- .apply_filter(object, filter_vec, exclude = FALSE,
                          slt = "obs", hard)

  return(object)
}

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
  } else {
    recipient <- lapply(1:length(band_list), function(i) {
      suppressMessages(filter_det(object, ping_dev = band_list[[i]],
                 hard = FALSE)@det$valid)
    })
    checker <- data.table::as.data.table(recipient)
    to_run <- paste0("checker[, valid := ",
                     paste0(paste0("V", 1:ncol(checker)), collapse = " | "),
                     "]")
    eval(parse(text = to_run))

    object <- .apply_filter(object, checker$valid, exclude = FALSE,
                          slt = "det", hard = hard)
  }
  return(object)
}

#' Wrapper to filter detections and observations that match the
#' transmitters in the tag and dep slots.
#' 
#' @inheritParams filter_det
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_targeted <- function(object, hard = FALSE) {
  if (!has(object, "tag") & !has(object, "dep")) {
    stop("Both the tag and the dep slotes are empty. Can't filter.", call. = FALSE)
  }
  targets <- c(object@tag$transmitter, object@dep$transmitter)

  if (has(object, "det")) {
    object <- filter_det(object, transmitter = targets, hard = hard)
  } else {
    message("M: No detections to filter.")
  }

  if (has(object, "obs")) {
    object <- filter_obs(object, transmitter = targets, hard = hard)
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
    object <- filter_det(object, datetime = c(from, to), hard = hard)
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
    object <- filter_obs(object, datetime = c(from, to), hard = hard)
  } else {
    message("M: No observations to filter.")
  }
  return(object)
}

#' Clear all previously applied (non-hard) filters
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_reset <- function(object) {
  is_ato(object)
  has(object, "det", error = TRUE)

  if (all(object@det$valid)) {
    message("M: All ", nrow(object@det), " detections are already valid.")
  }
  # use data.table for fast processing
  object@det$valid <- TRUE

  return(object)
}
