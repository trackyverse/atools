#' Filter rows from an ATO slot
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' @param ... named arguments to filter for. The argument's name must match
#'   one of the column names in the animals slot. If only one value is provided,
#'   data is filtered by exact match. If two values are provided for continuous
#'   variables, data is filtered within the interval provided. If three or more
#'   values are provided, data is filtered by exact match.
#' @param slt The slot to be filtered.
#' @param hard If false (the default), animal rows are flagged as invalid
#'   through the valid column, but kept in the dataset. Switch to true to drop
#'   the tags that do not fit the filtering criteria (useful to improve 
#'   performance when handling very large datasets).
#' @param silent Logical: Supress summary messages
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter <- function(object, slt = c("ani", "dep", "det", "tag", "obs"),
                   ..., hard = FALSE, silent = FALSE) {
  slt <- match.arg(slt)
  is_ato(object)
  has(object, slt, error = TRUE)
  
  checks <- list(...)
  if (length(checks) == 0) {
    stop("No filtering arguments provided.", call. = FALSE)
  }

  filter_vec <- create_filter_vec(object, slt, checks)
  object <- .apply_filter(object, filter_vec, exclude = FALSE,
                          slt = slt, hard = hard)

  if (getOption("ATO_match_immediate", default = TRUE)) {
    object <- match_update(object, silent = TRUE)
  }
  return(object)
}

#' Worker function to create filtering vector
#' 
#' @param object an ATO
#' @param slt the ATO slot to filter
#' @param checks a list of column names and respective values to filter with
#' 
#' @return a logical vector
#' 
#' @export
#' 
create_filter_vec <- function(object, slt = c("ani", "dep", "det", "tag", "obs"),
                              checks) {
  longslot <- switch(slt,
                     ani = "animals",
                     dep = "deployments",
                     det = "detections",
                     obs = "observations",
                     tag = "tags")

  # use data.table for fast processing
  if (table_type(object) == "data.table") {
    checker <- slot(object, slt)
  } else {
    checker <- data.table::as.data.table(slot(object, slt))
  }
  # check that filtering requests are appropriate
  for (i in names(checks)) {
    if (!(i %in% colnames(slot(object, slt)))) {
      warning("Variable ", i, " not found in @", slt, ". Skipping.",
              immediate. = TRUE, call. = FALSE)
      checks[[i]] <- NULL
    } else {
      if (!any(class(checker[[i]]) %in% class(checks[[i]]))) {
        stop("data in ", i, " (", .comma(class(checker[[i]])),
             ") is not of the same class as the provided filtering values (",
             .comma(class(checks[[i]])), "). Please modify the class",
             " of the filtering values.", call. = FALSE)
      }
    }
  }
  if (length(checks) == 0) {
    stop("None of the requested variables exist in @", slt, ".",
         call. = FALSE)
  } 
  # for each requested variable, check which rows would be kept
  eval_cols <- c()
  for (i in names(checks)) {
    eval_cols <- c(eval_cols, paste0("filter_check_", i))
    if (length(checks[[i]]) == 1 && is.na(checks[[i]])) {
      to_run <- paste0("checker[, filter_check_", i,
                       " := is.na(", i, ")]")
    } else {
      if (length(checks[[i]]) == 2 &
          (is.double(checks[[i]]) | is.integer(checks[[i]]))) {
        to_run <- paste0("checker[, filter_check_", i,
                         " := ",
                         i, " >= checks[[i]][1] & ",
                         i, " <= checks[[i]][2]]")
      } else {
        to_run <- paste0("checker[, filter_check_", i,
                         " := ",
                         i, " %in% checks[[i]]]")
      }
    }
    eval(parse(text = to_run))
  }

  to_run <- paste0("checker[, filter_check_pass := ",
                   paste0(eval_cols, collapse = " & "),
                   "]")
  eval(parse(text = to_run))

  output <- checker$filter_check_pass

  # clean up if using data.table
  if (table_type(object) == "data.table") {
    for (i in names(checks)) {
      if ((i %in% colnames(slot(object, slt)))) {
        to_run <- paste0("checker[, filter_check_", i,
                         " := NULL]")
      }
      eval(parse(text = to_run))
    }
    checker$filter_check_pass <- NULL
  }
  
  return(output)
}

#' Worker function to apply filtering
#' 
#' @param object an ATO
#' @param filter_vec a logical vector, normally created by
#'   \code{\link{create_filter_vec}}.
#' @param exclude FALSE to filter by filter_vec, TRUE to exclude by filter_vec. 
#' @param slt the ATO slot to filter
#' @param hard Should the filtering mark rows as invalid (FALSE) or remove them
#'   altogether (TRUE)
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.apply_filter <- function(object, filter_vec, exclude,
                          slt = c("ani", "dep", "det", "tag", "obs"),
                          hard = FALSE) {
  longslot <- switch(slt,
                     ani = "animals",
                     dep = "deployments",
                     det = "detections",
                     obs = "observations",
                     tag = "tags")
  if (exclude) {
    # inverting the vector makes it so we only need one
    # pipeline for the filtering. All the variation then
    # is in the messaging.
    filter_vec <- !filter_vec
  }

  if (all(filter_vec)) {
    if (exclude) {
      message("M: No ", longslot, " match the exclusion criteria.")
    } else {
      message("M: All ", longslot, " match the filtering criteria.")
    }
  } else {
    old_total <- nrow(slot(object, slt))
    if (hard) {
      filtered <- slot(object, slt)[filter_vec, ]
      new_total <- sum(abs(exclude - filter_vec))
      message(ifelse(exclude,
                     "M: Removed ", 
                     "M: Subset "), 
              new_total,
              " ", longslot, " (",
              .dyn_round(new_total / old_total * 100),
              "%).")
      # using add is an easy way to trigger the match checks again
      object <- add(object, filtered, silent = TRUE)
      rm(filtered)
    } else {
      if (exclude) {
        new_total <- sum(slot(object, slt)$valid & !filter_vec)
      } else {
        new_total <- sum(slot(object, slt)$valid & filter_vec)
      }
      slot(object, slt)$valid[!filter_vec] <- FALSE

      message(ifelse(exclude,
                     "M: Rendered ", 
                     "M: "), 
              new_total,
              " ", longslot, " (",
              .dyn_round(new_total / old_total * 100),
              ifelse(exclude,
                     "%) invalid.",
                     "%) remain valid."))
    }
  }
  return(object)
}

#' wrapper to filter detections by ping deviation bands
#' 
#' @inheritParams filter
#' @param bands a vector of deviation bands to filter
#' @param grace width of the bands to extract
#' 
#' @return the updated ATO
#' 
#' @export
#'
filter_ping_dev <- function(object, bands, grace, hard = FALSE) {
  old_match_immediate <- getOption("ATO_match_immediate", default = TRUE)
  on.exit(options(ATO_match_immediate = old_match_immediate))

  band_list <- lapply(bands, function(i) c(i - grace, i + grace))
  if (!has(object, "det")) {
    message("M: No detections to filter.")
  } else {
    options(ATO_mach_immediate = FALSE)
    recipient <- lapply(1:length(band_list), function(i) {
      suppressMessages(filter(object, "det", ping_dev = band_list[[i]],
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

  if (old_match_immediate) {
    object <- match_update(object)
  }

  if (any(object@det$time_diff[object@det$valid] < 0.5)) {
    message("Note: Valid detections with very small ping intervals (>500ms)",
            " found. Could these be reflections?")
  }
  return(object)
}

#' Wrapper to filter detections and observations that match the
#' transmitters in the tag and dep slots.
#' 
#' @inheritParams filter
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
    object <- filter(object, "det", transmitter = targets, hard = hard)
  } else {
    message("M: No detections to filter.")
  }

  if (has(object, "obs")) {
    object <- filter(object, "obs", transmitter = targets, hard = hard)
  } else {
    message("M: No observations to filter.")
  }
  return(object)
}

#' Wrapper to filter detections and observations within the desired time period
#' 
#' @inheritParams filter
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
    object <- filter(object, "det", datetime = c(from, to), hard = hard)
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
    object <- filter(object, "obs", datetime = c(from, to), hard = hard)
  } else {
    message("M: No observations to filter.")
  }
  return(object)
}

#' Clear all previously applied (non-hard) filters
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' @param s the slot to be reset. If not provided, all slots are reset.
#' 
#' @return the updated ATO
#' 
#' @export
#' 
filter_reset <- function(object, s) {
  is_ato(object)
  if (missing(s)) {
    s <- c("det", "dep", "tag", "ani", "obs=")
  }
  for (i in s) {
    if (has(object, i)) {
      slot(object, i)$valid <- TRUE
    }
  }
  return(object)
}
