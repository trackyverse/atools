#' Round value to a reasonable number of decimal places
#' 
#' @param value the value to be rounded
#' @param max the maximum number of decimal places allowed
#' 
#' @return the rounded value
#' 
#' @keywords internal
#' 
.dyn_round <- function(value, max = 10) {
  dec <- 2
  while(round(value, dec) == 0 & dec < max) {
    dec <- dec + 1
  }
  return(round(value, dec))
}

#' worker function to create filtering vector
#' 
#' @param object an ATO
#' @param checks a list of column names and respective values to filter with
#' @param slt the ATO slot to filter
#' 
#' @return a logical vector
#' 
#' @keywords internal
#' 
.create_filter_vec <- function(object, checks,
                               slt = c("det", "dep", "tag", "ani", "obs")) {
  longslot <- switch(slt,
                     det = "detections",
                     dep = "deployments",
                     tag = "tags",
                     ani = "animals",
                     obs = "observations")

  # use data.table for fast processing
  checker <- data.table::as.data.table(slot(object, slt))

  # for each requested variable, check which rows would be kept
  eval_cols <- c()
  for (i in names(checks)) {
    if (!(i %in% colnames(slot(object, slt)))) {
      warning("Variable ", i, " not found in @", slt, ". Skipping.",
              immediate. = TRUE, call. = FALSE)
    } else {
      eval_cols <- c(eval_cols, paste0("filter_check_", i))
      if (length(checks[[i]]) == 2 & is.double(checks[[i]])) {
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

  if (length(eval_cols) == 0) {
    stop("None of the requested variables exist in @", slt, ".",
         call. = FALSE)
  } 
  
  to_run <- paste0("checker[, filter_check_pass := ",
                   paste0(eval_cols, collapse = " & "),
                   "]")
  eval(parse(text = to_run))
  return(checker$filter_check_pass)
}

#' Worker function to apply filtering
#' 
#' @param object an ATO
#' @param filter_vec a logical vector, normally created by
#'   \code{\link{.create_filter_vec}}.
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
                          slt = c("det", "dep", "tag", "ani", "obs"),
                          hard = FALSE) {
  longslot <- switch(slt,
                     det = "detections",
                     dep = "deployments",
                     tag = "tags",
                     ani = "animals",
                     obs = "observations")
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
      object <- add(object, filtered)
      rm(filtered)
    } else {
      if (exclude) {
        new_total <- sum(slot(object, det)$valid & !filter_vec)
      }
      slot(object, slt)$valid[!filter_vec] <- FALSE
      if (!exclude) {    
        new_total <- sum(slot(object, slt)$valid)
      }
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
