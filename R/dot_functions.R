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
#' @param type the ATO slot to filter
#' 
#' @return a logical vector
#' 
#' @keywords internal
#' 
.create_filter_vec <- function(object, checks,
                               type = c("det", "dep", "tag", "ani", "obs")) {
  longtype <- switch(type,
                     det = "detections",
                     dep = "deployments",
                     tag = "tags",
                     ani = "animals",
                     obs = "observations")

  # use data.table for fast processing
  checker <- data.table::as.data.table(slot(object, type))

  # for each requested variable, check which rows would be kept
  eval_cols <- c()
  for (i in names(checks)) {
    if (!(i %in% colnames(slot(object, type)))) {
      warning("Variable ", i, " not found in @", type, ". Skipping.",
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
    stop("None of the requested variables exist in @", type, ".",
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
#' @param new_valid a logical vector, normally created by
#'   \code{\link{.create_filter_vec}}.
#' @param type the ATO slot to filter
#' @param hard Should the filtering mark rows as invalid (FALSE) or remove them
#'   altogether (TRUE)
#' 
#' @return the updated ATO
#' 
#' @keywords internal
#' 
.apply_filter <- function(object, new_valid,
                          type = c("det", "dep", "tag", "ani", "obs"),
                          hard = FALSE) {
  longtype <- switch(type,
                     det = "detections",
                     dep = "deployments",
                     tag = "tags",
                     ani = "animals",
                     obs = "observations")

  if (all(new_valid)) {
    message("M: All ", longtype, " match the filtering criteria.")
  } else {
    old_total <- nrow(slot(object, type))
    if (hard) {
      filtered <- slot(object, type)[new_valid, ]
      new_total <- nrow(filtered)        
      message("M: Subset ", new_total,
              " ", longtype, " (",
              .dyn_round(new_total / old_total * 100),
              "%) that match the filtering criteria.")
      # using add is an easy way to trigger the match checks again
      object <- add(object, filtered)
      rm(filtered)
    } else {
      slot(object, type)$valid[!new_valid] <- FALSE
      new_total <- sum(slot(object, type)$valid)
      message("M: ", new_total,
              " ", longtype, " (",
              .dyn_round(new_total / old_total * 100),
              "%) remain valid after applying the filtering criteria.")
    }
  }
  return(object)
}
