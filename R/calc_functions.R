#' Calculate time interval between detections
#' 
#' @param object an \code{\link[ATO]{ATO}}
#' 
#' @return an updated \code{\link[ATO]{ATO}}
#' 
#' @export
#' 
calc_det_interval <- function(object) {
  by_receiver <- split(object@det,
                       object@det$receiver)
  recipient1 <- lapply(by_receiver, function(receiver) {
    # cat(receiver$receiver_serial[1], "\n")
    by_tag <- split(receiver,
                    receiver$transmitter)
    recipient2 <- lapply(by_tag, function(tag) {
      # cat(" - ", tag$transmitter[1], "\n")
      tag <- tag[order(tag$datetime), ]
      tag$datetime_num <- as.numeric(tag$datetime) + tag$frac_second
      # the first detection has no previous to be matched against
      tag$time_diff <- c(NA,
                         tag$datetime_num[-1] - tag$datetime_num[-nrow(tag)])
      # The NAs can trip filtering functions though, so give the first
      # detection the same interval as the second. This ensures that
      # both are kept in the event of filtering.
      tag$time_diff[1] <- tag$time_diff[2]
      
      ping_rate <- NA
      tag_link <- object@tag$transmitter == tag$transmitter[1]
      dep_link <- object@dep$transmitter == tag$transmitter[1]
      if (any(tag_link)) {
        ping_rate <- object@tag$ping_rate[tag_link]
      }
      if (any(dep_link)) {
        ping_rate <- object@dep$transmitter_ping_rate[dep_link]
      }
      if (!is.na(ping_rate)) {
        tag$ping_dev <- tag$time_diff %% ping_rate
        dev_link <- c(tag$ping_dev > (ping_rate / 2))
        tag$ping_dev[dev_link] <- ping_rate - tag$ping_dev[dev_link]
      } else {
        tag$ping_dev <- NA
      }
      return(tag)
    })
    output <- data.table::rbindlist(recipient2)
    return(output)
  })
  output <- data.table::rbindlist(recipient1)
  class(output) <- class(object@det)
  object@det <- output
  return(object)
}
