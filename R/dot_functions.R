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

#' Concatenate vectors with commas
#' 
#' @param x The vector to write out
#' @param max_i maximum number of vector elements to display before
#'  saying "and n others"
#' 
#' @return a string
#' 
#' @keywords internal
#' 
#' tests:
#'   .comma(LETTERS[1])
#'   .comma(LETTERS[1:2])
#'   .comma(LETTERS[1:3])
#'   .comma(LETTERS[1:5])
#'   .comma(LETTERS[1:10])
#'
#'   .comma(LETTERS[1], max_i = 2)
#'   .comma(LETTERS[1:2], max_i = 2)
#'   .comma(LETTERS[1:3], max_i = 2)
#'   .comma(LETTERS[1:5], max_i = 2)
#'   .comma(LETTERS[1:10], max_i = 2)
#'
#'   .comma(LETTERS[1], max_i = 1)
#'   .comma(LETTERS[1:2], max_i = 1)
#'   .comma(LETTERS[1:3], max_i = 1)
#'   .comma(LETTERS[1:5], max_i = 1)
#'   .comma(LETTERS[1:10], max_i = 1)
#'
.comma <- function(x, max_i = 5) {
  if (max_i < 2) {
    if (length(x) == 1) {
      output <- x
    } else {
      output <- paste0(x[1], " and ", length(x)-1,
                      ifelse(length(x)-1 == 1,
                             " other",
                             " others"))
    }
  } else {
    if (length(x) == 1) {
      output <- x
    }
    if (length(x) == 2) {
      output <- paste0(x[1], " and ", x[2])  
    }
    if (length(x) > 2) {
      if (length(x) <= max_i) {
        output <- paste0(paste0(x[-length(x)], collapse = ", "),
                         ", and ", x[length(x)])
      } else {
        output <- paste0(paste0(x[1:max_i], collapse = ", "),
                         " and ", length(x)-max_i,
                         ifelse(length(x)-max_i == 1,
                                " other",
                                " others"))
      }
    }
  }
  return(output)
}
