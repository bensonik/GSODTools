#' Compendium of helper functions
#' 
#' @name helperFunctions
NULL

## long-term trends based on monthly data
partialSum <- function(x, monthly_limit = 0.5) {
  if (length(which(is.na(x))) / length(x) > monthly_limit) {
    return(NA)
  } else {
    return(sum(x, na.rm = TRUE))
  }
}

partialMean <- function(x, valid_months = 5) {
  if (sum(!is.na(x)) < valid_months) {
    return(NA)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}