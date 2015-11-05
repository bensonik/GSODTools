#' Convert inches and hundredths to millimeters
#' 
#' @description
#' This function converts precipitation amounts from native inches and 
#' hundredths to millimeters (mm).
#' 
#' @param val Numeric. A vector of precipitation amounts in inches and hundredths.
#' @param ... Additional arguments passed to \code{\link{round}}.
#' 
#' @return
#' A numeric vector of precipitation amounts in mm.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' inch2Millimeter(val = c(68, 72, 76), digits = 1)
#' 
#' @export inch2Millimeter
#' @aliases inch2Millimeter
inch2Millimeter <- function(val, ...) {

  val_new <- val * 25.4
  val_new <- round(val_new, ...)
  
  return(val_new)
}
