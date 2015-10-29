#' Retrieve latest version of the GSOD station inventory
#' 
#' @description 
#' Download the latest version of the GSOD station inventory from 
#' \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv} or, if no active 
#' internet connection exists, import the latest local backup.
#' 
#' @param online 'logical'. If \code{TRUE} (default), the function tries to 
#' retrieve the latest online version of the GSOD station inventory. Otherwise 
#' (or if no active internet connection is available), the inventory is imported 
#' locally.
#' 
#' @return 
#' A 'data.frame' with information about available GSOD stations.
#' 
#' @author 
#' Florian Detsch
#' 
#' @examples 
#' updateInventory()
#' 
#' @export updateInventory
#' @name updateInventory
updateInventory <- function(online = TRUE) {

  ## try to get online inventory  
  if (online) {
    onlinefile <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
    inventory <- try(read.csv(onlinefile, 
                              na.strings = c("NA", "-99999", "-999999")), 
                     silent = TRUE)
    
    if (class(inventory) == "try-error")
      online <- FALSE
  }
    
  ## or use offline inventory  
  if (!online) {
    offlinefile <- system.file("extdata/isd-history.csv", 
                               package = "GSODTools")
    inventory <- read.csv(offlinefile, 
                          na.strings = c("NA", "-99999", "-999999"))
  }
  
  return(inventory)
}
