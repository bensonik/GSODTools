updateInventory <- function(online = TRUE) {

  ## try to get online inventory  
  if (online) {
    inventory <- read.csv("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv", 
                          na.strings = c("NA", "-99999", "-999999"))
    
  ## or use offline inventory  
  } else {
    inventory <- system.file("extdata", "isd-history.csv", 
                             packages = "GSODTools")
  }
  
}
