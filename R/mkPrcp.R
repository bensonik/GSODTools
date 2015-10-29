mkPrcp <- function(country, ref_ext = NULL, 
                   start_date, end_date, 
                   overall_limit = 1, monthly_limit = 0.5, valid_months = 1,
                   p = 0.05, cores = 1L, visualize = TRUE) {
  
  ## required packages
  library(rworldmap)
  library(doParallel)
  
  ## parallelize
  if (cores > 1L) {  
    supcl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(supcl)
  }
  
  ## reference extent
  if (!missing(country)) {
    data("countriesLow")
    ref_ext <- subset(countriesLow, ADMIN == country)
    suppressWarnings(sp::proj4string(ref_ext) <- "+init=epsg:4326")
  } else if (missing(country) & !is.null(ref_ext)) {
    ref_ext <- ref_ext
  } else {
    stop("Please specify either a valid country name or a reference extent.\n")
  }
  
  if (visualize)
    plot(ref_ext)
  
  # download.file("http://biogeo.ucdavis.edu/data/gadm2.7/rds/IRN_adm0.rds", 
  #               destfile = "data/IRN_adm0.rds")
  # spy_iran <- readRDS("data/IRN_adm0.rds")
  # suppressWarnings(proj4string(spy_iran) <- "+init=epsg:4326")
  
  ## gsod stations in reference extent
  gsod_shp <- gsodReformat(data = updateInventory(online = FALSE),
                           elevation = TRUE, 
                           coords = TRUE,
                           df2sp = TRUE)
  
  gsod_shp <- gsod_shp[ref_ext, ]

  ## stations with measurements between 'start_date' and 'end_date'
  st <- as.Date(as.character(gsod_shp$BEGIN), format = "%Y%m%d")
  nd <- as.Date(as.character(gsod_shp$END), format = "%Y%m%d")
  
  id_avl <- which(st <= start_date & nd >= end_date)

  #   ## download data
  #   suppressWarnings(
  #     lst_gsod <- foreach(i = id_avl, .packages = "GSODTools") %dopar% {
  #       usaf <- gsod_shp@data[i, "USAF"]
  #       dlGsodStations(usaf = usaf, dsn = paste0("data/", usaf), unzip = TRUE, 
  #                      start_year = substr(start_date, 1, 4), 
  #                      end_year = substr(end_date, 1, 4))
  #     }
  #   )
  
  #   # save output
  #   saveRDS(lst_gsod, file = "data/iran_1982_2013.rds")
  lst_gsod <- readRDS("data/iran_1982_2013.rds")
  
  ## stations including rainfall
  id_prcp <- sapply(lst_gsod, function(i) {
    "PRCP" %in% names(i)
  })

  if (visualize)
    points(gsod_shp[id_prcp, ], col = "grey75", pch = 4)
  
  ## stations with sufficient amount of valid records
  dt_dates <- seq(start_date, end_date, 1)
  df_dates <- data.frame(YEARMODA = dt_dates)
  
  if (overall_limit < 1L) {
    # loop over single stations
    num_valid <- sapply(lst_gsod, function(i) {
      # merge station data with continuous time series
      i$YEARMODA <- as.Date(as.character(i$YEARMODA), format = "%Y%m%d")
      dat_mrg <- merge(df_dates, i, by = "YEARMODA", all = TRUE)
      # calculate share of valid measurements
      sum(!is.na(dat_mrg$PRCP)) / nrow(dat_mrg)
    })
    
    id_valid <- which(num_valid > (1-overall_limit))
    lst_gsod <- lst_gsod[id_valid]
  }  
  
  # significant trends (p < 0.05); if not significant, NA is returned
  lst_dsn <- lapply(lst_gsod, function(i) {
    # merge station data with continuous time series
    i$YEARMODA <- as.Date(as.character(i$YEARMODA), format = "%Y%m%d")
    dat_mrg <- merge(df_dates, i, by = "YEARMODA", all = TRUE)
    
    # aggregate by month
    dat_mrg$YEARMO <- strftime(dat_mrg$YEARMODA, format = "%Y-%m")

    dat_mrg %>%
      dplyr::select(YEARMODA, YEARMO, PRCP) %>%
      dplyr::group_by(YEARMO) %>%
      dplyr::summarize(PRCP = GSODTools:::partialSum(PRCP, monthly_limit = monthly_limit))  %>%
      data.frame() -> dat_agg

    dat_agg$MO <- sapply(strsplit(dat_agg$YEARMO, "-"), "[[", 2)    
    
    dat_agg %>% 
      dplyr::group_by(MO) %>%
      dplyr::summarize(PRCP_LTM = GSODTools:::partialMean(PRCP, valid_months = valid_months)) %>%
      data.frame() -> dat_ltm
    
    dat_mrg <- merge(dat_agg, dat_ltm, by = "MO", sort = FALSE)
    dat_mrg <- dat_mrg[order(dat_mrg$YEARMO), ]
    dat_mrg$PRCP_DSN <- dat_mrg$PRCP - dat_mrg$PRCP_LTM
    
    return(dat_mrg)
  })

  num_tau <- sapply(lst_dsn, function(i) {  
    # mann-kendall trend
    mk <- Kendall::MannKendall(i$PRCP_DSN)
    
    # if trend is significant, return kendall's tau, else return NA
    if (mk$sl < p) {
      return(mk$tau)
    } else {
      return(NA)
    }
  })

  ch_usaf <- sapply(lst_gsod, function(i) {
    unique(i[, 1])
  })
  
  df_usaf <- data.frame(USAF = ch_usaf, TAU = num_tau)
  
  ## merge with spatial data
  df_sign <- merge(updateInventory(), df_usaf, by = "USAF", all.y = TRUE)
  coordinates(df_sign) <- ~ LON + LAT
  proj4string(df_sign) <- "+init=epsg:4326"
  
  ## visualize
  if (visualize) {
    points(df_sign, pch = 4)
    points(subset(df_sign, !is.na(TAU) & TAU > 0), pch = 4, cex = 1.5, lwd = 1.5,
           col = "blue")
    points(subset(df_sign, !is.na(TAU) & TAU < 0), pch = 4, cex = 1.5, lwd = 1.5,
           col = "red")
  }
  
  ## deregister parallel backend
  if (cores > 1L)
    parallel::stopCluster(supcl)
  
  ## return mann-kendall data
  return(df_sign)
}