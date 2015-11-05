mkPrcp <- function(country, dsn = getwd(), ref_ext = NULL, rds = NULL, chirps = NULL,
                   start_date, end_date, interval = c("monthly", "bimonthly"),
                   prewhitening = TRUE, overall_limit = 1, monthly_limit = 0.5, 
                   valid_months = 1, 
                   cores = 1L, visualize = TRUE, p = 0.05) {
  
  ## required packages
  library(rworldmap)
  library(doParallel)
  
  ## parallelize
  if (cores > 1L) {  
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
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
  
  ## gsod stations in reference extent
  gsod_shp <- gsodReformat(data = updateInventory(),
                           elevation = TRUE, 
                           coords = TRUE,
                           df2sp = TRUE)
  
  gsod_shp <- gsod_shp[ref_ext, ]

  ## stations with measurements between 'start_date' and 'end_date'
  st <- as.Date(as.character(gsod_shp$BEGIN), format = "%Y%m%d")
  nd <- as.Date(as.character(gsod_shp$END), format = "%Y%m%d")
  
  id_avl <- which(st <= start_date & nd >= end_date)

  ## download data
  if (!is.null(rds)) {
    lst_gsod <- readRDS(rds)
  } else {
    suppressWarnings(
      lst_gsod <- foreach(i = id_avl, .packages = "GSODTools") %dopar% {
        usaf <- gsod_shp@data[i, "USAF"]
        GSODTools::dlGsodStations(usaf = usaf, 
                                  dsn = paste(dsn, usaf, sep = "/"), unzip = TRUE, 
                                  start_year = substr(start_date, 1, 4), 
                                  end_year = substr(end_date, 1, 4))
      }
    )
  }

  ## stations including rainfall
  id_prcp <- sapply(lst_gsod, function(i) {
    "PRCP" %in% names(i)
  })

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
  
  ## reject zero values when chirps > 0 mm
  if (!is.null(chirps)) {
    lst_gsod <- foreach(i = 1:length(lst_gsod)) %dopar% {
      # gsod
      dat_gsod <- lst_gsod[[i]]
      dat_gsod$YEARMODA <- as.Date(as.character(dat_gsod$YEARMODA), 
                                   format = "%Y%m%d")
      
      # chirps
      dt_chirps <- as.Date(names(chirps)[1:sum(!is.na(names(chirps)))])
      mat_chirps <- chirps[, 1:sum(!is.na(names(chirps)))]
      dat_chirps <- data.frame(YEARMODA = dt_chirps, CHIRPS = mat_chirps[i, ])

      # merge data by date
      suppressWarnings(
        dat <- merge(dat_gsod, dat_chirps, by = "YEARMODA", all.x = TRUE)      
      )
      dat$PRCP[dat$PRCP == 0 & dat$CHIRPS > 0] <- NA
      
      dat_gsod$PRCP <- dat$PRCP
      return(dat_gsod)
    }  
  }
  
  
  # significant trends (p < 0.05); if not significant, NA is returned
  lst_dsn <- lapply(lst_gsod, function(i) {
    # merge station data with continuous time series
    i$YEARMODA <- as.Date(as.character(i$YEARMODA), format = "%Y%m%d")
    dat_mrg <- merge(df_dates, i, by = "YEARMODA", all = TRUE)
    
    ## aggregate by month
    if (interval[1] == "monthly") {
      dat_mrg$YEARMO <- strftime(dat_mrg$YEARMODA, format = "%Y-%m")
      
      dat_mrg %>%
        dplyr::select(YEARMODA, YEARMO, PRCP) %>%
        dplyr::mutate(PRCP = inch2Millimeter(PRCP, 2)) %>%
        dplyr::group_by(YEARMO) %>%
        dplyr::summarize(PRCP = GSODTools:::partialSum(PRCP, monthly_limit = monthly_limit))  %>%
        data.frame() -> dat_agg
      
      dat_agg$MO <- sapply(strsplit(dat_agg$YEARMO, "-"), "[[", 2)    
      
    ## aggregate half months  
    } else if (interval[1] == "bimonthly") {

      dat_mrg$YEARMO <- strftime(dat_mrg$YEARMODA, format = "%Y-%m")
      
      # determine first and second half per month
      days <- sapply(1:nrow(dat_mrg), function(i) {
        if (as.numeric(substr(dat_mrg$YEARMODA[i], 9, 10)) <= 15) {
          return("15a") 
        } else {
          return("15b")
        }
      })
      dat_mrg$YEARMO <- paste(dat_mrg$YEARMO, days, sep = "-")
            
      dat_mrg %>%
        dplyr::select(YEARMODA, YEARMO, PRCP) %>%
        dplyr::mutate(PRCP = inch2Millimeter(PRCP, 2)) %>%
        dplyr::group_by(YEARMO) %>%
        dplyr::summarize(PRCP = GSODTools:::partialSum(PRCP, monthly_limit = monthly_limit))  %>%
        data.frame() -> dat_agg
      
      dat_agg$MO <- substr(dat_agg$YEARMO, 6, 11)    
      
    } 

    ## compute long-term (bi-)monthly means
    dat_agg %>% 
      dplyr::group_by(MO) %>%
      dplyr::summarize(PRCP_LTM = GSODTools:::partialMean(PRCP, valid_months = valid_months)) %>%
      data.frame() -> dat_ltm
    
    ## compute seasonal (i.e., monthly or bi-monthly) anomalies
    dat_mrg <- merge(dat_agg, dat_ltm, by = "MO", sort = FALSE)
    dat_mrg <- dat_mrg[order(dat_mrg$YEARMO), ]
    dat_mrg$PRCP_DSN <- dat_mrg$PRCP - dat_mrg$PRCP_LTM
    
    return(dat_mrg)
  })
  
  # saveRDS(lst_dsn, file = "data/iran_dsn_1982_2013.rds")

  lst_tau <- lapply(lst_dsn, function(i) {  
    # mann-kendall trend
    Rsenal::significantTau(i$PRCP_DSN, prewhitening = prewhitening, 
                           df = TRUE, p = p)
  })
  dat_tau <- do.call("rbind", lst_tau)
  
  ## usaf codes
  ch_usaf <- sapply(lst_gsod, function(i) {
    unique(i[, 1])
  })
  
  df_usaf <- data.frame(USAF = ch_usaf, dat_tau)
  
  ## merge with spatial data
  df_usaf <- merge(updateInventory(), df_usaf, by = "USAF", all.y = TRUE)
  coordinates(df_usaf) <- ~ LON + LAT
  proj4string(df_usaf) <- "+init=epsg:4326"
  
  ## visualize
  if (visualize) {
    # reference extent
    plot(ref_ext)
    # stations with precipitation measurements (grey)
    points(gsod_shp[id_prcp, ], col = "grey75", pch = 4)
    # stations which match user-defined criteria (black)
    points(df_usaf, pch = 4)
    
    # color significantly increasing (blue) and decreasing trends (red)
    df_sign <- df_usaf[df_usaf@data$p < p, ]
    points(subset(df_sign, !is.na(tau) & tau > 0), pch = 4, cex = 1.5, lwd = 2,
           col = "blue")
    points(subset(df_sign, !is.na(tau) & tau < 0), pch = 4, cex = 1.5, lwd = 2,
           col = "red")
  }
  
  ## deregister parallel backend
  if (cores > 1L)
    parallel::stopCluster(supcl)
  
  ## return mann-kendall data
  return(df_usaf)
}