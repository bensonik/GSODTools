#' Download and Process Global Surface Summary of the Day Data
#'
#' We provide a set of functions to retrieve information about Global Surface 
#' Summary of the Day (GSOD) weather stations currently available online; 
#' download and pre-process datasets; and perform various operations in the 
#' context of time series analysis.
#'
#' @name GSODTools-package
#' @aliases GSODToolspackage
#' @docType package
#' @title Download and Process Global Surface Summary of the Day Data
#' @author Florian Detsch
#'
#' @import methods rgdal raster sp dplyr foreach scales ggplot2
#' @importFrom gmt geodist
#' @importFrom RColorBrewer brewer.pal
#' @importFrom reshape2 melt
#' @importFrom rworldmap countriesLow
#' @importFrom zoo zoo read.zoo
#'
#' @references NOAA (2016) Global Surface Summary of the Day, 
#' \url{https://data.noaa.gov/dataset/global-surface-summary-of-the-day-gsod}.
#'
#' @keywords package
#'
NULL
