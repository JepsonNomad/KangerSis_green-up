message("Program initiation:")
message(Sys.time())
timeInit = Sys.time()

#### Load packages ----
library(devtools)
library(raster)
library(plyr)
library(dplyr)
library(phenex)
library(rgdal)
library(doParallel)
library(parallel)
library(snow)
library(ggplot2)

set.seed(12150615) # Signing of the Magna Carta!

# Navigate to directory
setwd("PATH/TO/DIR/MOD13Q1/")
# setwd("/Users/christianjohn/Documents/Post_Lab/Data/Kanger/MOD13Q1/")

#### Declare functions ----
source("002_DataFilteringFunctions.R")

print(sessionInfo())
message("Libraries and other functions loaded.")

#### Define parameters ----
bandsPerYear = 23
nYears = 18
nCores = 24

#### Load data ----
args = commandArgs(TRUE)
tileName = args
# tileName = "375_375"
DOY_basename = paste0("MOD13Q1_crop_DOY_", tileName)
NDVI_basename = paste0("MOD13Q1_crop_NDVI_", tileName)  

DOY = raster::stack(paste0(DOY_basename, ".tif"))
NDVI = raster::stack(paste0(NDVI_basename, ".tif"))

# print(str(DOY))
# print(str(NDVI))
message("Datasets loaded.")

#### Apply smoothing and scaling functions ----
# 1. Test with a small time series
# sampleDOY = crop(DOY, extent(NDVI, 130, 134, 210, 215))
# sampleNDVI = crop(NDVI, extent(NDVI, 130, 134, 210, 215))
# sampleWinter = calc(x = sampleNDVI, fun = winterNDVI)
# sampleMedian = calc(x = sampleWinter, fun = medianwindow)
# sampleScaled = calc(x = sampleMedian, fun = ts_scale)
# plot(sampleDOY)
# plot(sampleNDVI[[13]])
# plot(sampleWinter[[13]])
# plot(sampleMedian[[15]])
# plot(sampleScaled[[15]])

# Benchmark
winterstart = Sys.time()
# 2. winterNDVI
beginCluster(nCores)
NDVI.winter = clusterR(NDVI,
                       calc,
                       args=list(winterNDVI)); winend = Sys.time()
endCluster()
message("Winter NDVI calculated and applied to time series.")

# 3a. medianwindow
beginCluster(nCores)
NDVI.median = clusterR(NDVI.winter,
                       calc,
                       args=list(medianwindow)); medend = Sys.time()
endCluster()
message("Moving window filter completed.")

# 3b. ts_scale
beginCluster(nCores)
NDVI.scaled = clusterR(x = NDVI.median,
                       calc,
                       args=list(ts_scale)); scaleend = Sys.time()
endCluster()

# Complete benchmarking, report elapsed time
PreprocessingTime = scaleend-winterstart
message(paste(paste0("Preprocessing elapsed time: ", 
                     round(PreprocessingTime, digits = 2)),
              attributes(PreprocessingTime)$units, sep = " "))

# plot(NDVI.winter[[13]], main = "winter")
# plot(NDVI.median[[13]], main = "median window")
# plot(NDVI.scaled[[13]], main = "scaled NDVI")
message(Sys.time())
message("Vegetation index values filtered and scaled.")


#### Apply phenex across scaled NDVI, DOY annual map pairs ----
AnnualAnalyzer = function(doyRas, ndviRas, 
                          yearLayers, yearIndex, tileName){
  # Generate output naming information
  yearChar = as.character(2000+yearIndex)
  
  message("------------------------------------------------------------")
  message(Sys.time())
  message(paste0("Year: "), yearChar)
  message(paste0("Tile: "), tileName)
  #### Format data for phenex ----
  # Vegetation index
  NDVI.array = as.array(subset(ndviRas, yearLayers))
  # dim(NDVI.array)
  # Day-of-year
  DOY.array = as.array(subset(doyRas,yearLayers))
  DOY.array[DOY.array == 0] <- NA # Remove DOY values where day = 0
  # dim(DOY.array)
  # Complete array with a final dimension for data type (VI vs DOY)
  MODIS_array = array(data = c(NDVI.array, DOY.array),
                      dim = c(dim(NDVI.array),2))
  dim(MODIS_array)
  
  message(Sys.time())
  message("Constructed space-time-dataset hypervolume.")  
  #### Apply phenex approach to model NDVI ----
  
  # Apply phenex::modelNDVI and phenex::phenoPhase via `yearphenology()`
  # Create multidimensional array over which to apply yearphenology
  # Test one pixel
  # yearphenology(x = MODIS_array[10,10,,],
  #               yearLayers = yearLayers,
  #               yearIndex = yearIndex)

  # Benchmark
  phenologyStart = Sys.time()
  # Set up parallel for plyr::aaply
  registerDoParallel(cores=nCores)
  paropts <- list(.packages = "phenex")
  # Apply over array
  phenoScape = plyr::aaply(.data = MODIS_array,
                           .fun = yearphenology,
                           .margins = c(1,2),
                           .parallel = TRUE,
                           .paropts = paropts,
                           yearIndex = yearIndex,
                           yearLayers = yearLayers,
                           approach = "DE")
  # phenoScape = apply(X = MODIS_array,
  #                    FUN = yearphenology,
  #                    MARGIN = c(1,2),
  #                    yearIndex = yearIndex,
  #                    yearLayers = yearLayers)
  phenologyEnd = Sys.time()
  message(Sys.time())
  message(paste(paste("PhenoScape calculated for Tile", 
                      tileName, sep = " "), 
          yearChar, sep = ", "))
  
  # Look at the product
  # message("phenoScape dimensions:")
  # dim(phenoScape)
  
  greenupDay = raster(phenoScape[,,1], template = ndviRas[[1]])
  greenupSD = raster(phenoScape[,,2], template = ndviRas[[1]])
  senescenceDay = raster(phenoScape[,,3], template = ndviRas[[1]])
  senescenceSD = raster(phenoScape[,,4], template = ndviRas[[1]])
  
  phenoRaster = raster::brick(list(greenupDay,
                                   greenupSD,
                                   senescenceDay,
                                   senescenceSD))
  
  message("Array-to-raster conversion complete.")
  
  # Plot results
  pdf(paste(paste(paste("FARM_outputs/PLOT_PhenologyMaps", 
                        tileName, sep = "_"), 
                  yearChar, sep = "_"), 
            ".pdf", sep = ""))
  par(mfrow=c(2,2))
  plot(phenoRaster[[1]], main = "Green-up date")
  plot(phenoRaster[[2]], main = "Green-up rate")
  plot(phenoRaster[[3]], main = "Senescence date")
  plot(phenoRaster[[4]], main = "Senescence rate")
  dev.off()
  
  # Reproject raster to UTM for simplified interpretation
  myproj = CRS("+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  phenoRaster.UTM = projectRaster(phenoRaster, crs = myproj)
  # Look at transformation
  pdf(paste(paste(paste("FARM_outputs/PLOT_PhenologyUTMS", 
                        tileName, sep = "_"), 
                  yearChar, sep = "_"), 
            ".pdf", sep = ""))
  par(mfrow=c(1,2))
  plot(phenoRaster[[1]], main = "Green-up, Sinusoidal")
  plot(phenoRaster.UTM[[1]], main = "Green-up, UTM")
  dev.off()
  
  # Save the data
  outname = paste(paste(paste("FARM_outputs/Kanger_Phenology", 
                              tileName, sep = "_"), 
                        yearChar, sep = "_"), 
                  ".tif", sep = "")
  writeRaster(phenoRaster.UTM, filename = outname)
  message(paste("Raster saved as ", outname, sep = ""))
}

for(i in 1:nYears){
  yearIndex = i
  yearLayers = c(((yearIndex-1)*bandsPerYear+1):(yearIndex*bandsPerYear))
  AnnualAnalyzer(doyRas = DOY,
                 ndviRas = NDVI.scaled,
                 yearIndex = yearIndex,
                 yearLayers = yearLayers,
                 tileName = tileName)
  message(Sys.time())
  message(paste0(paste0(paste0("Completed analyzing year ", i), " of "), nYears))
}

# Tell us when you finished
message("Program completion:")
message(Sys.time())

# Final benchmarking
timeComplete = Sys.time()
elapsedComplete = timeComplete - timeInit
message(paste(paste("Complete processing time:",
                    round(elapsedComplete, digits = 2), sep = " "),
              attributes(elapsedComplete)$units, sep = " "))
