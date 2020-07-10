library(rgdal)
library(gdalUtils)
library(raster)

# setwd("PATH/TO/DIR/MOD10A1")

sessionInfo()

# Define parameters
# Simply open a terminal, navigate to directory and run with the only argument being the year as a character
# Rscript 910_gdalWarp.R '2001'

args = commandArgs(TRUE)
y = as.character(args[1])
# y = 2001
# Identify working directory
mywd = getwd()

yearPattern = paste0(y, ".tif")
searchPattern = paste0("*", yearPattern)
mosaicName = paste0("Kanger_Snowmelt_Complete_", yearPattern)
targetCRS = "+proj=utm +zone=22 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

print(paste0("Year selected: ", y))
print(paste0("File suffix: ", yearPattern))
print(paste0("Search pattern: ", searchPattern))
print(paste0("Output name: ", mosaicName))

# Find all raster tiles for year y
myrasters = list.files(path = paste0(mywd, "/data"),
                       pattern = searchPattern,
                       full.names = TRUE)
print("File list: ")
print(myrasters)

# Convert rasters to UTM for comparison with NDVI phenology data.
gdalwarp(myrasters, mosaicName, t_srs = targetCRS)

