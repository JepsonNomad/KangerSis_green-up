library(rgdal)
library(gdalUtils)
library(raster)

sessionInfo()

# Define parameters
# Simply open a terminal, navigate to directory and run with the only argument being the year as a character
# cd PATH/TO/DIR/MOD13Q1/
# Rscript 910_gdalWarp.R '2001'

args = commandArgs(TRUE)
y = as.character(args[1])

# Identify working directory
mywd = getwd()

yearPattern = paste0(y, ".tif")
searchPattern = paste0("*", yearPattern)
mosaicName = paste0("Kanger_Phenology_Complete_", yearPattern)

print(paste0("Year selected: ", y))
print(paste0("File suffix: ", yearPattern))
print(paste0("Search pattern: ", searchPattern))
print(paste0("Output name: ", mosaicName))

# Find all raster tiles for year y
myrasters = list.files(path = paste0(mywd, "/FARM_outputs"),
                       pattern = searchPattern,
                       full.names = TRUE)
print("File list: ")
print(myrasters)

# Mosaic tiles into a single raster and save it.
gdalwarp(myrasters, mosaicName)

