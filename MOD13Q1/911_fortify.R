library(raster)
library(tidyverse)
library(RStoolbox)
library(egg)
library(rgdal)
sessionInfo()

#### Define parameters ----

# Simply open a terminal, navigate to directory and run with the only argument being the year as a character
# cd /Users/christianjohn/Documents/Post_Lab/Data/Kanger/MOD13Q1
# Rscript 911_fortify.R '2006'

# setwd("PATH/TO/DIR/MOD13Q1")
args = commandArgs(TRUE)
yearPattern = as.character(args[1])
# yearPattern = "2018"
mosaicName = paste0(paste0("Kanger_Phenology_Complete_", 
                    yearPattern), ".tif")
print(mosaicName)


#### Load data ----
# Sample raster for reprojecting
sampleRas = raster("PATH/TO/DIR/MOD10A1/Kanger_Snowmelt_Complete_2001.tif")

# Mask layer from GIMP
oceanice = brick("PATH/TO/DIR/GIMP_OceanIce/GIMP.tif")
oceanice = projectRaster(oceanice, sampleRas)

x = raster::brick(mosaicName)
x = projectRaster(x, sampleRas)

# Water mask shapefile
mywater = readOGR(dsn = "PATH/TO/DIR/GL_water/","GRL_water_areas_dcw")
mywater = spTransform(mywater, CRSobj = crs(x))

#### Data formatting ----
# Mask data in x based on GIMP snow and ice mask
# For some reason it fails if you try once but after the 3rd attempt it works:
x[oceanice[[1]] != 0] <- NA
x[oceanice[[1]] != 0] <- NA
x[oceanice[[1]] != 0] <- NA

x[oceanice[[2]] != 0] <- NA

# Remove waterways
x = mask(x, mywater, inverse = T)

# Fortify the raster
z = fortify(x)
str(z)
head(z)
names(z)
names(z) = c("x", "y","Greenup", "Green_rate", "Senescence", "Sen_rate")
head(z)
# Gather the data into a single long-format data.frame
zGath = gather(z, key = "Measurement", value = "Timing",
              Greenup, Green_rate, Senescence, Sen_rate)
# Add a column for year
zGath$Year = yearPattern
head(zGath)

# Save gathered fortification as a dataset.
write.table(zGath,
            file = paste0(paste0("DATA_fortify_", yearPattern),
                          ".csv"), 
            sep = ",",
            row.names = FALSE, col.names = TRUE)
