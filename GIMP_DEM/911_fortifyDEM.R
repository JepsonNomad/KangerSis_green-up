library(raster)
library(tidyverse)
library(RStoolbox)
library(egg)
library(rgdal)
sessionInfo()

#### Define parameters ----

# Simply open a terminal, navigate to directory and run with the only argument being the year as a character
# cd PATH/TO/DIR/MOD13Q1
# Rscript 911_fortifyDEM.R

setwd("PATH/TO/DIR/MOD13Q1")
# yearPattern = "2018"
mosaicName = "Kanger_Phenology_Complete_2001.tif"
print(mosaicName)

DEM = raster("DEM_proj.tif")
proj4string(DEM)

#### Load data ----
# Sample raster for reprojecting
sampleRas = raster("PATH/TO/DIR/MOD10A1/Kanger_Snowmelt_Complete_2001.tif")

# Project data to match everything else
DEM = projectRaster(DEM, sampleRas)

#### Data formatting ----

# Fortify the raster
z = fortify(DEM)
str(z)
head(z)
names(z)
names(z) = c("x", "y","DEM")
head(z)

# Save gathered fortification as a dataset.
write.table(z,
            file = "DATA_fortify_DEM.csv",
            sep = ",",
            row.names = FALSE, 
            col.names = TRUE)
