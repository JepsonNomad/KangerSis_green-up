library(raster)
library(tidyverse)
library(RStoolbox)
library(egg)
library(rgdal)

sessionInfo()

#### Define parameters ----

# Simply open a terminal, navigate to directory and run with the only argument being the year as a character
# cd PATH/TO/DIR/MOD10A1
# Rscript 911_fortify.R '2006'

args = commandArgs(TRUE)
yearPattern = as.character(args[1])
# yearPattern = "2001"
# yearPattern = paste0(y, ".tif")
mosaicName = paste0(paste0("Kanger_Snowmelt_Complete_", 
                    yearPattern), ".tif")
print(mosaicName)


#### Load data ----
# Phenology raster
x = raster::brick(mosaicName)

# Mask layer from GIMP
oceanice = brick("PATH/TO/GIMP_OceanIce/GIMP.tif")
oceanice = projectRaster(oceanice, x)

# Water mask shapefile
mywater = readOGR(dsn = "PATH/TO/DIR/","GRL_water_areas_dcw")
mywater = spTransform(mywater, CRSobj = crs(x))

#### Data formatting ----
# Mask data in x based on GIMP snow and ice mask
x[oceanice[[1]] != 0] <- NA
x[oceanice[[2]] != 0] <- NA

# Remove waterways
x = mask(x, mywater, inverse = T)

# Fortify the raster
z = fortify(x)
str(z)
head(z)
names(z)
names(z) = c("x", "y","Snowmelt", "Snowfall")
head(z)
# Gather the data into a single long-format data.frame
zGath = gather(z, key = "Measurement", value = "Timing",
               Snowmelt, Snowfall)
# Add a column for year
zGath$Year = yearPattern
head(zGath)

# Save gathered fortification as a dataset.
write.table(zGath,
            file = paste0(paste0("DATA_fortify_", yearPattern),
                          ".csv"), 
            sep = ",",
            row.names = FALSE, col.names = TRUE)

