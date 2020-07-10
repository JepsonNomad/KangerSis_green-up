# Due to extent issue in Earth Engine export
# This script resets spatial metadata on raster objects to reflect correct projection information.

library(raster)
library(rgdal)
library(stringr)

setwd("PATH/TO/DIR/MOD13Q1/")

#### Projection information ----
MOD1 = raster("MOD13Q1_comp_DOY.tif", 11) # choose a random raster to get crs

ROI = readOGR(".","Kanger_Region_SP") # load shapefile
ROIprojM = spTransform(ROI, CRSobj = proj4string(MOD1))
t_ext = extent(ROIprojM) # target extent object

t_ext

#### Extent fix done using gdal in the terminal ----
"
cd PATH/TO/DIR/MOD13Q1/

gdal_translate -a_ullr -2434880 7477786 -2166206 7315551 MOD13Q1_comp_NDVI.tif MOD13Q1_stack_NDVI.tif

gdal_translate -a_ullr -2434880 7477786 -2166206 7315551 MOD13Q1_comp_DOY.tif MOD13Q1_stack_DOY.tif

gdal_translate -a_ullr -2434880 7477786 -2166206 7315551 MOD13Q1_comp_QA.tif MOD13Q1_stack_QA.tif
"

#### Plot results ----
myraster = raster("MOD13Q1_stack_NDVI.tif", 11)
plot(myraster)
lines(ROIprojM)

