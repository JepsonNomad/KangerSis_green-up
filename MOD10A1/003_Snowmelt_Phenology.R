message("Program initiation:")
message(Sys.time())
timeInit = Sys.time()

#### A script for quantifying snowmelt timing ----

#### Load packages, set wd ----
library(raster)
library(rgdal)
library(stringr)
library(DEoptim)
library(plyr)
library(doParallel)


setwd("PATH/TO/DIR/MOD10A1/")

#### Define parameters ----
set.seed(-3500) # Invention of wheel and plow in Mesopotamia

# Collect system arguments
args <- commandArgs(TRUE)
year = as.character(args[1])
nCores = as.numeric(args[2])
message(paste0("Year selected: ", year))
message(paste("Number of cores requested by R: ", nCores))

# Find raster file
annualcrops = paste0(paste0("NDSI_clip_",year),".tif")
message(paste0("Analyzing raster stack: ", annualcrops))

# noData is the fill value for empty cells of the input raster
# defined in Earth Engine and gdal_translate
noData = -100000
# NDSI threshold required to count a pixel 
# as snow-covered (see Hall et al 2012)
NDSIcutoff = 20 

message(paste0(paste0(
  "NDSI threshold for binarization: ",
  NDSIcutoff), "%"))

# Convert layer Name information into dates
myLayerTable = paste0(paste0("NDSI_LayerNames_", year),
                      ".csv")
layerNames = read.table(myLayerTable, header = F,
                        stringsAsFactors = FALSE)[,1]
layerDates = as.Date(layerNames, format = "%m/%d/%y")
layerDOYs = as.numeric(format(layerDates, format = "%j"))



#### Define functions ----

## A function to measure timing of beginning and end of snow-free season
## Requires a vector with length = 366 with daily snow presence (1) absence (0) data (last element = NA if not a leap year). Example of dataset appropriate for this function is one year of MOD10A1 NDSI for a single pixel, binarized (based on e.g. 20% snow cover such as in Hall et al 2012). 
snowDescriber = function(x){
    # Select only pixels with at least 20 observations in the year and at least 2 snow-free observations
    if(sum(!is.na(x)) > 20 & sum(x == 0, na.rm = T) > 1){ 
      # Create data.frame with binary snow and associated Day of Year
      snowDF = data.frame(SNOW = x,
                          DOY = 1:length(x))
      # NA values are not allowed in the optimizing process
      snowDF = snowDF[complete.cases(snowDF),]
      
      # Use double logistic function to describe temporal snow dynamics
      # Here, baseline is 1 and snow disappears in summer.
      # Using the model fitting code structure of phenex::dLogistic
      
      # The model we want to fit
      dlPredictor = function(xmidS,xmidA,scalS,scalA,DOY){
        SNOW = 
          ((1/(1+exp((xmidA-DOY)/scalA))) -
             (1/(1+exp((xmidS-DOY)/scalS)))) + 1
        return(SNOW)
      }
      # Calculate difference from fitted object and observed values (dlObs)
      dlDelta = function(x, DOY, dlObs){
        SNOW = sum((dlPredictor(xmidS=x[1], xmidA=x[2], 
                                scalS=x[3], scalA=x[4], DOY)-dlObs)^2)
        return(ifelse((is.infinite(SNOW)||is.nan(SNOW)),1e50,SNOW))
      }
      # Optimize for sum of squared errors
      dlModel = DEoptim(fn=dlDelta, DOY=snowDF$DOY, dlObs=snowDF$SNOW,
                        lower = c(1,1,0,0),
                        upper = c(366,366,15,15), 
                        control=list(VTR=0, strategy=1, 
                                     NP=200, itermax = 200, 
                                     trace=FALSE, CR=0.9))
      
      # Access the midpoints of the spring and autumn seasons
      xmidS <- dlModel$optim$bestmem[1]
      xmidA <- dlModel$optim$bestmem[2]
      names(xmidS) = NULL
      names(xmidA) = NULL
      
      # Find midpoint of spring and autumn seasons
      snowPheno = c(xmidS, xmidA)
      
      # Return two-layer array with melt season and snowfall onset
      return(snowPheno)
  }else{
    return(c(NA, NA))
  }
}


#### Load data ----
# Load raster stack and convert to 3-dimensional array
myStack = raster::stack(annualcrops)
message("Raster stack loaded.")
message(Sys.time())
# plot(myStack[[100]])
# names(myStack)
myArray = raster::as.array(myStack)
message("Raster converted to array.")
message(Sys.time())

myArray[myArray==noData] <- NA # Remove -99999 values (GEE Mask flag - gdalwarp converts this to -100000)
message("Array cleaned of noData values.")
message(Sys.time())

#### Measure snowmelt timing ----
# Create output array with appropriate dimensions
annualArray = array(dim = c(dim(myArray)[1], 
                            dim(myArray)[2], 
                            366))
# Sort Extent.Array into daily slices
for(p in (1:dim(myArray)[3])){
  zfinal = layerDOYs[p]
  z = as.numeric(zfinal)
  for(q in (1:nrow(myArray))){
    rowindex = q
    for(r in (1:ncol(myArray))){
      colindex = r
      annualArray[rowindex,
                  colindex,
                  z] = myArray[rowindex,
                               colindex,
                               p]
    }
  }
}
message("Array sorted into daily time bins.")
message(Sys.time())

# Pixels less than 20% NDSI are considered snow-free as was done in Hall et al 2012
annualArray[annualArray < NDSIcutoff] = 0
annualArray[annualArray >= NDSIcutoff] = 1
message("NDSI layer binarized.")
message(Sys.time())

# Look at regional snow cover
annualCoverage = apply(annualArray,
                       MARGIN = 3,
                       FUN = mean,
                       na.rm = T)
regionalSnowTS = data.frame("SnowCoverPercent" = annualCoverage,
                            "DOY" = 1:length(annualCoverage))
write.table(regionalSnowTS, 
            paste0(paste0("data/snowCoverPct_",
                          year), 
                   ".csv"),
            sep = ",",
            col.names = TRUE,
            row.names = FALSE)
message("Annual snow cover time series saved to disk.")
message(Sys.time())


# Create an array containing snowmelt phenology data
# Start off with a small sample to verify it'll work
# sampleArray = annualArray[100:104,310:314,]
# samplyPheno = apply(X = sampleArray,
#                     MARGIN = c(1,2),
#                     FUN = snowDescriber)
# Benchmark
phenologyStart = Sys.time()
# Set up parallel for plyr::aaply
registerDoParallel(cores=nCores)
paropts <- list(.packages = "DEoptim")

phenoArray = plyr::aaply(.data = annualArray,
                         .margins = c(1,2),
                         .fun = snowDescriber,
                         .parallel = TRUE,
                         .paropts = paropts)
saveRDS(phenoArray, paste0(paste0("data/snowPhenology_Array_",
                                  year), 
                           ".RDS"))
message("Snow seasons calculated.")
message(Sys.time())

# Convert snow season array back into a raster object
phenoscape <- raster::brick(x = phenoArray)
# Assign proj4string and extent based on original data
proj4string(phenoscape) = proj4string(myStack)
extent(phenoscape) = extent(myStack)

message("Array converted to raster.")
message(Sys.time())

# Save the data
writeRaster(phenoscape, paste0(paste0("data/snowPhenology_",
                                      year), 
                               ".tif"))
message("Raster saved to disk.")
message(Sys.time())

# Final benchmarking
timeComplete = Sys.time()
elapsedComplete = timeComplete - timeInit
message(paste(paste("Complete processing time:",
                    round(elapsedComplete, digits = 2), sep = " "),
              attributes(elapsedComplete)$units, sep = " "))
