
#### Load packages, set wd ----
library(tidyverse)
library(lme4)

setwd("PATH/TO/DIR/")

#### Import data ----
# Phenology data
plantPheno_files = list.files(path = "./MOD13Q1/", pattern = "DATA_fort", full.names = T)
snowPheno_files = list.files(path = "./MOD10A1/", pattern = "DATA_fort", full.names = T)


#### Wrangle data ----
plantPheno_raw = lapply(plantPheno_files,
                        FUN = read.table,
                        header = T,
                        sep = ",")
snowPheno_raw = lapply(snowPheno_files,
                        FUN = read.table,
                        header = T,
                        sep = ",")

# rbind all years into single data.frame
plantPheno_allYears = do.call("rbind", plantPheno_raw)
snowPheno_allYears = do.call("rbind", snowPheno_raw)

# Spread datasets by measurement
plantPheno_wide = plantPheno_allYears %>%
  spread(key = Measurement, value = Timing)
snowPheno_wide = snowPheno_allYears %>%
  spread(key = Measurement, value = Timing)
head(plantPheno_wide)
head(snowPheno_wide)

# Join datasets by pixel and year
mydata = full_join(plantPheno_wide, snowPheno_wide,
                   by = c("x","y","Year"))
mydata$YearFactor = as.factor(mydata$Year)
mydata$pixelID = paste0(mydata$x,mydata$y)
head(mydata)

write.table(mydata, 
            "PATH/TO/DIR/Regional_Phenology/DATA_pixelwiseGreenSnow.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE)

