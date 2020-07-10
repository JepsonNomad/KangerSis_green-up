#### Load libraries, set wd ----

library(tidyverse)
library(MASS)
library(lme4)
library(MuMIn)
library(rgdal)
library(raster)

select = dplyr::select

setwd("PATH/TO/DIR")


#### Define parameters ----

## Repeatability
set.seed(800) # Crowning of Charlemagne


## Spatial 
# Define CRS to be used
utm = CRS("+proj=utm +zone=22 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# ROI
ROI = readOGR("PATH/TO/DIR/Kanger_Caribou_Ranges/",
              "Kanger_Region_SP")
ROI = spTransform(ROI, utm) # Convert to UTM's

# Create spatial object for airport coordinates
Towns.latlon = cbind(c(-50.6946,-53.7221),c(67.0192,66.9520)) # Lat/lon coords for airports
Towns.sp = SpatialPoints(Towns.latlon, proj4string = CRS("+init=epsg:4326")) # Create Spatial* object
Towns.UTM = spTransform(Towns.sp, CRSobj = utm) # Convert to UTM's


#### Import data ----
# Weather
DMI_wide = read.table("PATH/TO/DIR/DMI/vejrarkiv_complete_WIDE.csv",
                      sep = ",",
                      header = TRUE)
head(DMI_wide)
# Elevation
DEM = read.table("PATH/TO/DIR/MOD13Q1/DATA_fortify_DEM.csv",
                 sep = ",",
                 header = TRUE)
head(DEM)
DEM$pixelID = paste0(DEM$x,DEM$y)
# MODIS
MOD = read.table("PATH/TO/DIR/Regional_Phenology/DATA_pixelwiseGreenSnow.csv",
                 sep = ",",
                 header = TRUE,
                 stringsAsFactors = FALSE)
# Turn MOD YearFactor into a factor
MOD$YearFactor = as.factor(MOD$YearFactor)
str(MOD)

#### Data wrangling ----
# Add columns to MODIS data for relative Longitude and Elevation
medEastings = 472788.4
medNorthings = 7401850
MOD$UTM.x.cen = ((MOD$x-medEastings)/1000)
MOD$UTM.y.cen = ((MOD$y-medNorthings)/1000)
MOD$Elevation = DEM$DEM[match(MOD$pixelID,DEM$pixelID)]

## Convert MOD data into a regional aggregate
MOD$SeasonLength = MOD$Senescence-MOD$Greenup
MOD_reg = aggregate(. ~ Year,
                    data = MOD,
                    FUN = function(x){mean(as.numeric(as.character(x)), na.rm = T)}) %>%
  select(-x, -y, -pixelID, -YearFactor)
head(MOD_reg)

## Calculate longitudinal greenup coefficient
myYrs = unique(MOD$Year)
SeeSaw = function(yr, df){
  dataSubset = df %>%
    filter(Year == yr)
  lm_subset = lm(Greenup ~ x, data = dataSubset)
  lm_subset$coefficients[2]
}
lonCoefs = as.vector(unlist(lapply(myYrs, FUN = SeeSaw, df = MOD)))
lcDF = data.frame(Year = myYrs,
                  lonCoef = lonCoefs)

## Join all datasets
mydata_regional_wide = full_join(MOD_reg, DMI_wide, by = "Year")
mydata_regional_wide = full_join(mydata_regional_wide, lcDF, by = "Year")

mydata_regional_wide$wOI = mydata_regional_wide$Greenup - mydata_regional_wide$Snowmelt

mydata_regional_wide %>%
  dplyr::filter(wOI == min(wOI, na.rm = T))
mydata_regional_wide %>%
  dplyr::filter(Snowmelt == min(Snowmelt, na.rm = T))
mydata_regional_wide %>%
  dplyr::filter(wOI > 30)
mydata_regional_wide %>%
  dplyr::filter(Snowmelt == max(Snowmelt, na.rm = T))
#### Statistical analyses ----

#### Results Paragraph 1 ----

### "Mean monthly temperatures preceding and during the growing season (January through June) emerged as strong predictors of green-up timing at the regional scale"
# Greenup vs weather variables
# Regional mean temp in April and May are significant predictors of regional greenup timing
DF_grnTemp = mydata_regional_wide %>% 
  dplyr::select(Greenup, paste0("R.", str_pad(c(1:6), 
                                              pad = "0", 
                                              side = "left", 
                                              width = 2)))
# Green-up vs Jan-Jun monthly regional temp predictors
lm_grnTemp = lm(Greenup ~ .,
                data = DF_grnTemp)
summary(lm_grnTemp)

### "Because of multicollinearity among monthly temperatures, we selected the two strongest predictor months, April and May (which were not correlated; Pearson’s r = 0.18)"
lm_grnStep = stepAIC(lm_grnTemp)
summary(lm_grnStep)
cor(mydata_regional_wide$R.05, 
    mydata_regional_wide$R.04, 
    use="complete.obs") # cor = 0.18

lm_grnApr = lm(Greenup ~ R.04, data = mydata_regional_wide) # Fig 2a
lm_grnMay = lm(Greenup ~ R.05, data = mydata_regional_wide) # Fig 2a
lm_grnSnow = lm(Greenup ~ Snowmelt, data = mydata_regional_wide) # Fig 2a
summary(lm_grnApr) # Fig 2a
summary(lm_grnMay) # Fig 2a
summary(lm_grnSnow) # Fig 2a

### "Mean April and May temperatures were weak predictors of snowmelt timing"
## Abiotic drivers of plant phenology
lm_snowAprMay = lm(Snowmelt ~ R.04 + R.05, data = mydata_regional_wide)
summary(lm_snowAprMay)

### "...but strong predictors of green-up timing"
lm_grnAprMay = lm(Greenup ~ R.04 + R.05, data = mydata_regional_wide)
summary(lm_grnAprMay) # Fig 2a

### "Together with regional snowmelt timing (Fig 2b), April and May temperatures explained 86.3% of variation in green-up timing at the regional scale"
lm_grnAprMaySnow = lm(Greenup ~ R.04 + R.05 + Snowmelt, data = mydata_regional_wide)
summary(lm_grnAprMaySnow)

### "...although regional snowmelt and regional May temperature were correlated"
lm_grnMaySnow = lm(Greenup ~ Snowmelt + R.05, data = mydata_regional_wide)
summary(lm_grnMaySnow) # p < 0.001, R^2 = 0.86
cor(mydata_regional_wide$R.05, mydata_regional_wide$Snowmelt, use="complete.obs") # cor = -0.43
cor.test(mydata_regional_wide$R.05, mydata_regional_wide$Snowmelt, use="complete.obs")
summary(lm(mydata_regional_wide$R.05 ~ mydata_regional_wide$Snowmelt))
# Early snowmelt = early greenup; beta = 0.41
# Warm may = early greenup; beta = -2.17


# Fig 2
# Fig 2a
Fig2a = ggplot(mydata_regional_wide, aes(x = R.05, y = Greenup)) +
  stat_smooth(method = "lm", col = "orange2", se = F, size = 2) +
  geom_point(size = 2) +
  ylab("Green-up timing (day of year)") +
  xlab("Regional mean May temperature (°C)") +
  CJsBasics::BasicTheme
Fig2a
ggsave("AAAR/PLOTfinal_Fig2a.eps", 
       plot = Fig2a, 
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig2a.jpg", 
       plot = Fig2a, 
       width = 5, height = 5, units = "in", dpi = 1200)

# Greenup vs Snowmelt
# Fig 2b
Fig2b = ggplot(mydata_regional_wide, aes(x = Snowmelt, y = Greenup)) +
  stat_smooth(method = "lm", col = "orange2", se = F, size = 2) +
  geom_point(size = 2) +
  ylab("Green-up timing (day of year)") +
  xlab("Snowmelt timing (day of year)") +
  xlim(120,185) + ylim(120,185) +
  geom_abline(slope = 1) +
  coord_equal() +
  CJsBasics::BasicTheme
Fig2b
ggsave("AAAR/PLOTfinal_Fig2b.eps", 
       plot = Fig2b, 
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig2b.jpg", 
       plot = Fig2b, 
       width = 5, height = 5, units = "in", dpi = 1200)
summary(lm(Greenup ~ Snowmelt, data = mydata_regional_wide))



#### Results Paragraph 2 ----

### "At the local (pixel) scale, spring green-up timing was positively associated with elevation, indicating green-up occurred later at higher elevations"
## Importance of elevation
# Green-up progresses over elevation
lm_grnDEM = lm(Greenup ~ Elevation, data = MOD)
summary(lm_grnDEM)

### "This pattern was consistent for all years of the study"
# The greenup vs elevation pattern holds across years
lmer_grnDEM = lmer(Greenup ~ Elevation + (1|Year), data = MOD)
summary(lmer_grnDEM)

### "A regular inland-to-coastal progression of green-up emerged"
lm_grnLon = lm(Greenup ~ UTM.x.cen, data = MOD)
summary(lm_grnLon) # Large longitude (further east) has earlier greenup

### "Green-up timing was also associated with snowmelt timing across the dataset"
# Effect of snowmelt timing on Greenup
lm_grnSnowPIXEL = lm(Greenup ~ Snowmelt, data = MOD)
summary(lm_grnSnowPIXEL)

### "...a relationship that was maintained each year"
lmer_grnSnowPIXEL = lmer(Greenup ~ Snowmelt + (1|Year), data = MOD)
summary(lmer_grnSnowPIXEL)

### "Snowmelt was delayed somewhat with respect to elevation, but the two variables were sufficiently unrelated that both could be used together as predictors"
# Snowmelt timing over elevation
lm_snowDEM = lm(Snowmelt ~ Elevation, data = MOD)
cor(MOD$Snowmelt, MOD$Elevation, use = "complete.obs")
cor.test(MOD$Snowmelt, MOD$Elevation, use = "complete.obs")
# Correlation coefficient between snow and elevation is  0.32; p < 0.001

### "Inclusion of both terms in a linear model explained 39% of variation in green-up timing across the duration of the study"
# Including snow and elevation in one model of greenup
lm_grnSnowDEM = lm(Greenup ~ Elevation + Snowmelt, data = MOD)
summary(lm_grnSnowDEM)
car::vif(lm_grnSnowDEM) # low variance inflation factor

# Pattern holds across years
lmer_grnSnowDEM = lmer(Greenup ~ Elevation + Snowmelt + (1|Year), data = MOD)
summary(lmer_grnSnowDEM)


## Fig 3
# Fig 3a
Fig3a = ggplot(MOD,
               aes(x = Elevation, y = Greenup)) +
  geom_point(size = 0.001) +
  stat_smooth(method = "lm", size = 2, col = "orange2") +
  xlab("Elevation (m)") +
  ylab("Green-up timing (day of year)") +
  xlim(0,1700) +
  CJsBasics::BasicTheme
Fig3a
ggsave("AAAR/PLOTfinal_Fig3a.eps",
       Fig3a,
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig3a.jpg",
       Fig3a,
       width = 5, height = 5, units = "in", dpi = 1200)

# Fig 3b
Fig3b = ggplot(MOD,
               aes(x = Snowmelt, y = Greenup)) +
  geom_point(size = 0.01) +
  geom_abline(slope = 1) +
  stat_smooth(method = "lm", size = 2, col = "orange2") +
  xlab("Snowmelt timing (day of year)") +
  ylab("Green-up timing (day of year)") +
  CJsBasics::BasicTheme
Fig3b
ggsave("AAAR/PLOTfinal_Fig3b.eps",
       Fig3b,
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig3b.jpg",
       Fig3b,
       width = 5, height = 5, units = "in", dpi = 1200)

# Fig S1
FigS1 = ggplot(MOD,
               aes(x = Elevation, y = Greenup)) +
  geom_point(size = 0.1) +
  facet_wrap(~ Year) +
  stat_smooth(method = "lm", col = "orange2") +
  xlab("Elevation (m)") +
  xlim(0,1700) +
  ylab("Green-up timing (day of year)") +
  CJsBasics::BasicTheme
FigS1
ggsave("AAAR/PLOTfinal_FigS1.eps",
       FigS1,
       width = 12, height = 8, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_FigS1.jpg",
       FigS1,
       width = 12, height = 8, units = "in", dpi = 1200)

# Fig S2
FigS2 = ggplot(MOD,
               aes(x = UTM.x.cen, y = Greenup)) +
  geom_point(size = 0.1) +
  facet_wrap(~ Year) +
  stat_smooth(method = "lm", col = "orange2") +
  xlim(-100,100) +
  xlab("Eastings (km)") +
  ylab("Green-up timing (day of year)") +
  CJsBasics::BasicTheme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
FigS2
ggsave("AAAR/PLOTfinal_FigS2.eps",
       FigS2,
       width = 12, height = 8, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_FigS2.jpg",
       FigS2,
       width = 12, height = 8, units = "in", dpi = 1200)

# Fig S3
FigS3 = ggplot(MOD,
               aes(x = Snowmelt, y = Greenup)) +
  geom_point(size = 0.1) +
  geom_abline(slope = 1) +
  stat_smooth(method = "lm", col = "orange2") +
  xlim(0,365) +
  ylim(0,365) +
  xlab("Snowmelt timing (day of year)") +
  ylab("Green-up timing (day of year)") +
  facet_wrap(~ Year) +
  CJsBasics::BasicTheme
FigS3
ggsave("AAAR/PLOTfinal_FigS3.eps",
       FigS3,
       width = 12, height = 8, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_FigS3.jpg",
       FigS3,
       width = 12, height = 8, units = "in", dpi = 1200)



#### Results paragraph 3 ----
### First, prep data for paragraphs 3 and 4:
## Make data selection 
spatialResultsLocs = unique(MOD[complete.cases(MOD),]$pixelID)
spatialResultsMOD = MOD %>%
  filter(pixelID %in% spatialResultsLocs)
spatialResultsMOD

## Convert to list format
# This function breaks a dataframe into a list of interannual dataframes by pixelID
dfBreaker = function(df = spatialResultsMOD){
  # Generate ID's for each pixel position
  myIDS = unique(df$pixelID)
  # A function to subset a data.frame according to Pixel ID
  sampleDF = function(ID, y){
    # Use a data.frame y of observations
    y %>% 
      # Select just observations from ID pixel from the data.frame y
      filter(pixelID == ID) 
  }
  # Generate a list where each element is one pixel's total phenology observations
  phenologyList = lapply(myIDS,
                         FUN = sampleDF,
                         y = df)
  return(return(phenologyList))
}
# Generate a list of pixel-wise green-up observations as described in dfBreaker function
spatialResultsList = dfBreaker(spatialResultsMOD)

## Calculate distance from pixel to weather station locations
# This function returns the distance from a pixel to Kanger and Sisimiut airports respectively
distFinder = function(df, myPoints, crsChoice = utm){
  # Extract location of pixel
  loc.x = df$x[1]
  loc.y = df$y[1]
  loc = cbind(loc.x,loc.y)
  # Convert to spatial object
  loc.sp = SpatialPoints(loc, proj4string = crsChoice)
  # Find distance from each element of myPoints (in this case, Kanger and Sisimiut)
  myDistances = pointDistance(loc.sp, myPoints, lonlat = FALSE)
  return(c(loc.x,loc.y,myDistances))
}

# Apply function to list of dataframes created above
pixelDists = lapply(spatialResultsList,
                    FUN = distFinder,
                    myPoints = Towns.UTM)

# Bind distances into a data.frame.
pixelDists = do.call("rbind",pixelDists) %>%
  as.data.frame()
names(pixelDists) = c("UTM.x","UTM.y","dist.K", "dist.S")
head(pixelDists)

## Measure predictive power of each weather station for pixel green-up
head(DMI_wide)
corrFinder = function(x = spatialResultsList[[2]], y = DMI_wide){
  # For a given pixel (spatialResultsList[[x]]), join the total wide weather data by year
  masterDF = full_join(x, y, by = "Year")
  # Create a linear model to predict greenup with Kanger May temp
  pixelKlm = lm(Greenup ~ K.05, data = masterDF)
  # Create a linear model to predict greenup with Sisimiut May temp
  pixelSlm = lm(Greenup ~ S.05, data = masterDF)
  # Create a linear model to predict greenup with mean Regional May temp
  pixelRlm = lm(Greenup ~ R.05, data = masterDF)
  # Create a linear model to predict greenup with colocated snowmelt timing
  pixelSnow = lm(Greenup ~ Snowmelt, data = masterDF)
  # Extract R^2 values for each model
  rK = summary(pixelKlm)$r.squared
  rS = summary(pixelSlm)$r.squared
  rR = summary(pixelRlm)$r.squared
  rSnow = summary(pixelSnow)$r.squared
  # Find difference in AIC between Kanger and Sisimiut (needed for Fig S5)
  dAIC = AIC(pixelKlm) - AIC(pixelSlm)
  # Return each of the R^2 and dAIC vals in a row format
  return(c("r.K" = rK, "r.S" = rS, "r.R" = rR, 
           "r.Snow" = rSnow, "dAIC" = dAIC))
}
# Apply corrFinder to each pixel
pixelCorrs = lapply(spatialResultsList,
                    FUN = corrFinder)
# rbind the results into a single data.frame
pixelCorrs = do.call("rbind", pixelCorrs)

## Measure predictive power of met station data
distCorrs = cbind(pixelDists, pixelCorrs)
meanX = mean(distCorrs$UTM.x)
meanY = mean(distCorrs$UTM.y)
distCorrs$UTM.x.cen = (distCorrs$UTM.x-medEastings)/1000 # Note this is now in Kilometers
distCorrs$UTM.y.cen = (distCorrs$UTM.y-medNorthings)/1000 # Note this is now in Kilometers
summary(distCorrs)
head(distCorrs)
## Fig 4 dataframe - subset and rename
R2plotsDF = distCorrs %>%
  gather(key = "Predictor", value = "R2",
         r.K,r.S,r.R,r.Snow)
head(R2plotsDF)
R2plotsDF$Predictor = ifelse(R2plotsDF$Predictor == "r.K",
                             "May temperature in Kangerlussuaq",
                             ifelse(R2plotsDF$Predictor == "r.S",
                                    "May temperature in Sisimiut",
                                    ifelse(R2plotsDF$Predictor == "r.R",
                                           "Mean regional May temperature",
                                           "Snowmelt timing (day of year)")))
df_Fig4 = filter(R2plotsDF,
                 Predictor != "May temperature in Kangerlussuaq" &
                   Predictor != "May temperature in Sisimiut")

############################################
#### END OF WRANGLING FOR PARAGRAPHS 3 AND 4
############################################

### "The predictive power (R2) of temperature on green-up timing was inversely related to that of snowmelt timing on green-up timing"
lm_rsnowrmayPIXEL = lm(r.Snow ~ r.R, data = distCorrs)
summary(lm_rsnowrmayPIXEL) # Across the region there is a negative relationship between explanatory power of May temperature and snowmelt for interannual variation in green-up timing.

### "May temperature in Kangerlussuaq was a consistently better predictor of green-up timing than May temperature in Sisimiut, especially for areas farther inland"
lm_dAICUTM = lm(dAIC ~ UTM.x.cen, data = distCorrs)
summary(lm_dAICUTM)


## Fig 4
# Fig 4a
Fig4a = ggplot(df_Fig4 %>% filter(Predictor == "Mean regional May temperature"),
               aes(x = UTM.x.cen, y = UTM.y.cen)) +
  geom_tile(aes(fill = R2)) +
  coord_equal() +
  scale_fill_viridis_c(name = expression(R^2), 
                       begin = 0, end = 1, limits=c(0, 1), 
                       option = "D") +
  xlab("Eastings (km)") +
  ylab("Northings (km)") +
  geom_point(aes(x=(Towns.UTM@coords[1,1]-medEastings)/1000, 
                 y=(Towns.UTM@coords[1,2]-medNorthings)/1000), 
             shape = 25,
             size = 4,
             fill="orange2",
             stroke = 2) +
  geom_point(aes(x=(Towns.UTM@coords[2,1]-medEastings)/1000, 
                 y=(Towns.UTM@coords[2,2]-medNorthings)/1000), 
             shape = 21,
             size = 4,
             fill="orange2",
             stroke = 2) +
  geom_polygon(data = fortify(ROI),
               aes(x = (long-medEastings)/1000, 
                   y = (lat-medNorthings)/1000),
               fill = "transparent", col = "grey40") +
  # coord_equal() +
  # xlab("Longitude (km)") +
  # ylab("Latitude (km)") +
  scale_y_continuous(breaks=c(-50,0,50)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(colour = "black", size = 0.5))
Fig4a
ggsave("AAAR/PLOTfinal_Fig4a.eps", 
       plot = Fig4a, 
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig4a.jpg", 
       plot = Fig4a, 
       width = 5, height = 5, units = "in", dpi = 1200)

# Fig 4b
Fig4b = ggplot(df_Fig4 %>% filter(Predictor == "Snowmelt timing (day of year)"),
               aes(x = UTM.x.cen, y = UTM.y.cen)) +
  geom_tile(aes(fill = R2)) +
  coord_equal() +
  scale_fill_viridis_c(name = expression(R^2), 
                       begin = 0, end = 1, limits=c(0, 1), 
                       option = "D") +
  xlab("Eastings (km)") +
  ylab("Northings (km)") +
  geom_point(aes(x=(Towns.UTM@coords[1,1]-meanX)/1000, 
                 y=(Towns.UTM@coords[1,2]-meanY)/1000), 
             shape = 25,
             size = 4,
             fill="orange2",
             stroke = 2) +
  geom_point(aes(x=(Towns.UTM@coords[2,1]-meanX)/1000, 
                 y=(Towns.UTM@coords[2,2]-meanY)/1000), 
             shape = 21,
             size = 4,
             fill="orange2",
             stroke = 2) +
  geom_polygon(data = fortify(ROI),
               aes(x = (long-medEastings)/1000, 
                   y = (lat-medNorthings)/1000),
               fill = "transparent", col = "grey40") +
  # coord_equal() +
  # xlab("Longitude (km)") +
  # ylab("Latitude (km)") +
  scale_y_continuous(breaks=c(-50,0,50)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.background = element_rect(colour = "black", size = 0.5))
Fig4b
ggsave("AAAR/PLOTfinal_Fig4b.eps", 
       plot = Fig4b, 
       width = 5, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_Fig4b.jpg", 
       plot = Fig4b, 
       width = 5, height = 5, units = "in", dpi = 1200)


## Fig S4
df_FigS4 = filter(R2plotsDF,
                  Predictor != "Mean regional May temperature" &
                    Predictor != "Snowmelt timing (day of year)")
FigS4 = ggplot(df_FigS4,
               aes(x = UTM.x.cen, y = UTM.y.cen)) +
  geom_tile(aes(fill = R2)) +
  facet_wrap(~Predictor) +
  coord_equal() +
  scale_fill_viridis_c(name = expression(R^2), 
                       begin = 0, end = 1, limits=c(0, 1), 
                       option = "D") +
  xlab("Eastings (km)") +
  ylab("Northings (km)") +
  geom_point(aes(x=(Towns.UTM@coords[1,1]-meanX)/1000, 
                 y=(Towns.UTM@coords[1,2]-meanY)/1000), 
             shape = 25,
             size = 4,
             fill="orange2",
             stroke = 2) +
  geom_point(aes(x=(Towns.UTM@coords[2,1]-meanX)/1000, 
                 y=(Towns.UTM@coords[2,2]-meanY)/1000), 
             shape = 21,
             size = 4,
             fill="orange2",
             stroke = 2) +
  ggthemes::theme_tufte() +
  theme(axis.line = element_line(size = 0.1),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(size = 14,family = "Arial"),
        axis.text = element_text(size = 12, family = "Arial"),
        axis.title = element_text(size = 14, family = "Arial"),
        legend.text = element_text(size = 12, colour = "grey30", family = "Arial"),
        legend.title = element_text(size = 14, family = "Arial"))
FigS4
ggsave("AAAR/PLOTfinal_FigS4.eps", 
       plot = FigS4, 
       width = 12, height = 5, units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_FigS4.jpg", 
       plot = FigS4, 
       width = 12, height = 5, units = "in", dpi = 1200)


## Fig S5
FigS5 = ggplot(distCorrs,
               aes(x = UTM.x.cen, y = dAIC)) +
  geom_text(x = -100, y = 8, hjust = 0, 
            col = "grey60",
            label = "Sisimiut is a better predictor") +
  geom_text(x = -100, y = -15, hjust = 0,
            col = "grey60",
            label = "Kangerlussuaq is a better predictor") +
  geom_hline(yintercept = 0, lty = 1) +
  geom_hline(yintercept = 2, lty = 2) +
  geom_hline(yintercept = -2, lty = 2) +
  geom_point(size = 0.01) +
  stat_smooth(method= "lm", lwd = 2, col = "orange2") +
  xlim(-100,102) +
  xlab("Eastings (km)") +
  ylab(expression(Delta*'AICc')) +  
  CJsBasics::BasicTheme 
ggsave("AAAR/PLOTfinal_FigS5.eps", 
       plot = FigS5, width = 5, height = 5,
       units = "in", dpi = 1200)
ggsave("AAAR/PLOTfinal_FigS5.jpg", 
       plot = FigS5, width = 5, height = 5,
       units = "in", dpi = 1200)
summary(lm(dAIC ~ UTM.x.cen, data = distCorrs))



#### Results paragraph 4 ----

### "The regional balance of green-up (measured as the slope of a linear model of green-up vs. longitude; see Fig S2) was not significantly affected by the difference in May temperature between Kangerlussuaq and Sisimiut"
DF_coefTemp = mydata_regional_wide %>% 
  select(lonCoef, contains("D."))
DF_coefTemp = DF_coefTemp[,1:7]
lm_coefTemp = lm(lonCoef ~ .,
                 data = DF_coefTemp)
summary(lm_coefTemp)
summary(stepAIC(lm_coefTemp))

### "Instead, while temperature was more predictive of inland green-up timing than coastal green-up timing"
lm_gradientTemp = lm(r.R ~ UTM.x.cen, data = distCorrs)
summary(lm_gradientTemp)

### "...snowmelt timing (which was not strongly related to temperature) was a more important predictor for green-up timing near the coast than it was farther inland"
lm_gradientSnow = lm(r.Snow ~ UTM.x.cen, data = distCorrs)
summary(lm_gradientSnow)

## Note that Fig S2 is referenced previously in results paragraph 1.
