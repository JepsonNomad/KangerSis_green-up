## John, Miller, and Post 2020

The code in this repository can be used to reproduce the figures and results presented in our accepted manuscript entitled:

*Regional variation in green-up timing along a caribou migratory corridor: spatial associations with snowmelt and temperature*

Currently, a postprint of the manuscript can be found on EcoEvoRxiv at [this link](https://ecoevorxiv.org/xmd69/)

## Instructions

The code is arranged according to data products used in the study. Code is saved using a consistent filename convention for ease of reproduction. "001_*" should be used to access and download all data.

The core datasets used in this analysis were the MOD10A1 snow product and MOD13Q1 vegetation index (NDVI) product. Please follow the workflow in the order of filenames (some numeric prefixes are absent because they represented unrelated or preliminary analyses).

### Snowmelt Phenology (subdirectory `/MOD10A1/`)

To measure snowmelt timing, we downloaded MOD10A1 data using Google Earth Engine. Data can be accessed and downloaded by copying and pasting the `001_MOD10A1_DL.txt` script into the GEE code editor. Files will be saved to your Google Drive account. The ROI for cropping and subsequent analyses can be found in `/Kanger_Caribou_Ranges/Kanger_Region_SP.shp`.

NDSI data were binarized using a 0.2 NDSI cutoff, with values above 0.2 set to 1 and below set to 0. A double logistic function was then fit to the time series for each MOD10A1 pixel using the snowDescriber function in `003_Snowmelt_Phenology.R`. This script is called directly via the shell script `901_snowPheno.sh`, which in turn was called in a SLURM environment for parallel computing with the commands in `902_submitjobs.txt`. Code can be adjusted as needed to work on a personal computer. Raster data were then warped and fortified to generate a table of snowmelt and snowfall phenology metrics using `910_gdalWarp.R` and `911_fortify.R`, and their respective execute shell scripts.

### NDVI Phenology (subdirectory `/MOD13Q1/`)

Vegetation phenology was indexed using a double logistic function described in [Bischof et al 2012](https://www.jstor.org/stable/10.1086/667590?seq=1#metadata_info_tab_contents). MOD13Q1 data were downloaded using Google Earth Engine with the contents of `001_MOD13Q1_DL.txt`. A projection issue in the data existed at the time of download, and if that issue persists can be corrected using the `003_MOD-ExtentFix.R` script. Finally, the MOD13Q1 data were tiled to save computing requirements per raster dataset; tiling parameters are  defined in `004_rasterTile.py` and tiling is executed using the code in `004_rasterTile_bash.txt`.

Functions for preprocessing the NDVI data are in `002_DataFilteringFunctions.R` and are called down the line in `101_phenomap.R`, where the derivation of phenology indices also occurs. The phenomap function is executed in the shell script `901_phenology1.sh` and needs to be performed for each tile generated in `004_...`. Because computations were carried out on a remote computing cluster, the phenology shell scripts are executed with `902_submitjonbs.txt`, which again must be called for each tile. Phenology raster products can then be warped to a common CRS and fortified using the same approach described above for snowmelt.

### Bringing everything together (subdirectory `/Regional_Phenology/`)

Fortified data from the MOD10A1 and MOD13Q1 datasets are combined using `100_AAAR_phenologyWrangling.R`. Analyses and figures from our manuscript can be recreated using `999_AAAR_FinalResults.R`.




