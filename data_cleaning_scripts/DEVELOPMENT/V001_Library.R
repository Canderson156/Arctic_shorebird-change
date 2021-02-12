#Set operating system filepath for data file


OS <- .Platform$OS.type

if (OS == "unix"){
  data_path <- "/Volumes/LaCie/PhD/Data/" # MAC file path
  lib_path <- "/Library/Frameworks/R.framework/Versions/4.0/Resources/library"
} else if (OS == "windows"){
  data_path <- "I:/PhD/Data/" # windows file path
  lib_path <- "C:/Users/andersonch/Documents/R/win-library/4.0"
} else {
  print("ERROR: OS could not be identified")
}


##### LOAD PACKAGES

#check for updates
options(pkgType = "binary")
update.packages(lib.loc = lib_path, checkBuilt = TRUE, ask = FALSE)

library(lmtest)
library(nlme)
library(ggfortify)
library(TTR)
library(changepoint)
library(kableExtra)
library(lmerTest)
library(tictoc)
library(terra)
library(raster)
library(Rmisc)
library(miceadds)
library(arm)
library(lme4)
library(ggplot2)
library(sf)
library(tidyverse)
library(rgdal)






#library(mapedit)
#library(adehabitatHR)
#library(lubridate)
#library(reshape2)
#library(beepr)
#library(rgdal)
#library(sp)
#library(MASS)
#library(rgeos)
#library(maptools)
#library(reshape)
#library(SDMTools)
#library(velox)
#library(gdalUtils)
#library(plyr)
#library(corrplot)
#library(scales)
#library(units)
#library(stringi)
#library(lwgeom)
#library(measurements)
#library(plotKML)
#library(tmaptools)
#library(GGally)
#library(knitr)
#library(lavaan)
#library(standardize)
#library(caret)
#library(pROC)
#library(lmtest)
#library(car)
#library(effects)
#library(psycho)
#library(landscapemetrics)
#library(installr)
#library(RColorBrewer)
#library(vcdExtra)
#library(ResourceSelection)
#library(corrplot)
#library(MuMIn)
#library(lavaanPlot)




#### settings

options(scipen=999) #disable scientific notation
options(stringsAsFactors = FALSE) #disable strings as factors



#### source custom functions
source.all(paste(data_path, "R_functions/", sep = ""))



##### OBJECTS SPECIFIC TO THIS PROJECT


## Coordinate reference systems

### need to update these to be in WKT / PROJ6 format


#NPLAEA <-  CRS("+init=EPSG:3573")
#AEA <- CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +ellps=GRS80 +units=m +no_defs")
#WGS84 <- CRS("+init=EPSG:4326")
NAD83 <- CRS("+init=EPSG:4269")



