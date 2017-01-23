###########################
#  NSERP FORAGE QUALITY   #
#  2013 PREDICTIVE MODEL  #
# to compare with 2014/15 #
#    KRISTIN BARKER       #
#        JAN 2017         #
###########################


#################
####  Setup  ####
#################

#################
#### Setup   ####
#################

library(raster)
library(rgdal) # read/write shp's
library(ggplot2)
library(AICcmodavg)
library(dplyr) #general awesomeness
library(stats)

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Vegetation"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Vegetation"

if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
  wd <- wd_workcomp
} else {
    setwd(wd_laptop)
    wd <- wd_laptop
}
rm(wd_workcomp, wd_laptop)

#################
#### Rasters ####
#################


# fix up and add to covs2013:
# landcov(esp), ndvi_amp, precip

# define filepath to folder containing original rasters
rasterpath <- paste(getwd(), "/writtenrasters/orig/2012-2013data/formodel", sep="")
allrasters <- list.files(rasterpath, pattern = "tif$") 

# read in rasters that need fixing up; name as covariates
landcov_13 <- raster(paste(rasterpath, "esp6_13.tif", sep="/")) 
ndvi_amp_13 <- raster(paste(rasterpath, "ndvi_amp_2013.tif", sep="/"))
precip_13 <- raster(paste(rasterpath, "precip_2013.tif", sep="/")) 

# read in rasters already fixed up
elev <- raster(paste(getwd(), "/writtenrasters/covs2014/elev.tif", sep="")) 
radn <- raster(paste(getwd(), "/writtenrasters/covs2014/radn.tif", sep="")) 
slope <- raster(paste(getwd(), "/writtenrasters/covs2014/slope.tif", sep="")) 

# crop new rasters to study area; match extents and resolutions to enable stacking
boundbox <- extent(elev)
landcov_13 <- crop(landcov_13, boundbox)
landcov_13 <- resample(landcov_13, elev, "ngb")
landcov_13[landcov_13 == 0] <- NA
ndvi_amp_13 <- crop(ndvi_amp_13, boundbox)
ndvi_amp_13 <- resample(ndvi_amp_13, elev, "ngb")
precip_13 <- crop(precip_13, boundbox)
precip_13 <- resample(precip_13, elev, "ngb")

# stack all rasters and write to covs2013
raststck <- stack(elev, landcov_13, ndvi_amp_13, precip_13, radn, slope)
names(raststck) <- c("elev", "landcov_13", "ndvi_amp_13", "precip_13",
                     "radn", "slope")
writeRaster(raststck, file.path('writtenrasters/covs2013', names(raststck)),
                                bylayer = TRUE, format = "GTiff", overwrite=TRUE)


#################
#### Predict ####
#################

# read in model data #
# (unclear how this works since using wrong yr data) #
dat <- read.csv("DE-model-data.csv") 

# Set factor levels for landcover type #
ref.lev <- dat %>%
  dplyr::select(c(landcov, class_name, DE)) %>%
  group_by(class_name, landcov) %>%
  summarise(AvgDE = mean(DE)) %>%
  arrange(AvgDE) %>% ## Order from least to most forb GDM 
  ungroup() 
ref.lev # so reference level is landcover type with lowest DE
dat$landcov <- factor(dat$landcov, levels = as.vector(ref.lev$landcov))

# define predictive model #
de.pred <- lm(DE ~ elev + landcov + radn + ndvi_amp + 
              precip + slope, data = dat)

# prep rasters #
rast.13 <- list.files(path=paste(wd, "writtenrasters/covs2013", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2013 rasters
stack.13 <- stack(rast.13)
names(stack.13) # rename rasters to match covariate names
names(stack.13) <- c("elev", "landcov", "ndvi_amp", "precip", "radn", "slope")
names(stack.13) # sanity check


# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2013 rasters of DE and StdError (indices 1 and 2, respectively)
de2013 <- predict(stack.13, de.pred, fun=predfun, index=1:2, progress="text")
names(de2013) <- c("DE2013","StdErr13") 
plot(de2013) # plot both
plot(de2013[["DE2013"]]) # plot one at a time
plot(de2013[["StdErr13"]])

# export DE rasters
writeRaster(de2013, names(de2013), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)

			
#################
#### Compare ####
#################

# view with 2014 and 2015
de13 <- raster("DE2013.tif")
de14 <- raster("DE2014.tif")
de15 <- raster("DE2015.tif")
par(mfrow=c(1,3))
plot(de13, main = "2013")
plot(de14, main = "2014")
plot(de15, main = "2015")
#look super similar, good. 

t.test(de13, de14, alternative = "two.sided")
t.test(de14, de15, alternative = "two.sided")
t.test(de13, de15, alternative = "two.sided")
#2013/2014 diff < 2014/2015 diff

summary(de13); 

cellStats(de13, stat='mean') 
cellStats(de13, stat='sd')

cellStats(de14, stat='mean') 
cellStats(de14, stat='sd')

cellStats(de15, stat='mean') 
cellStats(de15, stat='sd')
par(mfrow=c(1,2)); boxplot(de13); boxplot(de14)
