#################################
#      PREPARING RASTERS        #
# FOR USE IN PREDICTIVE MODELS  #
#           NSERP               #
#       KRISTIN BARKER          #
#################################


## Code to take rasters used in Bitterroot studies and
## crop them to North Sapphires only for faster processing
## And add/remove some spatial covariates


#################
#### Setup   ####
#################

library(raster)


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


##############
#### Data ####
##############

# define filepath to folder containing original rasters
rasterpath <- paste(getwd(), "/writtenrasters/orig", sep="")
allrasters <- list.files(rasterpath, pattern = "tif$") 

# read in rasters and name as covariates
cc <- raster(paste(rasterpath, "cc.tif", sep="/")) #canopy cover
elev <- raster(paste(rasterpath, "elev.tif", sep="/")) #elevation
landcov_14 <- raster(paste(rasterpath, "esp6_14.tif", sep="/")) #landcover
landcov_15 <- raster(paste(rasterpath, "esp6_15.tif", sep="/")) #(incl fire history)
radn <- raster(paste(rasterpath, "gsri.tif", sep="/")) #solar radiation
ndvi_amp_14 <- raster(paste(rasterpath, "ndvi_amp_2014.tif", sep="/")) #ndvi amplitude
ndvi_amp_15 <- raster(paste(rasterpath, "ndvi_amp_2015.tif", sep="/")) #(photosynth inc)
ndvi_dur_14 <- raster(paste(rasterpath, "ndvi_dur_14.tif", sep="/")) #ndvi duration
ndvi_dur_15 <- raster(paste(rasterpath, "ndvi_dur_15.tif", sep="/")) #(lgth growing season)
ndvi_ti_14 <- raster(paste(rasterpath, "ndvi_ti_14.tif", sep="/")) #time-integrated ndvi
ndvi_ti_15 <- raster(paste(rasterpath, "ndvi_ti_15.tif", sep="/")) #(photosynth activity)
precip_14 <- raster(paste(rasterpath, "precip_2014.tif", sep="/")) #growing season precip
precip_15 <- raster(paste(rasterpath, "precip_2015.tif", sep="/")) #(may - aug)
spr_precip_14 <- raster(paste(rasterpath, "spr_precip_2014.tif", sep="/")) #spring precip
spr_precip_15 <- raster(paste(rasterpath, "spr_precip_2015.tif", sep="/")) #(may - june)
slope <- raster(paste(rasterpath, "slope.tif", sep="/")) #slope

# crop to study area; match extents and resolutions to enable stacking
boundbox <- extent(cc) # use canopy cover raster as basis for all others
elev <- crop(elev, boundbox)
elev <- resample(elev, cc, "ngb")
landcov_14 <- crop(landcov_14, boundbox)
landcov_14 <- resample(landcov_14, cc, "ngb")
landcov_15 <- crop(landcov_15, boundbox)
landcov_15 <- resample(landcov_15, cc, "ngb")
radn <- crop(radn, boundbox)
radn <- resample(radn, cc, "ngb")
ndvi_amp_14 <- crop(ndvi_amp_14, boundbox)
ndvi_amp_14 <- resample(ndvi_amp_14, cc, "ngb")
ndvi_amp_15 <- crop(ndvi_amp_15, boundbox)
ndvi_amp_15 <- resample(ndvi_amp_15, cc, "ngb")
ndvi_dur_14 <- crop(ndvi_dur_14, boundbox)
ndvi_dur_14 <- resample(ndvi_dur_14, cc, "ngb")
ndvi_dur_15 <- crop(ndvi_dur_15, boundbox)
ndvi_dur_15 <- resample(ndvi_dur_15, cc, "ngb")
ndvi_ti_14 <- crop(ndvi_ti_14, boundbox)
ndvi_ti_14 <- resample(ndvi_ti_14, cc, "ngb")
ndvi_ti_15 <- crop(ndvi_ti_15, boundbox)
ndvi_ti_15 <- resample(ndvi_ti_15, cc, "ngb")
precip_14 <- crop(precip_14, boundbox)
precip_14 <- resample(precip_14, cc, "ngb")
precip_15 <- crop(precip_15, boundbox)
precip_15 <- resample(precip_15, cc, "ngb")
spr_precip_14 <- crop(spr_precip_14, boundbox)
spr_precip_14 <- resample(spr_precip_14, cc, "ngb")
spr_precip_15 <- crop(spr_precip_15, boundbox)
spr_precip_15 <- resample(spr_precip_15, cc, "ngb")
slope <- crop(slope, boundbox)
slope <- resample(slope, cc, "ngb")

# stack rasters and export to "writtenrasters" folder
filepath <- paste(getwd(), "/writtenrasters", sep="")
raststck <- stack(cc, elev, landcov_14, landcov_15, radn, ndvi_amp_14, ndvi_amp_15,
                  ndvi_dur_14, ndvi_dur_15, ndvi_ti_14, ndvi_ti_15, precip_14,
                  precip_15, spr_precip_14, spr_precip_15, slope)
names(raststck) <- c("cc", "elev", "landcov_14", "landcov_15", "radn", "ndvi_amp_14", "ndvi_amp_15",
                  "ndvi_dur_14", "ndvi_dur_15", "ndvi_ti_14", "ndvi_ti_15", "precip_14",
                  "precip_15", "spr_precip_14", "spr_precip_15", "slope")
writeRaster(raststck, file.path('writtenrasters', names(raststck)),
                                bylayer = TRUE, format = "GTiff", overwrite=TRUE)




