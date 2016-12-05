##########################################################
#         ESTIMATING GRAMS OF DIGESTIBLE MATTER          #
#      ACROSS NORTH SAPPHIRES IN SUMMER 2014 & 2015      #
#                   KRISTIN BARKER                       #
#                     OCT 2016                           #
##########################################################

###########################
#### Setup             ####
###########################

library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(raster)
library(rgdal) #Access geodatabases
library(rgeos)
library(ggplot2)
library(AICcmodavg)
library(car) # correlations with factor covariates
library(pscl) # zero-inflated model
library(dplyr) 

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
rasterOptions(maxmemory = 1e+09) # increases max number of cells to read into memory, increasing processing time

############################################################################
#  EITHER RUN THIS SECTION OR FOLLOWING SECTION                           ##
# Read, process, and write out all data for modeling and predictions ####
############################################################################

###############
## PLOT DATA ##

# per plot: year sampled, GDM, lat/long, NDVI
plot.data <-read.csv("gdm-plot-lifeform.csv", header=T)

# get points, write out to dataframe, to spatial data frame, to shapefile 
xy <- data.frame("x"=plot.data$Longitude,"y"=plot.data$Latitude)
latlong = CRS("+init=epsg:4326")
xy.spdf.ll <- SpatialPointsDataFrame(xy, plot.data, proj4string = latlong)
proj.crs <- "+proj=lcc +lat_1=49 +lat_2=45 +lat_0=44.25 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
plots.tmp <- spTransform(xy.spdf.ll, CRS(proj.crs)) # reproject to MT stateplane
writeOGR(plots.tmp, dsn = ".", layer = 'GDM_plots', driver="ESRI Shapefile",overwrite_layer=TRUE)
# read in shapefile and extract attribute data
plots <- readOGR(".", layer ='GDM_plots') 

#################
## RASTER DATA ##

# read in applicable rasters jesse already processed
  # and change landcover 0s to NAs (non-habitat, eg roads and rivers)
cc_orig <- raster("writtenrasters\\orig\\cc.tif") 
cti_orig <- raster("writtenrasters\\orig\\cti.tif") 
elev_orig <- raster("writtenrasters\\orig\\elev.tif")
esp6_14_orig <- raster("writtenrasters\\orig\\esp6_14.tif")
  esp6_14_orig[esp6_14_orig == 0] <- NA
esp6_15_orig <- raster("writtenrasters\\orig\\esp6_15.tif") 
  esp6_15_orig[esp6_15_orig == 0] <- NA
gsri_orig <- raster("writtenrasters\\orig\\gsri.tif")
hillshade_orig <- raster("writtenrasters\\orig\\hillshade.tif") 
ndvi_ti_14_orig <- raster("writtenrasters\\orig\\ndvi_ti_2014.tif") 
ndvi_ti_15_orig <- raster("writtenrasters\\orig\\ndvi_ti_2015.tif") 
precip_2014_orig <- raster("writtenrasters\\orig\\precip_2014.tif")
precip_2015_orig <- raster("writtenrasters\\orig\\precip_2015.tif")
slope_orig <-raster("writtenrasters\\orig\\slope.tif")

# add ndvi_dur (length of growing season)
ndvi_dur_14_orig <- raster("writtenrasters\\orig\\ndvi_dur_2014.tif") 
ndvi_dur_15_orig <- raster("writtenrasters\\orig\\ndvi_dur_2015.tif") 

# make raster extents and resolutions match 
bbox <- extent(ndvi_dur_14_orig) # NSERP study area extent
esp6_15 <- crop(esp6_15_orig, bbox, snap = 'near') # use this as baseline
esp6_14 <- crop(esp6_14_orig, bbox, snap = 'near')
cc <- crop(cc_orig, bbox, snap = 'near')
cti <- crop(cti_orig, bbox, snap = 'near')
elev <- crop(elev_orig, bbox, snap = 'near')
gsri <- crop(gsri_orig, bbox, snap = 'near')
hillshade <- crop(hillshade_orig, bbox, snap = 'near')
ndvi_ti_14 <- crop(ndvi_ti_14_orig, bbox, snap = 'near')
ndvi_ti_15 <- crop(ndvi_ti_15_orig, bbox, snap = 'near')
precip_2014 <- crop(precip_2014_orig, bbox, snap = 'near')
precip_2015 <- crop(precip_2015_orig, bbox, snap = 'near')
slope <- crop(slope_orig, bbox, snap = 'near')
ndvi_dur_14 <- ndvi_dur_14_orig
ndvi_dur_15 <- crop(ndvi_dur_15_orig, bbox, snap = 'near')

# stack and save
s <- stack(esp6_14, esp6_15, cc, cti, elev, gsri, 
           hillshade, ndvi_ti_14, ndvi_ti_15, precip_2014, precip_2015, 
           slope, ndvi_dur_14, ndvi_dur_15)
names(s)
names(s) <- c("esp6_14", "esp6_15", "cc", "cti", "elev", 
              "gsri", "hillshade", "ndvi_ti_14", "ndvi_ti_15", "precip_14", 
              "precip_15", "slope", "ndvi_dur_14", "ndvi_dur_15")
writeRaster(s, file.path('writtenrasters', names(s)), bylayer=TRUE, format='GTiff')

#################################################################################
##      EITHER RUN THIS SECTION OR PRECEDING SECTION                          ##
#  Pull in processed data from above section (if you've already run it once) ####
#################################################################################

# plot-level NDVI values
rmt.data <- read.csv("ndvi-plot.csv") %>%
  dplyr::select(PlotVisit, NDVI)
#datafile with the year sampled, GDM per plot, along with plot lat/longs
plot.data <-read.csv("gdm-plot-lifeform.csv", header=T)  %>% 
    filter(!PlotVisit == "220.2015-08-03") %>% #remove GDM outlier (landcov also wrong)
    left_join(rmt.data, by = "PlotVisit") #add ndvi
#above datafile as shapefile
plots<-readOGR(".", layer ='GDM_plots')
#all rasters for covariates
raster_data<-list.files(path=paste(wd, "writtenrasters", sep="/"), pattern="tif$", full.names=TRUE) 
s <- stack(raster_data) 
names(s)

##################################################################################################
#Extract attribute data for each plot and write a data.frame for building landscape GDM model ####
##################################################################################################

# Extract values of rasters to sampling locations and create data.frame with GDM and attributes
ext<-extract(s, plots) ##Extract from raster stack for each plot location
plot.data <- data.frame(plot.data) ##Convert plot info attribute table to dataframe
data <- cbind(plot.data, ext)	##Bind  plot info and extracted landcover data into df
data$Date<-as.Date(data$Date, "%Y-%m-%d") #Set dates and calculate Year
data$Year<-as.numeric(format(data$Date, '%Y'))
#Pick correct years of covariates
data$cover_class <- ifelse(data$Year == 2014, data$esp6_14, data$esp6_15)
data$ndvi_dur <- ifelse(data$Year == 2014, data$ndvi_dur_14, data$ndvi_dur_15)
data$ndvi_ti <- ifelse(data$Year == 2014, data$ndvi_ti_14, data$ndvi_ti_15)
data$sum_precip <- ifelse(data$Year == 2014, data$precip_14, data$precip_15)
#make NDVI durations classified as "NoData" into NAs
data$ndvi_dur <- ifelse(data$ndvi_dur < 90 | data$ndvi_dur > 365, NA, data$ndvi_dur)

#Create new covariates, standardize covariates and finish building dataset
data$cover_class<-as.factor(data$cover_class)
data$cc_std<-((data$cc-(mean(data$cc)))/(sd(data$cc)))
data$cti_std<-((data$cti-(mean(data$cti)))/(sd(data$cti)))
data$elev_std<-((data$elev-(mean(data$elev)))/(sd(data$elev)))
data$gsri_std<-((data$gsri-(mean(data$gsri)))/(sd(data$gsri)))
data$hillshade_std<-((data$hillshade-(mean(data$hillshade)))/(sd(data$hillshade)))
data$ndvi_ti_std<-((data$ndvi_ti-(mean(data$ndvi_ti)))/(sd(data$ndvi_ti)))
data$sum_precip_std<-((data$sum_precip-(mean(data$sum_precip)))/(sd(data$sum_precip)))
data$slope_std<-((data$slope-(mean(data$slope)))/(sd(data$slope)))

# join to land cover classifications to attach names
clsref_esp <- data.frame(cbind(cover_class = c(1,2,3,4,5,6,7,8,9,10,11,12),
                               class_name = c("Mesic Forest (Burn >15)", "Dry Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                              "Dry Ag", "Valley Bottom Riparian","Montane Riparian", "Irrigated Ag",
                                              "Dry Forest Burn 0-5", "Dry Forest Burn 6-15",
                                              "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                                              "Rx Dry Forest Burn 0-5")))
clsref_esp$cover_class <- as.numeric(as.character(clsref_esp$cover_class))
data <- merge(data, clsref_esp, by="cover_class")

# create subset of data without the "NA" NDVI durations
# datasub <- data[!is.na(data$ndvi_dur),]
# datasub$ndvi_dur_std<-((datasub$ndvi_dur-(mean(datasub$ndvi_dur)))/(sd(datasub$ndvi_dur)))

#data<-subset(data, !data$cover_class == 0) #Remove sample from "other" landcover 

# add back in biomass data
bmss <- read.csv("biomass-plot.csv") %>%
  select(c(PlotVisit, Biomass, ForbBiomass, GrassBiomass, ShrubBiomass)) %>%
  rename(Gforb = ForbBiomass, Ggrass = GrassBiomass, Gshrub = ShrubBiomass)
data <- right_join(data, bmss, by = "PlotVisit")

# and export
write.csv(data, "data_GDM.csv", row.names = FALSE)


##############################################################################################
#### Summarize GDM data                                                                   ####
##############################################################################################

# KRISTIN you can skip this section for now

dat.GDM <- read.csv("data_GDM.csv")
#  dat.GDM <- data  #if running code in full from above, can replace read.csv with this

#sapply(dat.GDM,class) #just prints str(dat.GDM) in diff format
dat.GDM$cover_class<-factor(dat.GDM$cover_class) #not needed if replaced read.csv above
dat.GDM$Date<-as.Date(dat.GDM$Date, "%Y-%m-%d") #not needed if replaced read.csv above
#dat.GDM<-subset(dat.GDM, dat.GDM$Season == "Summer") #kjb data already subsetted
summarise(dat.GDM, aveGDM = mean(dat.GDM$GDM))
summarise(dat.GDM, sdGDM = sd(dat.GDM$GDM))

dat<-group_by(dat.GDM, cover_class)
GDMSummary=summarise(dat,
                     class_name=first(class_name),
                     count=n(),
                     AveGDM=mean(GDM),
                     MedGDM=median(GDM),
                     MinGDM=min(GDM),
                     MaxGDM=max(GDM),
                     SdGDM=sd(GDM))
GDMSummary 
#write.table(GDMSummary, "GDMSummary_landcover_summer.csv", sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)

#############################
#####   GGPLOT  BOXPLOT   ###

library(plyr)
dat.GDM$class_name <- factor(dat.GDM$class_name, levels=c("Dry Forest Burn 0-5", "Rx Dry Forest Burn 0-5", "Dry Forest Burn 6-15", "Dry Forest (Burn >15)",   
                                                            "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", "Mesic Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                                            "Montane Riparian", "Valley Bottom Riparian", "Dry Ag", "Irrigated Ag"))
#labelling sample size
give.n <- function(x){ return(c(y = 0.36, label = length(x)))}
#xlabels <- ddply(dat.GDM, .(class_name), summarize, xlabels = paste(unique(class_name),' \n (n=', length(class_name),')       ', sep=""))

GDM_class= ggplot(data=dat.GDM, aes(x=class_name, y=GDM)) +
  #geom_boxplot(aes(fill=class_name), alpha=0.4) +
  geom_boxplot() +
  ylim(0.35, 1) + ylab("Dry Matter Digestibility") +
  theme(axis.text.y=element_text(size=12)) +
  theme(axis.title.y=element_text(size=14)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  theme(axis.text.x=element_text(size=13, angle=55, hjust=1, vjust = 1)) +
  theme(legend.position="none") +
  stat_summary(fun.data = give.n, geom = "text", size=4.5, color='#636363')
  #scale_x_discrete(labels = xlabels[['xlabels']])
GDM_class
#save to high quality tiff
ggsave("GDM boxplot per Cover Class_nocol.tiff", plot = GDM_class, width=7, height =6, units ="in", dpi = 300)


##############################################################################################
#### Build summer nutrition models for each vegetation type                    ####
##############################################################################################

# Read in data - pick one of the 3 lines below

#dat.GDM <- read.csv("data_GDM.csv") 
dat.GDM <- data
#dat.GDM <- datasub # from removing bad ndvi_dur values
  #summary(dat.GDM$GDM)
  #summary(datasub$GDM) #good, not super different

############################
####    FORB             ###
############################

# Check out data and pick reference levels

## distribution of GDM
hist(dat.GDM$GDMforb) # super skewed R (valid, not outliers - high in irrig ag)
  hist(log(dat.GDM$GDMforb))
  hist(log10(dat.GDM$GDMforb))
  hist(log1p(dat.GDM$GDMforb)) # box-cox transformation

ggplot(dat.GDM, aes(x=GDMforb)) +
  geom_histogram() +
  facet_grid(~cover_class) #same per landcover
median(dat.GDM$GDMforb); mean(dat.GDM$GDMforb)
# median best meas of center; use log10GDM for model

## order landcov by inc'ing median GDM; set this as reference level order
cov.gdm.forb <- dat.GDM %>%
  dplyr::select(c(cover_class, class_name, GDMforb)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDMforb = median(GDMforb)) %>%
  arrange(desc(MedGDMforb)) # desc orders high to low (default is low to high)
write.csv(cov.gdm.forb, file = "gdm-forb-landcov.csv", row.names=F)
lev.forb <- as.vector(cov.gdm.forb$cover_class)

dat.GDM$cover_class <- factor(dat.GDM$cover_class, levels = lev.forb)

# Check correlations between covariates of interest
mydata <- dat.GDM %>% # or mydata <- datasub %>%
   dplyr::select(cc, cti, elev, gsri, hillshade, ndvi_dur, ndvi_ti, 
                 sum_precip, slope, GDMforb, Gforb)
head(mydata)
cor(mydata) # correlation matrix
# plot the data and display r values
library(PerformanceAnalytics)
chart.Correlation(mydata)
# no correlation coefficients higher than 0.54



vif(m.all.forb) # correlations with cover_class factor


#step <- stepAIC(m.all, direction="both")
#step$anova  # display results  


############################
####    GRAMINOID        ###
############################

# Check out data and pick reference levels

## distribution of GDM
hist(dat.GDM$GDMgrass) # skewed R, but not as much as forbs
  hist(log(dat.GDM$GDMgrass)) 
  hist(sqrt(dat.GDM$GDMgrass)) 
  hist(log1p(dat.GDM$GDMgrass)) # box-cox transformation
ggplot(dat.GDM, aes(x=GDMgrass)) +
  geom_histogram() +
  facet_grid(~cover_class) #same per landcover
median(dat.GDM$GDMgrass); mean(dat.GDM$GDMgrass)
# median best meas of center; use logGDM for model

## order landcov by inc'ing median GDM and set as ref level order for model
cov.gdm.grass <- dat.GDM %>%
  dplyr::select(c(cover_class, class_name, GDMgrass)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDMgrass = median(GDMgrass)) %>%
  arrange(desc(MedGDMgrass))
lev.grass <- as.vector(cov.gdm.grass$cover_class)

dat.GDM$cover_class <- factor(dat.GDM$cover_class, levels = lev.grass)

############################
####    SHRUB            ###
############################

# Check out data and pick reference levels

## distribution of GDM
hist(dat.GDM$GDMshrub) # skewed R, but not as much as forbs
  hist(log(dat.GDM$GDMshrub)) 
  hist(sqrt(dat.GDM$GDMshrub)) 
  hist(log1p(dat.GDM$GDMshrub)) # box-cox transformation
ggplot(dat.GDM, aes(x=GDMshrub)) +
  geom_histogram() +
  facet_grid(~cover_class) #same per landcover
median(dat.GDM$GDMshrub); mean(dat.GDM$GDMshrub)
# median best meas of center; use logGDM for model

## order landcov by inc'ing median GDM and set as ref level order for model
cov.gdm.shrub <- dat.GDM %>%
  dplyr::select(c(cover_class, class_name, GDMshrub)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDMshrub = median(GDMshrub)) %>%
  arrange(desc(MedGDMshrub))
lev.shrub <- as.vector(cov.gdm.shrub$cover_class)

dat.GDM$cover_class <- factor(dat.GDM$cover_class, levels = lev.shrub)

############################
#### COMBINED STUFF      ###
############################

# GDM by cover class for each veg type
cov.gdm <- left_join(cov.gdm.forb, cov.gdm.grass, 
                    by = c("cover_class", "class_name")) %>%
           left_join(cov.gdm.shrub, 
                     by = c("cover_class", "class_name"))
write.csv(cov.gdm, file = "gdm-landcov.csv", row.names=F)


###############################################################################

##############################################
#### KJB - CODE YOU HAVEN'T SPLIT YET      ###
##############################################

# look at order
dat.forb<-group_by(dat.GDM, cover_class)
(GDMSummaryforb=summarise(dat.forb,
                     class_name=first(class_name),
                     count=n(),
                     AveGDMforb=mean(GDMforb),
                     MedGDMforb=median(GDMforb),
                     MinGDMforb=min(GDMforb),
                     MaxGDMforb=max(GDMforb),
                     SdGDMforb=sd(GDMforb)))
dat.grass<-group_by(dat.GDM, cover_class)
(GDMSummarygrass=summarise(dat.grass,
                     class_name=first(class_name),
                     count=n(),
                     AveGDMgrass=mean(GDMgrass),
                     MedGDMgrass=median(GDMgrass),
                     MinGDMgrass=min(GDMgrass),
                     MaxGDMgrass=max(GDMgrass),
                     SdGDMgrass=sd(GDMgrass)))

dat.GDM$cover_class <- factor(dat.GDM$cover_class, levels = lev)

#Distribution of response
#library(wesanderson)
#pal <- wes_palette("Zissou", 10, type="continuous")
#ggplot(data=dat.GDM, aes(x=class_name, y=GDM, color=GDM)) +
#  geom_jitter(width=0.3, height=0.1, alpha=0.5) +
#  theme(axis.text.x=element_text(size=12, angle=55, hjust=1, vjust = 1)) +
#  scale_color_gradientn(colors=pal)




# backwards stepwise aic to determine top model
#m.all <- lm(log(GDM) ~ cover_class + cti_std + elev_std + gsri_std + hillshade_std + ndvi_ti_std + sum_precip_std + slope_std + ndvi_avg_std, data=dat.GDM)
#step <- stepAIC(m.all, direction="both")
#step$anova  # display results  

# look at top model
#m1 <- lm(log(GDM) ~ cover_class + cti_std + elev_std + ndvi_ti_std + slope_std + ndvi_avg_std, data=dat.GDM)
#summary(m1)
#plot(m1) # residual plots

### UNSTANDARDIZED MODEL
# to make spatial prediction easier/make more sense

# backwards stepwise aic to determine top model
### just making sure unstandardized result is same as standardized
m.all <- lm(log(GDM) ~ cover_class + cc + cti + elev + gsri + ndvi_ti + sum_precip + slope + ndvi_avg, data=dat.GDM)
step <- stepAIC(m.all, direction="both")

# look at top model
m1 <- lm(log(GDM) ~ cover_class + cti + elev + ndvi_ti + slope + ndvi_avg, data=dat.GDM)
summary(m1)

# check relative support for some simpler models
#Cand.set <- list( )
#Cand.set[[1]] <- lm(log(GDM) ~ cover_class + cti_std + elev_std + ndvi_ti_std + slope_std + ndvi_avg_std, data=dat.GDM)
#Cand.set[[2]] <- lm(log(GDM) ~ cover_class + cti_std + elev_std + ndvi_ti_std + ndvi_avg_std, data=dat.GDM)
#Cand.set[[3]] <- lm(log(GDM) ~ cover_class + cti_std + elev_std + slope_std + ndvi_avg_std, data=dat.GDM)
#Cand.set[[4]] <- lm(log(GDM) ~ cover_class + cti_std + elev_std + ndvi_ti_std + slope_std, data=dat.GDM)
#Cand.set[[5]] <- lm(log(GDM) ~ cover_class + ndvi_ti_std + ndvi_avg_std, data=dat.GDM)
#names(Cand.set) <- c("TopModel", "TopNoSlope", "TopNoTI", "TopNoNDVI", "VegOnly")
#aictable <- aictab(Cand.set, second.ord=FALSE)
#aicresults <- print(aictable, digits = 2, LL = FALSE)
# nah, better stick with what i've got

# Predict the models based on raster covariates ####

# 2014 covariate stack
# yes i realize it would be cooler if i'd done all this programmatically
rasters_14 <- list.files(path=paste(wd, "writtenrasters/covs2014", sep="/"), pattern="tif$", full.names=TRUE) 
s.14 <- stack(rasters_14) 
names(s.14)
names(s.14) <- c("cc", "cti", "elev", "cover_class", "hillshade", "ndvi_avg", 
                 "ndvi_dur", "ndvi_ti", "precip", "slope")
names(s.14)

# 2015 covariate stack
rasters_15 <- list.files(path=paste(wd, "writtenrasters/covs2015", sep="/"), pattern="tif$", full.names=TRUE) 
s.15 <- stack(rasters_15) 
names(s.15)
names(s.15) <- c("cc", "cti", "elev", "cover_class", "hillshade", "ndvi_avg", 
                 "ndvi_dur", "ndvi_ti", "precip", "slope")
names(s.15)

# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# run above function in raster::predict; ouput raster of both predicted GDM and SE

# 2014
pred2014 <- predict(s.14, m1, fun=predfun, index=1:2, progress="text") # predfun returns two variables (response and se), so need to include index=1:2
names(pred2014) <- c("lnGDM2014","se14") 
plot(pred2014) # plot both
plot(pred2014[["lnGDM2014"]]) # plot one at a time
plot(pred2014[["se14"]])

# 2015
pred2015 <- predict(s.15, m1, fun=predfun, index=1:2, progress="text")
names(pred2015) <- c("lnGDM2015","se15") 
plot(pred2015[["lnGDM2015"]]) # plot one at a time
plot(pred2015[["se15"]])
plot(pred2015) # plot both

# transform to actual GDM numbers
gdm14 <- exp(pred2014[["lnGDM2014"]])
gdm15 <- exp(pred2015[["lnGDM2015"]])
par(mfrow=c(2,1))
plot(gdm14, main = "GDM 2014")
plot(gdm14, main = "GDM 2015")

# store all rasters
writeRaster(pred2014, filename = "pred2014", byLayer = TRUE, format = 'GTiff')
writeRaster(pred2015, filename = "pred2015", byLayer = TRUE, format = 'GTiff')
writeRaster(gdm14, "GDM2014.tif", format = 'GTiff')
writeRaster(gdm15, "GDM2015.tif", format = 'GTiff')
