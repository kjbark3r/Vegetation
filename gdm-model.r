## kjb - things to do/change

#X create gdm-plot.csv using prev biomass code
  #X incl visitdate, latitude, longitude, gdm
#X github - upload jesses version as first commit
  #X so you can see exact changes you made, and also
  #X so when you eff it up you can easily revert back
# make bbox for nsapph (~l.82)
# decide best landcov reference level for model (~l.290)
# note prob need to recode looking at distn of response (~l.296)


###########################
#### Setup             ####
###########################
library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(raster)
library(rgdal) #Access geodatabases
library(rgeos)
library(dplyr) #Group by function
library(ggplot2)

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Vegetation"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\UMT\\Thesis\\Migration_Consequences\\Analyses\\Veg"

if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
  wd <- wd_workcomp
} else {
    setwd(wd_laptop)
    wd <- wd_laptop
}

rasterOptions(maxmemory = 1e+09) # increases max number of cells to read into memory, increasing processing time
memory.limit()

############################################################################
# Read, process, and write out all rasters for modeling and predictions ####
############################################################################

#Read in a datafile with the year sampled, GDM per plot, along with plot lat/longs
plot.data <-read.csv("gdm-plot.csv", header=T) 

head(plot.data)
nrow(plot.data)   
#Get points, write out the points to dataframe, to spatial data frame, to shapefile 
xy <- data.frame("x"=plot.data$Longitude,"y"=plot.data$Latitude)
latlong = CRS("+init=epsg:4326")
xy.spdf.ll <- SpatialPointsDataFrame(xy, plot.data, proj4string = latlong)
##Reproject plots to MT State Plane
proj.crs <- "+proj=lcc +lat_1=49 +lat_2=45 +lat_0=44.25 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
plots.tmp<-spTransform(xy.spdf.ll, CRS(proj.crs))
writeOGR(plots.tmp, dsn = ".", layer = 'GDM_plots', driver="ESRI Shapefile",overwrite_layer=TRUE)
#Read in shapefile and extract attribute data
plots<-readOGR(".", layer ='GDM_plots') 

#Read in all rasters in State Plane projection
esp6_15_orig <- raster("..\\Landcover\\finalrasters\\esp6_15.tif") 	
esp6_14_orig <- raster("..\\Landcover\\finalrasters\\esp6_14.tif") 
t_fire_15_orig <- raster("..\\Landcover\\finalrasters\\fysb2015_m.tif") ## New fire layers, outputted from decadalAccumYearsSinceFire.gdb with Mustang Complex fire amended
t_fire_14_orig <- raster("..\\Landcover\\finalrasters\\fysb2014_m.tif") 
elev_orig <-raster("..\\AnalysisRasters\\demMtWyId")
slope_orig <-raster("..\\AnalysisRasters\\slope_3st_deg")
cc_orig <- raster("..\\AnalysisRasters\\cc") 

precip_2014_orig <- raster("..\\AnalysisRasters\\cumuprec2014")
precip_2015_orig <- raster("..\\AnalysisRasters\\cumuprec2015") 
spr_precip_2014_orig <- raster("..\\AnalysisRasters\\sprprec2014")
spr_precip_2015_orig <- raster("..\\AnalysisRasters\\sprprec2015")
if (wd==wd.kp) { hillshade_orig <- raster("F:\\Research_Data\\DEMRasters\\Hillshade\\hillshade") 
} else { hillshade_orig <- raster("..\\..\\..\\..\\GIS\\Source Data\\Research_Data\\DEMRasters\\Hillshade\\hillshade")}
cti_orig<-raster("..\\AnalysisRasters\\cti")
ndvi_ti_2014_orig <- raster("..\\AnalysisRasters\\NDVI\\tin2014_v2") # Reprojected original to MT state plane and cropped to smaller extent in ArcMAP
ndvi_ti_2015_orig <- raster("..\\AnalysisRasters\\NDVI\\tin2015_v2")  
ndvi_amp_2014_orig <- raster("..\\AnalysisRasters\\NDVI\\amp2014_v2") 
ndvi_amp_2015_orig <- raster("..\\AnalysisRasters\\NDVI\\amp2015_v2") 
if (wd==wd.kp) { gsri_orig <- raster("..\\AnalysisRasters\\gsri") 
} else { gsri_orig <- raster("..\\AnalysisRasters\\gsri.tif")} 

#Process rasters down so they can be stacked 
#boundbox <- extent(130000, 130500, 110000, 110500) # TEST on on small area (xmin, xmax, ymin, ymax)
boundbox <- extent(150000, 350000, 110000, 350000) ##Entire study area
esp6_15 <- crop(esp6_15_orig, boundbox)				##Crop to boundbox extent
esp6_14 <- crop(esp6_14_orig, boundbox)	
t_fire_15 <- crop(t_fire_15_orig, boundbox)
t_fire_15 <- resample(t_fire_15, esp6_15, "ngb") 
t_fire_14 <- crop(t_fire_14_orig, boundbox)
t_fire_14 <- resample(t_fire_14, esp6_15, "ngb")
elev<- crop(elev_orig, boundbox)   ##if extent and resolution different, set extent and resolution in order to stack rasters 
elev<- resample(elev, esp6_15, "bilinear")  # resample down to esp level
slope<- crop(slope_orig, boundbox)
slope<- resample(slope, esp6_15, "bilinear")
cc<- crop(cc_orig, boundbox)
cc<- resample(cc, esp6_15, "bilinear")
precip_2014<- crop(precip_2014_orig, boundbox)
precip_2014<- resample(precip_2014, esp6_15, "bilinear")
precip_2015<- crop(precip_2015_orig, boundbox)
precip_2015<- resample(precip_2015, esp6_15, "bilinear")
spr_precip_2014<- crop(spr_precip_2014_orig, boundbox)
spr_precip_2014<- resample(spr_precip_2014, esp6_15, "bilinear")
spr_precip_2015<- crop(spr_precip_2015_orig, boundbox)
spr_precip_2015<- resample(spr_precip_2015, esp6_15, "bilinear")
hillshade<- crop(hillshade_orig, boundbox)
hillshade<- resample(hillshade, esp6_15, "bilinear")
cti<- crop(cti_orig, boundbox)
cti<- resample(cti, esp6_15, "bilinear")
ndvi_ti_2014<- crop(ndvi_ti_2014_orig, boundbox)
ndvi_ti_2014<- calc(ndvi_ti_2014, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_ti_2014<- resample(ndvi_ti_2014, esp6_15, "bilinear")
ndvi_ti_2014<- focal(ndvi_ti_2014, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) 
ndvi_ti_2015<- crop(ndvi_ti_2015_orig, boundbox)
ndvi_ti_2015<- calc(ndvi_ti_2015, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_ti_2015<- resample(ndvi_ti_2015, esp6_15, "bilinear")
ndvi_ti_2015<- focal(ndvi_ti_2015, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) 
ndvi_amp_2014<- crop(ndvi_amp_2014_orig, boundbox)
ndvi_amp_2014<- calc(ndvi_amp_2014, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_amp_2014<- resample(ndvi_amp_2014, esp6_15, "bilinear") 
ndvi_amp_2014<- focal(ndvi_amp_2014, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) # only for NA's, replace with focal mean of 11x11
ndvi_amp_2015<- crop(ndvi_amp_2015_orig, boundbox)
ndvi_amp_2015<- calc(ndvi_amp_2015, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_amp_2015<- resample(ndvi_amp_2015, esp6_15, "bilinear") 
ndvi_amp_2015<- focal(ndvi_amp_2015, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) # only for NA's, replace with focal mean of 11x11
gsri<- crop(gsri_orig, boundbox)
gsri<- resample(gsri, esp6_15, "bilinear") 

#Check rasters to confirm all x and y min and max's are the same for stacking
s <- stack()
s <- stack(esp6_15, esp6_14, t_fire_15, t_fire_14, elev, slope, cc, 
           precip_2014, precip_2015, spr_precip_2014, spr_precip_2015, 
           hillshade, cti, ndvi_ti_2014, ndvi_ti_2015, 
           ndvi_amp_2014, ndvi_amp_2015, gsri)  	##Create raster stack for performing extractions and calcs
names(s) <- c("esp6_15", "esp6_14", "t_fire_15", "t_fire_14", "elev", "slope", "cc", 
              "precip_2014", "precip_2015", "spr_precip_2014", "spr_precip_2015", "hillshade", "cti", 
              "ndvi_ti_2014", "ndvi_ti_2015", "ndvi_amp_2014", "ndvi_amp_2015", "gsri")
names(s)

#Writing all rasters into new folder (for reading in later for prediction raster w/o having to run above code)
if(!file.exists("writtenrasters")) {dir.create(file.path(wd, "writtenrasters"))}  # creates new folder if doesn't exist
writeRaster(s, file.path('writtenrasters', names(s)), bylayer=TRUE, format='GTiff') # takes time (add overwrite=TRUE if overwriting existing rasters)

################################################################################################################################
#Extract attribute data for each plot-life form and write a data.frame with data for building the landscape nutrition model ####
################################################################################################################################

#Read in rasters 
raster_data<-list.files(path=paste(wd, "writtenrasters", sep="/"), pattern="tif$", full.names=TRUE) 
s <- stack(raster_data) 
names(s)

# Extract values of rasters to sampling locations and create data.frame with GDM and attributes
ext<-extract(s, plots) ##Extract from raster stack for each plot location
plot.data <- data.frame(plot.data) ##Convert plot info attribute table to dataframe
data <- cbind(plot.data, ext)	##Bind the plot info and extracted landcover datainto a dataframe
head(data)
nrow(data)

data$Date<-as.Date(data$Date, "%m/%d/%Y") #Set dates and calculate Year
data$Year<-as.numeric(format(data$Date, '%Y'))
#Pick the correct year of landcover (esp) and fire
data$cover_class <- ifelse(data$Year == 2014, data$esp6_14,
                           ifelse(data$Year == 2015, data$esp6_15, NA))
data$t_fire <- ifelse(data$Year == 2014, data$t_fire_14,
                      ifelse(data$Year == 2015, data$t_fire_15, NA))
#Pick the correct year of summer (May - Aug) and spring (May-June) precipitation
data$sum_precip <- ifelse(data$Year == 2014, data$precip_2014,
				                  ifelse(data$Year == 2015, data$precip_2015, NA))
data$spr_precip <- ifelse(data$Year == 2014, data$spr_precip_2014,
				                  ifelse(data$Year == 2015, data$spr_precip_2015, NA))
#Pick the correct year of tiNDVI and NDVIamp
data$ndvi_ti <- ifelse(data$Year == 2014, data$ndvi_ti_2014,
				               ifelse(data$Year == 2015, data$ndvi_ti_2015, NA))
data$ndvi_amp <- ifelse(data$Year == 2014, data$ndvi_amp_2014,
				                ifelse(data$Year == 2015, data$ndvi_amp_2015, NA))

#Create new covariates, standardize covariates and finish building dataset
data$cover_class<-as.factor(data$cover_class)
data$area<-as.factor(data$Area)
data$elev_std<-((data$elev-(mean(data$elev)))/(sd(data$elev)))
data$slope_std<-((data$slope-(mean(data$slope)))/(sd(data$slope)))
data$hillshade_std<-((data$hillshade-(mean(data$hillshade)))/(sd(data$hillshade)))
data$cti_std<-((data$cti-(mean(data$cti)))/(sd(data$cti)))
data$cc_std<-((data$cc-(mean(data$cc)))/(sd(data$cc)))
data$sum_precip_std<-((data$sum_precip-(mean(data$sum_precip)))/(sd(data$sum_precip)))
data$spr_precip_std<-((data$spr_precip-(mean(data$spr_precip)))/(sd(data$spr_precip)))
data$ndvi_ti_std<-((data$ndvi_ti-(mean(data$ndvi_ti)))/(sd(data$ndvi_ti)))
data$ndvi_amp_std<-((data$ndvi_amp-(mean(data$ndvi_amp)))/(sd(data$ndvi_amp)))
data$gsri_std<-((data$gsri-(mean(data$gsri)))/(sd(data$gsri)))

# join to land cover classifications to attach names
clsref_esp <- data.frame(cbind(cover_class = c(1,2,3,4,5,6,7,8,9,10,11,12),
                               class_name = c("Mesic Forest (Burn >15)", "Dry Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                              "Dry Ag", "Valley Bottom Riparian","Montane Riparian", "Irrigated Ag",
                                              "Dry Forest Burn 0-5", "Dry Forest Burn 6-15",
                                              "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                                              "Rx Dry Forest Burn 0-5")))
clsref_esp$cover_class <- as.numeric(as.character(clsref_esp$cover_class))
data <- merge(data, clsref_esp, by="cover_class")

#data<-subset(data, !data$cover_class == 0) #Remove sample from "other" landcover 
head(data)
nrow(data)
write.csv(data, "data_GDM.csv")

##############################################################################################
#### Summarize GDM data                                                                   ####
##############################################################################################
dat.GDM <- read.csv("data_GDM.csv")
#dat.GDM <- data #if running code in full from above, can replace read.csv with this
head(dat.GDM)

sapply(dat.GDM,class)
dat.GDM$cover_class<-factor(dat.GDM$cover_class)
dat.GDM$Date<-as.Date(dat.GDM$Date, "%m/%d/%Y") #Set dates and calculate Year
dat.GDM<-subset(dat.GDM, dat.GDM$Season == "Summer") 
summarise(dat.GDM, aveGDM = mean(dat.GDM$GDM))
summarise(dat.GDM, sdGDM = sd(dat.GDM$GDM))

dat<-group_by(dat.GDM, cover_class)
GDMSummary=summarise(dat,
                     class_name=first(class_name),
                     count=n(),
                     AveGDM=mean(DM),
                     MedGDM=median(DM),
                     MinGDM=min(DM),
                     MaxGDM=max(DM),
                     SdGDM=sd(DM))
GDMSummary 
write.table(GDMSummary, "GDMSummary_landcover.csv", sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)

dat2<-group_by(dat.GDM, Area)
GDMSummary2=summarise(dat2,
                      count=n(),
                      AveGDM=mean(DM),
                      MedGDM=median(DM),
                      MinGDM=min(DM),
                      MaxGDM=max(DM),
                      SdGDM=sd(DM))
GDMSummary2
write.table(GDMSummary2, "GDMSummary_area.csv", sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)

dat3<-group_by(dat.GDM, cover_class, Area)
GDMSummary3=summarise(dat3,
                     class_name=first(class_name),
                     count=n(),
                     AveGDM=mean(DM),
                     MedGDM=median(DM),
                     MinGDM=min(DM),
                     MaxGDM=max(DM),
                     SdGDM=sd(DM))
GDMSummary3
write.table(GDMSummary3, "GDMSummary_area_landcover.csv", sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)

#####   GGPLOT  BOXPLOT   ###
library(plyr)
dat.GDM$class_name <- factor(dat.GDM$class_name, levels=c("Dry Forest Burn 0-5", "Rx Dry Forest Burn 0-5", "Dry Forest Burn 6-15", "Dry Forest (Burn >15)",   
                                                            "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", "Mesic Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                                            "Montane Riparian", "Valley Bottom Riparian", "Dry Ag", "Irrigated Ag"))
#labelling sample size
give.n <- function(x){ return(c(y = 0.36, label = length(x)))}
#xlabels <- ddply(dat.GDM, .(class_name), summarize, xlabels = paste(unique(class_name),' \n (n=', length(class_name),')       ', sep=""))

GDM_class= ggplot(data=dat.GDM, aes(x=class_name, y=DM)) +
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
#### Build summer nutrition models using backwards step-wise selection                    ####
##############################################################################################
dat.GDM <- read.csv("data_GDM.csv")
dat.GDM<-subset(dat.GDM, dat.GDM$Season == "Summer")  
dat.GDM$cover_class<-as.factor(dat.GDM$cover_class)
nrow(dat.GDM) 
head(dat.GDM)
summary(dat.GDM$GDM)

#Reorder cover classes and reference level for model (dry forest no burn first, then chronological)
dat.GDM$cover_class <- factor(dat.GDM$cover_class, levels=c("2","8", "12", "9", "1", "10", "11", "3", "6", "5", "4", "7"))

#Look at distribution of response variable and correlations
hist(dat.GDM$GDM, breaks = 50)

#Distribution of response
library(wesanderson)
pal <- wes_palette("Zissou", 10, type="continuous")
ggplot(data=dat.GDM, aes(x=class_name, y=opt, color=DM)) +
  geom_jitter(width=0.3, height=0.1, alpha=0.5) +
  theme(axis.text.x=element_text(size=12, angle=55, hjust=1, vjust = 1)) +
  scale_color_gradientn(colors=pal)

#Covariates of interest to check correlations
mydata <- dat.GDM %>% 
  select(elev, slope, cc, hillshade, cti, gsri, t_fire, sum_precip, spr_precip, ndvi_ti, ndvi_amp)
head(mydata)
##Spr and Sum Precip are correlated, NDVI ti and amp are correlated
## Elev and Spr precip 0.67, hillshade and gsri 0.67
cor(mydata) # correlation matrix
# plot the data and display r values
library(PerformanceAnalytics)
chart.Correlation(mydata)

m1 <- glm(gdm~cover_class+elev_std+slope_std+cti_std+cc_std+ndvi_amp_std+spr_precip_std+gsri_std, family = binomial(link=logit), data=dat.GDM)
step <- stepAIC(m1, direction="both")
step$anova  # display results  

mtop.ade.std <- glm(ade ~ cover_class + elev_std + ndvi_amp_std + gsri_std, family = binomial(link=logit), data=dat.GDM)
mtop.ade<-glm(ade ~ cover_class + elev + ndvi_amp + gsri, family = binomial, data=dat.GDM)
summary(mtop.ade.std)
confint(mtop.ade.std)

#dat.GDM <- dat.GDM[!dat.GDM$cover_class==4,]
m2 <- glm(opt~cover_class+elev_std+slope_std+cti_std+cc_std+ndvi_amp_std+spr_precip_std+gsri_std, family = binomial(link=logit), data=dat.GDM)
step <- stepAIC(m2, direction="both")
step$anova  # display results  

mtop.opt.std <- glm(opt ~ cover_class + gsri_std, family = binomial(link=logit), data=dat.GDM)
mtop.opt<- glm(opt ~ cover_class + gsri, family = binomial(link=logit), data=dat.GDM)
summary(mtop.opt.std)
confint(mtop.opt.std)

#Write out model summary
ESTs <- mtop.opt.std
names(ESTs$coefficients) <- c("Intercept", "Dry Forest Burn 0-5", "Dry Forest Rx Burn 0-5", "Dry Forest Burn 6-15",
                                      "Mesic Forest (Burn >15)", "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                                      "Grass/Shrub/Open Woodland", "Montane Riparian", "Valley Bottom Riparian", "Dry Ag", "Irrigated Ag", 
                                      "Solar Radiation")
ESTs <- data.frame(ESTs$coefficients)
ESTs$class_name <- rownames(ESTs)
CIs <- confint(mtop.opt.std)
rownames(CIs) <- c("Intercept", "Dry Forest Burn 0-5", "Dry Forest Rx Burn 0-5", "Dry Forest Burn 6-15",
                  "Mesic Forest (Burn >15)", "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                  "Grass/Shrub/Open Woodland", "Montane Riparian", "Valley Bottom Riparian", "Dry Ag", "Irrigated Ag", 
                  "Solar Radiation")
CIs <- data.frame(CIs)
CIs$class_name <- rownames(CIs)
mtop.opt.ESTIMATES <- left_join(ESTs, CIs, by="class_name")
mtop.opt.ESTIMATES <- mtop.opt.ESTIMATES %>%
  rename(LCI=X2.5.., UCI=X97.5.., Covariate=class_name, Estimate=ESTs.coefficients) %>%
  select(Covariate, Estimate, LCI, UCI) # reorder
write.csv(mtop.opt.ESTIMATES, "mtop.opt.estimates.csv")


# Exponentiate the coefficients and interpret them as odds-ratios. 
#Get odds ratio and 95% CI on odds ratio.  For 1 unit increase in COEF, odds of opt increase/decrease by OR...
exp(cbind(OR = coef(mtop.opt.std), confint(mtop.opt.std)))

#Estimate the predicted probability of optm for each cover class holding gsri at mean
newdata <- with(dat.GDM,
                 data.frame(gsri_std = mean(gsri_std), cover_class= factor(1:12)))
newdata <- merge(newdata, clsref_esp, by="cover_class")

## view data frame
newdata
newdata$opt.prob <- predict(mtop.opt.std, newdata = newdata, type = "response")  #predicted probabilty of optm
newdata

#code to generate the predicted probabilities (the first line below) is the same as before,  also get standard errors to plot a confidence interval. 
#get the estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
newdata2 <- with(dat.GDM,
                 data.frame(gsri_std = rep(seq(from = min(gsri_std), to = max(gsri_std), length.out = 100), 12),
                            cover_class = factor(rep(1:12, each = 100))))
#merge with classification names
clsref_esp <- data.frame(cbind(cover_class = c(1,2,3,4,5,6,7,8,9,10,11,12),
                               class_name = c("Mesic Forest (Burn >15)", "Dry Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                              "Dry Ag", "Valley Bottom Riparian","Montane Riparian", "Irrigated Ag",
                                              "Dry Forest Burn 0-5", "Dry Forest Burn 6-15",
                                              "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                                              "Rx Dry Forest Burn 0-5")))
clsref_esp$cover_class <- as.numeric(as.character(clsref_esp$cover_class))
newdata2 <- merge(newdata2, clsref_esp, by="cover_class")
newdata3 <- cbind(newdata2, predict(mtop.opt.std, newdata = newdata2, type="link", se=TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
## view first few rows of final dataset
head(newdata3)
newdata3$class_name <- droplevels(newdata3$class_name)
newdata3$class_name <- factor(newdata3$class_name, levels=c("Dry Forest Burn 0-5", "Rx Dry Forest Burn 0-5", "Dry Forest Burn 6-15", "Dry Forest (Burn >15)",   
                                                            "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", "Mesic Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                                            "Montane Riparian", "Valley Bottom Riparian", "Dry Ag", "Irrigated Ag"))

#Create graph of predicted probabilty of Optimal for each cover class
p<-ggplot(newdata3, aes(x = gsri_std, y = PredictedProb)) +
  #geom_ribbon(aes(ymin = LL, ymax = UL, fill = class_name), alpha = .2) +
  #geom_line(aes(colour = class_name), size=1) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(aes(), size=1) +
  facet_wrap(~ class_name) +  # remove this and next line if want all on one graph
  theme(legend.position="none") +
  labs(x="Solar Radiation", y="Predicted Probability")
p
ggsave("Prob Optm per Cover Class_nocolor.tiff", plot = p, width=8, height =6, units ="in", dpi = 300)

#Measure of model fit and significance of the overall model (likelihood ratio test), test statistic is the difference between the residual deviance for the model with predictors and the null model.
with(mtop.opt.std, null.deviance - deviance)
#The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the mode, and can be obtained using:
with(mtop.opt.std, df.null - df.residual)
#Finally, the p-value can be obtained using:
with(mtop.opt.std, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#p-value of less than 0.001 tells us that our model as a whole fits significantly better than an empty model. 

#Asessing predictive acuracy of the model on the training data, using decision boundary of 0.5, If P(y=1|X) > 0.5 then y = 1 otherwise y=0. 
test.data <- dat.GDM %>% 
  select(cover_class, gsri_std)
fitted.results <- predict(mtop.opt.std,newdata=test.data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != dat.GDM$opt)
print(paste('Accuracy',1-misClasificError))

##############################################################################################
#### Build raster prediction of summer forage quality models                              ####
##############################################################################################
#based on unstandardized coefficients

#Read in rasters 
raster_data<-list.files(path=paste(wd, "writtenrasters", sep="/"), pattern="tif$", full.names=TRUE) # these were exported after being stacked above
s <- stack(raster_data) 
names(s)

#Rename covariates according to position in stack
n_esp <- which(names(s)=="esp6_15"); n_esp    # using esp6_15 for "cover_class" covariate in model, so first get no. in stack
n_ndvi_amp <- which(names(s)=="ndvi_amp_2015"); n_ndvi_amp    # using ndvi_amp_2015 for "ndvi_amp" covariate in model
names(s[[n_esp]]) <- "cover_class"    # and based on number in stack, rename
names(s[[n_ndvi_amp]]) <- "ndvi_amp"    

#Predict the model based on raster covariates

#~~~~~ OPTION A: using built-in raster::predict() on raster stack
# --first create function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE, type='response') # type='respone' is probability scale, type='link' is log-odds scale
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# --then run that function in raster::predict, ouputting a raster of both probability and SE
rasts.pred.opt <- predict(s, mtop.opt, fun=predfun, index=1:2, progress="text") # predfun returns two variables (response and se), so need to include index=1:2
rasts.pred.ade <- predict(s, mtop.ade, fun=predfun, index=1:2, progress="text")
names(rasts.pred.opt) <- c("prob_opt_sum","se_opt_sum") 
names(rasts.pred.ade) <- c("prob_ade_sum","se_ade_sum")
plot(rasts.pred.opt) # plot both
plot(rasts.pred.opt[["prob_opt_sum"]]) # plot one at a time
plot(rasts.pred.opt[["se_opt_sum"]])
plot(rasts.pred.ade)

#Write rasters (writes both rasters in the stack)
writeRaster(rasts.pred.opt, names(rasts.pred.opt), bylayer=TRUE, format='GTiff', overwrite=TRUE)
writeRaster(rasts.pred.ade, names(rasts.pred.ade), bylayer=TRUE, format='GTiff', overwrite=TRUE)
#~~~~~~~~~~~~~~~

#### INCOMPLETE ###
#~~~~~~ OPTION B: extracting coefficients and using raster math
# Extract coefficients
summary(mtop.opt)
b0 <- mtop.opt$coefficients[[1]]
b1 <- mtop.opt$coefficients[[2]]
b2 <- mtop.opt$coefficients[[3]]
b3 <- mtop.opt$coefficients[[4]]
b4 <- mtop.opt$coefficients[[5]]
b5 <- mtop.opt$coefficients[[6]]
b6 <- mtop.opt$coefficients[[7]]
b7 <- mtop.opt$coefficients[[8]]
b8 <- mtop.opt$coefficients[[9]]
b9 <- mtop.opt$coefficients[[10]]
b10 <- mtop.opt$coefficients[[11]]
b11 <- mtop.opt$coefficients[[12]]
b12 <- mtop.opt$coefficients[[13]]

# Creating new stack to simplify
s2 <- stack(s[[7]],s[[8]])
s2

test2 <- overlay(s2, fun=function(x,y) { exp(b0 + b1*x + b2*x + b3*x + b4*x + b5*x + b6*x + b7*x + b8*x + b9*x + b10*x + b11*x + b12*y)/
                                         (1 + exp(b0 + b1*x + b2*x + b3*x + b4*x + b5*x + b6*x + b7*x + b8*x + b9*x + b10*x + b11*x + b12*y)) })

####################

prob_opt_sum <- raster("prob_opt_sum.tif") # read back in
prob_ade_sum <- raster("prob_ade_sum.tif")

#Convert >50% probability to adequate/optimal
fun <- function(x) { ifelse(x>=0.5, 1, 0)}
DMD_opt_sum <- calc(prob_opt_sum, fun=fun)
DMD_ade_sum <- calc(prob_ade_sum, fun=fun)
plot(DMD_opt_sum)
plot(DMD_ade_sum)

#Write rasters
writeRaster(DMD_opt_sum, "DMD_opt_sum.tif", overwrite=TRUE)
writeRaster(DMD_ade_sum, "DMD_ade_sum.tif", overwrite=TRUE)

################################################################################################
####  Extract proportion predicted DMD optimal/adequate per cover class within study area   ####
################################################################################################

#Read in rasters 
raster_data<-list.files(path=paste(wd, "writtenrasters", sep="/"), pattern="tif$", full.names=TRUE) # these were exported after being stacked above
s <- stack(raster_data) 
names(s)

#Rename land cover covariate according to position in stack
n_esp <- which(names(s)=="esp6_15"); n_esp    # using esp6_15 for "cover_class" covariate in model, so first get no. in stack
names(s[[n_esp]]) <- "cover_class"    # and based on number in stack, rename
names(s)

#Read in 0/1 DMD optimum and adequate rasters
DMD_opt_sum <- raster("DMD_opt_sum.tif")
DMD_ade_sum <- raster("DMD_ade_sum.tif")

#Read in study area, reproject to MT state plane, and merge the three study population polygons
studyarea <- readOGR(dsn = if(wd==wd.kp) { "F:\\SapphireVegModels\\Landcover\\studyarea"  # same polygon used in landcover code
  } else {"..\\..\\..\\..\\GIS\\Projects\\Landcover\\Landcover_BitterrootFireEffects\\data\\studyarea" }, 
                     layer ="studyarea_divided")
proj.crs <- "+proj=lcc +lat_1=49 +lat_2=45 +lat_0=44.25 +lon_0=-109.5 +x_0=600000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
studyarea <-spTransform(studyarea, proj.crs)
studyarea <- gUnaryUnion(studyarea)
plot(studyarea)

#Create dataframe of cover names to merge to
clsref_esp <- data.frame(cbind(cover_class = c(1,2,3,4,5,6,7,8,9,10,11,12),
                               class_name = c("Mesic Forest (Burn >15)", "Dry Forest (Burn >15)", "Grass/Shrub/Open Woodland",
                                              "Dry Ag", "Valley Bottom Riparian","Montane Riparian", "Irrigated Ag",
                                              "Dry Forest Burn 0-5", "Dry Forest Burn 6-15",
                                              "Mesic Forest Burn 0-5", "Mesic Forest Burn 6-15", 
                                              "Rx Dry Forest Burn 0-5")))
clsref_esp$cover_class <- as.numeric(as.character(clsref_esp$cover_class))

#Only need to run this segment once to output rasters (because takes so much time to re-run)
#Clip land cover and model prediction to study area using crop and mask
#cover_class <- s[["cover_class"]]
#cover_class_aoi <- crop(cover_class, extent(studyarea))
#cover_class_aoi <- mask(cover_class_aoi, studyarea) # takes time ~6.5 min
#names(cover_class_aoi) <- "cover_class_aoi"
#writeRaster(cover_class_aoi, "cover_class_aoi.tif", overwrite=TRUE)

#DMD_opt_sum_aoi <- crop(DMD_opt_sum, extent(studyarea))
#DMD_opt_sum_aoi <- mask(DMD_opt_sum_aoi, studyarea) # takes time ~6.5 min
#names(DMD_opt_sum_aoi) <- "DMD_opt_sum_aoi"
#writeRaster(DMD_opt_sum_aoi, "DMD_opt_sum_aoi.tif", overwrite=TRUE)

#DMD_ade_sum_aoi <- crop(DMD_ade_sum, extent(studyarea))
#DMD_ade_sum_aoi <- mask(DMD_ade_sum_aoi, studyarea) # takes time ~6.5 min
#names(DMD_ade_sum_aoi) <- "DMD_ade_sum_aoi"
#writeRaster(DMD_ade_sum_aoi, "DMD_ade_sum_aoi.tif", overwrite=TRUE)


##OPTIMUM forage quality
#Read back in rasters and stack
cover_class_aoi <- raster("cover_class_aoi.tif")
DMD_opt_sum_aoi <- raster("DMD_opt_sum_aoi.tif")

s_aoi <- stack(DMD_opt_sum_aoi, cover_class_aoi)

DMDopt_sum_cls <- overlay(s_aoi, fun=function(x,y) x*y) # multiplies dmd prediction (0's & 1's) by land cover
plot(DMDopt_sum_cls)
DMDopt_sum_freq <- data.frame(freq(DMDopt_sum_cls)) # get freqency table
DMDopt_sum_freq <- DMDopt_sum_freq[!is.na(DMDopt_sum_freq$value),] # remove NA's (outside of study area)
aoi.cells <- sum(DMDopt_sum_freq$count) # total no. cells in study area
DMDopt_sum_freq <- DMDopt_sum_freq[DMDopt_sum_freq$value != 0,] # remove cells classified as no optimum forage
DMDopt_sum_freq <- DMDopt_sum_freq %>%
    rename(class=value) %>%
    mutate(aoi_km2=round((aoi.cells*900)*1e-6,2),      # total area (km2) of study area 
           opt_km2=round((count*900)*1e-6,2),          # total area (km2) of optimal forage in cover class 
           opt_prop=round(opt_km2/aoi_km2,7)) %>%      # proportion optimal forage in study area
    left_join(clsref_esp, by=c("class"="cover_class")) %>%  # join to cover classifications to attach names
    arrange(desc(opt_prop))    # arrange from highest to lowest proportion
DMDopt_sum_freq
write.csv(DMDopt_sum_freq, "DMDoptSummary_landcover.csv")
 

##ADEQUATE forage quality
#Read back in rasters and stack
cover_class_aoi <- raster("cover_class_aoi.tif")
DMD_ade_sum_aoi <- raster("DMD_ade_sum_aoi.tif")

s_aoi <- stack(DMD_ade_sum_aoi, cover_class_aoi)

DMDade_sum_cls <- overlay(s_aoi, fun=function(x,y) x*y) # multiplies dmd prediction (0's & 1's) by land cover
plot(DMDade_sum_cls)
DMDade_sum_freq <- data.frame(freq(DMDade_sum_cls)) # get freqency table
DMDade_sum_freq <- DMDade_sum_freq[!is.na(DMDade_sum_freq$value),] # remove NA's (outside of study area)
aoi.cells <- sum(DMDade_sum_freq$count) # total no. cells in study area
DMDade_sum_freq <- DMDade_sum_freq[DMDade_sum_freq$value != 0,] # remove cells classified as no adequate forage
DMDade_sum_freq <- DMDade_sum_freq %>%
    rename(class=value) %>%
    mutate(aoi_km2=round((aoi.cells*900)*1e-6,2),      # total area (km2) of study area 
           ade_km2=round((count*900)*1e-6,2),          # total area (km2) of adequate forage in cover class 
           ade_prop=round(ade_km2/aoi_km2,7)) %>%      # proportion adequate forage in study area
    left_join(clsref_esp, by=c("class"="cover_class")) %>%  # join to cover classifications to attach names
    arrange(desc(ade_prop))   # arrange from highest to lowest proportion
DMDade_sum_freq
write.csv(DMDade_sum_freq, "DMDadeSummary_landcover.csv")



#################################################################################
####          Decadal prediction of DMD optimum and DMD adequate             ####
#################################################################################  Just optimum for now

## ~~~~~~~~~~ INCORPORATE DECADAL FIRE INTO LAND COVER ~~~~~~~~~~~~~~~~~~~~~~~~##

#Read in esp4 land cover model and all fire decades of interest
esp4 <- raster("../Landcover/finalrasters/esp4.tif")
firehis <- list.files(path=paste(wd, "../DecadalFires", sep="/"), pattern="tif$", full.names=TRUE) # these from decadalAccumFireHistory.gdb
firehis_names <- list.files(path=paste(wd, "../DecadalFires", sep="/"), pattern="tif$", full.names=FALSE) # get names only
firehis_names <- gsub(".tif", "", firehis_names) # remove .tif from name

#Crop and Resample firehistory rasters to match extent of land cover for stacking
fires <- stack(firehis)
fires <- crop(fires, extent(esp4)) # took ~4 min
fires <- resample(fires, esp4, "ngb") # took ~1 min

#Prepare for-loop
decade <- as.numeric(gsub("....([0-9]+).*$", "\\1", firehis_names)) # create list of decades
fire_fun <- function(e4,fr) {                      # fire function to calculate land cover incorporating each decadal fire, implemented in overlay() in forloop
  ifelse(e4 == 1 & fr >= decade[i] - 5, 10,        # if Mesic Forest & fire within 5 yrs == Mesic Burn 0-5
         ifelse(e4 == 2 & fr >= decade[i] - 5, 8,  # if Dry-Mesic Forest & fire within 5 yrs == Dry Burn 0-5
                ifelse(e4 == 1 & fr < decade[i] - 5 & fr >= decade[i] - 15, 11,        # if Mesic Forest & fire 6-15 yrs == Mesic Burn 6-15
                       ifelse(e4 == 2 & fr < decade[i] - 5 & fr >= decade[i] - 15, 9,  # if Dry-Mesic Forest & fire 6-15 years == Dry Burn 6-15
                              e4)))) 
}
esp5_fire_list <- vector()  # create empty vector for saving a list of each decadal land cover raster

# Run for-loop incorporating each decadal fire into land cover (~4 min)
for(i in 1:nlayers(fires)) {
  s <- stack(esp4, fires[[i]])
  esp5 <- overlay(s, fun = fire_fun)
  assign(paste("esp5", firehis_names[i], sep = "_"), esp5)         # output into global environment
  esp5_fire_list[i] <- paste("esp5", firehis_names[i], sep = "_")  # put into list
  }

#Stack all the layers to write rasters to disk
esp5_fire_list <- noquote(esp5_fire_list) # remove quotes from list
s <- stack(mget(esp5_fire_list)) # stacks each decadal land cover raster
s
names(s)

rm(list=esp5_fire_list, esp5_fire_list, firehis, firehis_names, esp4, esp5, fire_fun, decade)

#Writing all rasters into new folder
if(!file.exists("writtenrasters\\decadallandcover")) {dir.create(file.path(wd, "writtenrasters\\decadallandcover"))}  # creates new folder if doesn't exist
writeRaster(s, file.path('writtenrasters\\decadallandcover', names(s)), bylayer=TRUE, format='GTiff', overwrite=TRUE) # takes time

## ~~~~~~~~~~ PREDICT DECADAL FORAGE QUALITY (BASED ON OPTIMAL MODEL) ~~~~~~~~~~~~~~~~~~~~~~~~##

#Read back in decadal land cover rasters
esp5fires <- list.files(path=paste(wd, "writtenrasters/decadallandcover", sep="/"), pattern="tif$", full.names=TRUE)
esp5fires_names <- list.files(path=paste(wd, "writtenrasters/decadallandcover", sep="/"), pattern="tif$", full.names=FALSE) 
esp5fires_names <- gsub(".tif", "", esp5fires_names)

#Prepare prediction for-loop -- predicting PROBABALITY OF OPTIMAL FORAGE (prOPT)
# -- Load, prepare data, and run model (same as prvs code)
dat.DMD <- read.csv("data_DMD.csv")
dat.DMD <- subset(dat.DMD, dat.DMD$Season == "Summer")
q<-quantile(dat.DMD$GDM) 
q_ade<-q[3]
q_opt<-q[4]
dat.DMD$ade<-ifelse(dat.DMD$GDM > q_ade, 1, 0)
dat.DMD$opt<-ifelse(dat.DMD$GDM > q_opt, 1, 0)
dat.DMD$cover_class <- as.factor(dat.DMD$cover_class)
mtop.opt<- glm(opt ~ cover_class + gsri, family = binomial(link=logit), data=dat.DMD)
summary(mtop.opt)
confint(mtop.opt)

prOPT_names <- gsub("esp5_fhis", "", esp5fires_names) # setting up naming
prOPT_list <- vector() # Create empty vector for saving a list of each decadal forage quality prediction
gsri <- raster("writtenrasters\\gsri.tif") # read in gsri
decadalcover <- stack(esp5fires)

#Prediction for-loop (with a progress bar...horray!)
for(i in 1:nlayers(decadalcover)) {
  s <- stack(gsri, decadalcover[[i]])       # add each decadal land cover to stack with gsri
  names(s[[2]]) <- "cover_class"            # rename decadal land cover to how named in glm
  prOPT <- predict(s, mtop.opt, type="response", progress="text")     # apply prediction function to create raster based on model
  assign(paste("prOPT", prOPT_names[i], sep = "_"), prOPT)        # output into global environment
  prOPT_list[i] <- paste("prOPT", prOPT_names[i], sep = "_")      # put into list
}

#Stack all the layers to write to disk
prOPT_list <- noquote(prOPT_list) # remove quotes from list
s.prOPT <- stack(mget(prOPT_list))

s.prOPT
names(s.prOPT)
rm(list=prOPT_list, prOPT_list, esp5fires, esp5fires_names, prOPT)

#Writing all rasters into new folder
if(!file.exists("writtenrasters\\decadalFQ")) {dir.create(file.path(wd, "writtenrasters\\decadalFQ"))}  # creates new folder if doesn't exist
if(!file.exists("writtenrasters\\decadalFQ\\prOPT")) {dir.create(file.path(wd, "writtenrasters\\decadalFQ\\prOPT"))}  # creates new folder within previous if doesn't exist
writeRaster(s.prOPT, file.path('writtenrasters\\decadalFQ\\prOPT', names(s.prOPT)), bylayer=TRUE, format='GTiff', overwrite=TRUE) # takes time

## ~~~~~~~~~~ CONVERT PROBABILITY OF FORAGE QUALITY TO OPTIMAL FORAGE ~~~~~~~~~~~~~~~~~~~~~~~~##

#Read back in forage quality rasters
prOPT <- list.files(path=paste(wd, "writtenrasters/decadalFQ/prOPT", sep="/"), pattern="tif$", full.names=TRUE)
prOPT_names <- list.files(path=paste(wd, "writtenrasters/decadalFQ/prOPT", sep="/"), pattern="tif$", full.names=FALSE) 
prOPT_names <- gsub(".tif", "", prOPT_names)

#Prepare prediction for-loop
OPT_names <- gsub("prOPT_", "", prOPT_names) # setting up for naming in for loop
OPT_list <- vector() # Create empty vector for saving a list of each decadal forage quality prediction
decadalprOPT <- stack(prOPT)
OPT_fun <- function(x) { ifelse(x >= 0.5, 1, 0) } # function to assign 1s and 0s implemented in forloop

for(i in 1:length(prOPT)) {
  prOPTrast <- raster(prOPT[i])
  OPTrast <- calc(prOPTrast, fun=OPT_fun)    # assign 1s and 0s
  assign(paste("OPT", OPT_names[i], sep = "_"), OPTrast)    # name and output into global environment
  OPT_list[i] <- paste("OPT", OPT_names[i], sep = "_")      # put into list
}

#Stack all the layers to write to disk
OPT_list <- noquote(OPT_list) # remove quotes from list
s.OPT <- stack(mget(OPT_list))

s.OPT
names(s.OPT)
rm(list=OPT_list, OPT_list, prOPT_names, OPT_names, prOPT, OPT_fun, prOPTrast, OPTrast)

#Writing all rasters into new folder
if(!file.exists("writtenrasters\\decadalFQ\\OPT")) {dir.create(file.path(wd, "writtenrasters\\decadalFQ\\OPT"))}  # creates new folder if doesn't exist
writeRaster(s.OPT, file.path('writtenrasters\\decadalFQ\\OPT', names(s.OPT)), bylayer=TRUE, format='GTiff', overwrite=TRUE) # takes time 


#########################################################################################
#### Elk population ranges - extract mean % Optimal DMD (under 2012 conditions) ####
#########################################################################################
#Read in final model predictions of ade and optm DMD
DMDopt_sum <- raster("..\\DataCode\\DMDopt_sum.tif") 
DMDade_sum <- raster("..\\DataCode\\DMDade_sum.tif")

#Read in and create Sapphire summer range
Sapp_points<-read.csv("..\\AnalysisRasters\\Sapph_gps_data.csv", header=T)  #Points brought in in Lat/Long
Sapp_points$Date <- as.Date(Sapp_points$Date, "%d-%b-%Y")
head(Sapp_points)
nrow(Sapp_points)
Sapp_points <- subset(Sapp_points, Sapp_points$Sex == "Female")
sum_dat.sapp<- subset(Sapp_points, Sapp_points$Date > as.Date("2014-06-30") & Sapp_points$Date < as.Date("2014-9-01")|
                        Sapp_points$Date > as.Date("2015-06-30") & Sapp_points$Date < as.Date("2015-09-01"))
#Get Sapp summer points, write out the points to dataframe, to spatial data frame, to shapefile
sum.xy.sapp <- data.frame("x"=sum_dat.sapp$Long,"y"=sum_dat.sapp$Lat)
latlong = CRS("+init=epsg:4326")
xysp.sapp =SpatialPoints(sum.xy.sapp, proj4string = latlong)
kud.sapp  <- kernelUD(xysp.sapp, h="href", grid = 1000)
#Define 95% volume contour and export to shapefile
sum.95vol.sapp <- getverticeshr(kud.sapp , percent = 95, ida = NULL, unin = "m", unout = "km")
#Project into stateplane and write the shapefile with 95% summer volume 
stateplane = CRS("+init=epsg:2818")
sum.95vol.sapp <- spTransform(sum.95vol.sapp ,stateplane)
writeOGR(sum.95vol.sapp , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='sapp_sum_95vol', driver="ESRI Shapefile", overwrite_layer=TRUE)
#95% MCP HR
smcp.sapp =mcp(xysp.sapp , percent=95, unin="m", unout="km")
smcp.sapp <- spTransform(smcp.sapp ,stateplane)
writeOGR(smcp.sapp , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='sapp_sum_mcp', driver="ESRI Shapefile", overwrite_layer=TRUE)

#Read in and create EF summer range
ef_points<-read.csv("..\\AnalysisRasters\\Broot_gps_data.csv", header=T)  #Points brought in in Lat/Long, already subset to summer
ef_points$Date <- as.Date(ef_points$Date, "%d-%b-%Y")
ef_points <- subset(ef_points, EF_points$Herd == "EF")
#Get ef summer points, write out the points to dataframe, to spatial data frame, to shapefile
sum.xy.ef <- data.frame("x"=ef_points$Longitude,"y"=ef_points$Latitude)
latlong = CRS("+init=epsg:4326")
xysp.ef =SpatialPoints(sum.xy.ef, proj4string = latlong)
kud.ef  <- kernelUD(xysp.ef, h="href", grid = 1000)
#Define 95% volume contour and export to shapefile
sum.95vol.ef <- getverticeshr(kud.ef , percent = 95, ida = NULL, unin = "m", unout = "km")
#Project into stateplane and write the shapefile with 95% summer volume 
stateplane = CRS("+init=epsg:2818")
sum.95vol.ef <- spTransform(sum.95vol.ef ,stateplane)
writeOGR(sum.95vol.ef , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='ef_sum_95vol', driver="ESRI Shapefile", overwrite_layer=TRUE)
#95% MCP HR
smcp.ef =mcp(xysp.ef , percent=95, unin="m", unout="km")
smcp.ef <- spTransform(smcp.ef ,stateplane)
writeOGR(smcp.ef , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='ef_sum_mcp', driver="ESRI Shapefile", overwrite_layer=TRUE)

#Read in and create wf summer range
wf_points<-read.csv("..\\AnalysisRasters\\Broot_gps_data.csv", header=T)  #Points brought in in Lat/Long, already subset to summer
wf_points$Date <- as.Date(wf_points$Date, "%d-%b-%Y")
wf_points <- subset(wf_points, wf_points$Herd == "WF")
#Get wf summer points, write out the points to dataframe, to spatial data frame, to shapwfile
sum.xy.wf <- data.frame("x"=wf_points$Longitude,"y"=wf_points$Latitude)
latlong = CRS("+init=epsg:4326")
xysp.wf =SpatialPoints(sum.xy.wf, proj4string = latlong)
kud.wf  <- kernelUD(xysp.wf, h="hrwf", grid = 1000)
#Dwfine 95% volume contour and export to shapwfile
sum.95vol.wf <- getverticeshr(kud.wf , percent = 95, ida = NULL, unin = "m", unout = "km")
#Project into stateplane and write the shapwfile with 95% summer volume 
stateplane = CRS("+init=epsg:2818")
sum.95vol.wf <- spTransform(sum.95vol.wf ,stateplane)
writeOGR(sum.95vol.wf , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='wf_sum_95vol', driver="ESRI Shapwfile", overwrite_layer=TRUE)
#95% MCP HR
smcp.wf =mcp(xysp.wf , percent=95, unin="m", unout="km")
smcp.wf <- spTransform(smcp.wf ,stateplane)
writeOGR(smcp.wf , dsn = 'F:\\SapphireVegModels\\AnalysisRasters', layer ='wf_sum_mcp', driver="ESRI Shapwfile", overwrite_layer=TRUE)



##Summarize DM on summer and winter ranges
# create matrix to hold estimates of DM for elk populations seasonal ranges
DMD_range_stats=data.frame(matrix(nrow=1,ncol=7,dimnames=list(1:2,c("Season", "Sapp_Ade","Sapp_Opt","EF_Ade","EF_Opt", "WF_Ade","WF_Opt"))))
z_sapp_ade  <- extract(DMDade_sum, smcp.sapp, fun = mean, na.rm = T, df = T)
z_sapp_opt  <- extract(DMDopt_sum, smcp.sapp, fun = mean, na.rm = T, df = T)
z_ef_ade  <- extract(DMDade_sum, smcp.ef, fun = mean, na.rm = T, df = T)
z_ef_opt  <- extract(DMDopt_sum, smcp.ef, fun = mean, na.rm = T, df = T)
z_wf_ade  <- extract(DMDade_sum, smcp.wf, fun = mean, na.rm = T, df = T)
z_wf_opt  <- extract(DMDopt_sum, smcp.wf, fun = mean, na.rm = T, df = T)

DMD_range_stats[1,1]=paste("Summer2012", sep="")	
DMD_range_stats[1,2]=z_sapp_ade[,2] 	
DMD_range_stats[1,3]=z_sapp_opt[,2] 	 		
DMD_range_stats[1,4]=z_ef_ade[,2] 	
DMD_range_stats[1,5]=z_ef_opt[,2] 	
DMD_range_stats[1,6]=z_wf_ade[,2] 	
DMD_range_stats[1,7]=z_wf_opt[,2] 

DMD_range_stats
write.csv(DMD_range_stats,"DMD_range_stats.csv",row.names=F)   
#Write a loop to extract these for each decade and write to a table.