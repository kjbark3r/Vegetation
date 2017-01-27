##########################################################
#     PREDICTING ELK FORAGE ABUNDANCE DURING SUMMER      #
#      ACROSS NORTH SAPPHIRES IN SUMMER 2014 & 2015      #
#                   KRISTIN BARKER                       #
#                      JAN 2017                          #
##########################################################


#################
#### Setup   ####
#################

library(raster)
library(rgdal) # read/write shp's
library(ggplot2)
library(AICcmodavg)
library(dplyr) #general awesomeness

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

################################
#### Data Setup (run once)  ####
################################

# abundance (biomass, g/m^2) per plot , plus plot info
## from forageabundance.R
plot.data <- read.csv("biomass-plot-summeronly.csv", header=T) 

# plots shapefile (from foragequality_DE.R)
plots <- readOGR(".", layer ='biomass-plot-summer')

#all rasters for covariates
raster_data <- list.files(
  path=paste(wd, "writtenrasters/uncropped", sep="/"), 
  pattern="tif$", 
  full.names=TRUE) 
s <- stack(raster_data) 
names(s)

# cover class names
clsref <- data.frame(landcov = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     class_name = c("Mesic Forest (Burn >15)", #1
                                    "Dry Forest (Burn >15)", #2 
                                    "Grass/Shrub/Open Woodland", #3
                                    "Dry Ag", #4
                                    "Valley Bottom Riparian", #5
                                    "Montane Riparian", #6
                                    "Irrigated Ag", #7
                                    "Dry Forest Burn 0-5", #8
                                    "Dry Forest Burn 6-15", #9
                                    "Mesic Forest Burn 0-5",#10
                                    "Mesic Forest Burn 6-15", #11
                                    "Rx Dry Forest Burn 0-5")) #12

#############################
# Attribute data per plot ###
#############################

ext <- raster::extract(s, plots) # extract from raster stack for each plot

dat <- cbind(plot.data, ext) %>%	# add extracted values to plot data
  within(Date <- as.Date(Date, "%Y-%m-%d")) %>% # format dates
  mutate(  #select correct years of covariates
    Year = as.numeric(format(Date, '%Y')),
    landcov = ifelse(Year == 2014, landcov_14, landcov_15),
    ndvi_amp = ifelse(Year == 2014, ndvi_amp_14, ndvi_amp_15),    
    ndvi_dur = ifelse(Year == 2014, ndvi_dur_14, ndvi_dur_15),
    ndvi_ti = ifelse(Year == 2014, ndvi_ti_14, ndvi_ti_15),
    precip = ifelse(Year == 2014, precip_14, precip_15),
    spr_precip = ifelse(Year == 2014, spr_precip_14, spr_precip_15)
    ) %>%
  left_join(clsref, by = "landcov") %>% #add cover class names
  within(landcov <- as.factor(landcov)) %>%
  #classify "NoData" NDVI durations as NAs
  mutate(ndvi_dur = ifelse(ndvi_dur<90|ndvi_dur>365, NA, ndvi_dur)) %>%
  mutate(  #standardize covariates
    cc_std = ((cc-(mean(cc)))/(sd(cc))),
    elev_std = ((elev-(mean(elev)))/(sd(elev))),
    radn_std = ((radn-(mean(radn)))/(sd(radn))),
    ndvi_amp_std = ((ndvi_amp-(mean(ndvi_amp)))/(sd(ndvi_amp))),
    ndvi_dur_std = ((ndvi_dur-(mean(ndvi_dur, na.rm=T)))/(sd(ndvi_dur, na.rm=T))),
    ndvi_ti_std = ((ndvi_ti-(mean(ndvi_ti)))/(sd(ndvi_ti))),
    precip_std = ((precip-(mean(precip)))/(sd(precip))),
    spr_precip_std = ((spr_precip-(mean(spr_precip)))/(sd(spr_precip))),
    slope_std = ((slope-(mean(slope)))/(sd(slope)))) %>%
  dplyr::select(-contains("_1")) # remove individual year columns

write.csv(dat, file="biomass-model-data.csv", row.names=F)


###################################################################

##########################
#### HERBACEOUS MODEL ####
##########################

# read in DE data (if code not run in full from above)
dat <- read.csv("biomass-model-data.csv") %>%
  rename(gHerb = HerbaceousForageBiomass,
         gShrub = ForageShrubBiomass)

# Set factor levels for landcover type #
ref.lev <- dat %>%
  dplyr::select(c(landcov, class_name, gHerb)) %>%
  group_by(class_name, landcov) %>%
  summarise(AvgGherb = mean(gHerb)) %>%
  arrange(AvgGherb) %>% ## Order from least to most biomass
  ungroup() 
ref.lev # so reference level is landcover type with lowest abundance
dat$landcov <- factor(dat$landcov, levels = as.vector(ref.lev$landcov))
write.csv(ref.lev, file = "herb-biomass-by-landcover.csv", row.names=F)

# look at distribution of response
hist(dat$gHerb, breaks = 100) #normal enough
hist(log10(dat$gHerb))
hist(log10(dat$gHerb+0.02))

# check correlations and relationships
dat.cor <- dat %>%
  dplyr::select(cc_std, elev_std, landcov, radn_std, ndvi_amp_std, ndvi_dur_std, 
         ndvi_ti_std, precip_std, spr_precip_std, slope_std, gHerb)
source("../zMisc/pairs-panels.R")
pairs.panels(dat.cor) # bassing's prof's code to handle factors
# correlation coefficients > 0.6
  ## 0.91 spring precip & growing season precip
  ## 0.80 ndvi_ti & ndvi_amp


########################
# Covariate selection  #
########################

# spring precip, or growing season precip?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gHerb+0.02) ~ precip_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gHerb+0.02) ~ spr_precip_std, data = dat)
names(Cand.set) <- c("growing season", 
                     "spring")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# growing season precip

# time-integrated ndvi, or ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_ti_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_amp_std, data = dat)
names(Cand.set) <- c("time-integrated", 
                     "amplitude")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# indistinguishable; going with amplitude
  # because that's what was impt in bitterroot
  # and bc time integrated is very slightly correlated with elev

# ndvi duration, or ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_dur_std, data = dat.noNA)
Cand.set[[2]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_amp_std, data = dat.noNA)
names(Cand.set) <- c("ndvi_dur", 
                     "ndvi_amp")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# indistinguishable; going with amplitude

# quadratic or linear relationship with ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_amp_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gHerb+0.02) ~ ndvi_amp_std + I(ndvi_amp_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# linear

# quadratic or linear relationship with elev?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gHerb+0.02) ~ elev_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gHerb+0.02) ~ elev_std + I(elev_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# quadratic

####################
# Model selection  #
####################

# global model 
h.global <- lm(log10(dat$gHerb+0.02) ~ cc_std + elev_std + I(elev_std^2) +
                 landcov +radn_std + ndvi_amp_std + precip_std + slope_std, 
               data = dat)
  
# backwards stepwise AIC
step.aic <- step(h.global, direction = "both", k = 2)

# top model
h.top <- lm(log10(dat$gHerb + 0.02) ~ cc_std + elev_std + I(elev_std^2) + 
    landcov + radn_std + ndvi_amp_std + precip_std, data = dat)
summary(h.top)

#######################
# Predictive rasters  #
#######################

# top model with best predictive power
# using unstandardized covariates for prediction
h.pred <- lm(log10(dat$gHerb + 0.02) ~ cc + elev + I(elev^2) + 
    landcov + radn + ndvi_amp + precip, data = dat)

# prep rasters
# yes i realize it would be cooler if i'd done this programmatically
rast.14 <- list.files(path=paste(wd, "writtenrasters/covs2014/biomass-herb", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2014 rasters
stack.14 <- stack(rast.14)
names(stack.14) # rename rasters to match covariate names
names(stack.14) <- c("cc", "elev", "landcov", "ndvi_amp", "precip", "radn")
names(stack.14) # sanity check
rast.15 <- list.files(path=paste(wd, "writtenrasters/covs2015/biomass-herb", sep="/"), 
                      pattern="tif$", full.names=TRUE)  #read in 2015 rasters
stack.15 <- stack(rast.15)
names(stack.15) # rename these to match covariate names
names(stack.15) <- c("cc", "elev", "landcov", "ndvi_amp", "precip", "radn")
names(stack.15)


# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2014 rasters of gherb and StdError (indices 1 and 2, respectively)
gherb2014 <- predict(stack.14, h.pred, fun=predfun, index=1:2, progress="text")
names(gherb2014) <- c("gherb2014","StdErr14") 
gherb2014[["gherb2014"]] <- (10^gherb2014[["gherb2014"]])-0.02 #put back in g
plot(gherb2014[["gherb2014"]]) #gaze upon the beautiful rasters
plot(gherb2014[["StdErr14"]])

# predict 2015 rasters of gherb and StdError (indices 1 and 2, respectively)
gherb2015 <- predict(stack.15, h.pred, fun=predfun, index=1:2, progress="text")
names(gherb2015) <- c("gherb2015","StdErr15") 
gherb2015[["gherb2015"]] <- (10^gherb2015[["gherb2015"]])-0.02 #put back in g
plot(gherb2015[["gherb2015"]]) #gaze upon the beautiful rasters
plot(gherb2015[["StdErr15"]])

# export DE rasters
writeRaster(gherb2014, names(gherb2014), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
writeRaster(gherb2015, names(gherb2015), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
###################################################################

#####################
#### SHRUB MODEL ####
#####################

# read in DE data (if code not run in full from above)
dat <- read.csv("biomass-model-data.csv") %>%
  rename(gHerb = HerbaceousForageBiomass,
         gShrub = ForageShrubBiomass)

# Set factor levels for landcover type #
ref.lev <- dat %>%
  dplyr::select(c(landcov, class_name, gShrub)) %>%
  group_by(class_name, landcov) %>%
  summarise(AvggShrub = mean(gShrub)) %>%
  arrange(AvggShrub) %>% ## Order from least to most biomass
  ungroup() 
ref.lev # so reference level is landcover type with lowest abundance
dat$landcov <- factor(dat$landcov, levels = as.vector(ref.lev$landcov))
write.csv(ref.lev, file = "shrub-biomass-by-landcover.csv", row.names=F)

# look at distribution of response
hist(dat$gShrub, breaks = 100) #normal enough
hist(log10(dat$gShrub+0.02))


# check correlations and relationships
dat.cor <- dat %>%
  dplyr::select(cc_std, elev_std, landcov, radn_std, ndvi_amp_std, ndvi_dur_std, 
         ndvi_ti_std, precip_std, spr_precip_std, slope_std, gShrub)
source("../zMisc/pairs-panels.R")
pairs.panels(dat.cor) # bassing's prof's code to handle factors
# correlation coefficients > 0.6
  ## 0.91 spring precip & growing season precip
  ## 0.80 ndvi_ti & ndvi_amp


########################
# Covariate selection  #
########################

# spring precip, or growing season precip?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gShrub+0.02) ~ precip_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gShrub+0.02) ~ spr_precip_std, data = dat)
names(Cand.set) <- c("growing season", 
                     "spring")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# growing season

# time-integrated ndvi, or ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gShrub+0.02) ~ ndvi_ti_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gShrub+0.02) ~ ndvi_amp_std, data = dat)
names(Cand.set) <- c("time-integrated", 
                     "amplitude")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
#time integrated


# quadratic or linear relationship with ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gShrub+0.02) ~ ndvi_amp_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gShrub+0.02) ~ ndvi_amp_std + I(ndvi_amp_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# quadratic

# quadratic or linear relationship with elev?
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(dat$gShrub+0.02) ~ elev_std, data = dat)
Cand.set[[2]] <- lm(log10(dat$gShrub+0.02) ~ elev_std + I(elev_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# quadratic

####################
# Model selection  #
####################

# global model 
s.global <- lm(log10(dat$gShrub+0.02) ~ cc_std + elev_std + I(elev_std^2) +
                 landcov + radn_std + ndvi_amp_std + I(ndvi_amp_std^2) + 
                 precip_std + slope_std, data = dat)
  
# backwards stepwise AIC
step.aic <- step(s.global, direction = "both", k = 2)

# top model
s.top <- lm(log10(dat$gShrub + 0.02) ~ cc_std + elev_std + landcov + radn_std + 
    precip_std + slope_std, data = dat)
summary(s.top)

######################
# Predictive raster  #
######################

# top model with best predictive power
# using unstandardized covariates for prediction
s.pred <- lm(log10(dat$gShrub + 0.02) ~ cc + elev + landcov + radn + 
             precip + slope, data = dat)

# prep rasters
# yes i realize it would be cooler if i'd done this programmatically
rast.14 <- list.files(path=paste(wd, "writtenrasters/covs2014/biomass-shrub", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2014 rasters
stack.14 <- stack(rast.14)
names(stack.14) # rename rasters to match covariate names
names(stack.14) <- c("cc", "elev", "landcov", "precip", "radn", "slope")
names(stack.14) # sanity check
rast.15 <- list.files(path=paste(wd, "writtenrasters/covs2015/biomass-shrub", sep="/"), 
                      pattern="tif$", full.names=TRUE)  #read in 2015 rasters
stack.15 <- stack(rast.15)
names(stack.15) # rename these to match covariate names
names(stack.15) <- c("cc", "elev", "landcov", "precip", "radn", "slope")
names(stack.15)


# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2014 rasters of gshrub and StdError (indices 1 and 2, respectively)
gshrub2014 <- predict(stack.14, s.pred, fun=predfun, index=1:2, progress="text")
names(gshrub2014) <- c("gshrub2014","StdErr14") 
gshrub2014[["gshrub2014"]] <- (10^gshrub2014[["gshrub2014"]])-0.02 #put back in g
plot(gshrub2014[["gshrub2014"]]) #gaze upon the beautiful rasters
plot(gshrub2014[["StdErr14"]])

# predict 2015 rasters of gshrub and StdError (indices 1 and 2, respectively)
gshrub2015 <- predict(stack.15, s.pred, fun=predfun, index=1:2, progress="text")
names(gshrub2015) <- c("gshrub2015","StdErr15") 
gshrub2015[["gshrub2015"]] <- (10^gshrub2015[["gshrub2015"]])-0.02 #put back in g
plot(gshrub2015[["gshrub2015"]]) #gaze upon the beautiful rasters
plot(gshrub2015[["StdErr15"]])

# export DE rasters
writeRaster(gshrub2014, names(gshrub2014), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
writeRaster(gshrub2015, names(gshrub2015), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)


