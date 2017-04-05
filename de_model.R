##########################################################
# PREDICTING ELK FORAGE DIGESTIBLE ENERGY DURING SUMMER  #
#      ACROSS NORTH SAPPHIRES IN SUMMER 2014 & 2015      #
#                   KRISTIN BARKER                       #
#                DEC 2016 - JAN 2017                     #
##########################################################

# Code based on Jesse DeVoe's work on DE for Bitterroots #
#   Altered to use North Sapphire summer forage diet     #

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

# digestible energy (DE) per plot , plus plot info
## from foragequality_DE.R
plot.data <- read.csv("de-plot-summeronly.csv", header=T) 

# plots shapefile (from foragequality_DE.R)
plots <- readOGR(".", layer ='de-plot-summer')

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

ext <- extract(s, plots) # extract from raster stack for each plot

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

write.csv(dat, file="DE-model-data.csv", row.names=F)

#################################
#### DATA AND COVARIATE PREP ####
#################################

# read in DE data (if code not run in full from above)
dat <- read.csv("DE-model-data.csv") 

# Set factor levels for landcover type #
ref.lev <- dat %>%
  dplyr::select(c(landcov, class_name, DE)) %>%
  group_by(class_name, landcov) %>%
  summarise(MedianDE = median(DE)) %>%
  arrange(MedianDE) %>% ## Order from least to most forb GDM 
  ungroup() 
ref.lev # so reference level is landcover type with lowest DE
dat$landcov <- factor(dat$landcov, levels = as.vector(ref.lev$landcov))
write.csv(ref.lev, file = "de-by-landcover.csv", row.names=F)

# look at distribution of response
hist(dat$DE, breaks = 100) #normal enough

# check correlations and relationships
dat.cor <- dat %>%
  dplyr::select(cc_std, elev_std, landcov, radn_std, ndvi_amp_std, ndvi_dur_std, 
         ndvi_ti_std, precip_std, spr_precip_std, slope_std, DE)
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
Cand.set[[1]] <- lm(DE ~ precip_std, data = dat)
Cand.set[[2]] <- lm(DE ~ spr_precip_std, data = dat)
names(Cand.set) <- c("growing season", 
                     "spring")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# growing season precip

# time-integrated ndvi, or ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ ndvi_ti_std, data = dat)
Cand.set[[2]] <- lm(DE ~ ndvi_amp_std, data = dat)
names(Cand.set) <- c("time-integrated", 
                     "amplitude")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# indistinguishable; going with amplitude
  # because that's what was impt in bitterroot
  # and bc time integrated is very slightly correlated with elev

# ndvi duration, or ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ ndvi_dur_std, data = dat.noNA)
Cand.set[[2]] <- lm(DE ~ ndvi_amp_std, data = dat.noNA)
names(Cand.set) <- c("ndvi_dur", 
                     "ndvi_amp")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# ndvi amplitude

# quadratic or linear relationship with ndvi amplitude?
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ ndvi_amp_std, data = dat)
Cand.set[[2]] <- lm(DE ~ ndvi_amp_std + I(ndvi_amp_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# indistinguishable (so i'm going with linear)

# quadratic or linear relationship with elev?
Cand.set <- list( )
Cand.set[[1]] <- lm(lm(DE ~ elev_std, data = dat))
Cand.set[[2]] <- lm(DE ~ elev_std + I(elev_std^2), data = dat)
names(Cand.set) <- c("linear", 
                     "quadratic")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# quadratic

#########################
#### MODEL SELECTION ####
#########################

# global model 
m.global <- lm(DE ~ cc_std + elev_std + I(elev_std^2) + landcov +
               radn_std + ndvi_amp_std + precip_std + slope_std, 
               data = dat)
  
# backwards stepwise AIC
step.aic <- step(m.global, direction = "both", k = 2)

# selected models
summary(lm(DE ~ elev_std + landcov + radn_std + ndvi_amp_std + 
           precip_std + slope_std, data = dat))
summary(lm(DE ~ elev_std + I(elev_std^2) + landcov + radn_std + ndvi_amp_std + 
    precip_std + slope_std, data = dat))

# summary info for reporting
topmod <- lm(DE ~ elev_std + landcov + radn_std + ndvi_amp_std + 
           precip_std + slope_std, data = dat)
coefs <- data.frame(summary(topmod)$coefficients[,1:2])
coefs$Coeff <- rownames(coefs)
lcs <- ref.lev %>%
  mutate(Coeff = paste("landcov", landcov, sep="")) %>%
  select(c(class_name, Coeff))
coefs <- left_join(coefs, lcs, by = "Coeff") 
write.csv(coefs, file = "topmodelcoeffs.csv", row.names=F)

##############################
#### PREDICTIVE DE RASTER ####
##############################

# top model with best predictive power
# using unstandardized covariates for prediction
de.pred <- lm(DE ~ elev + landcov + radn + ndvi_amp + 
              precip + slope, data = dat)

# prep rasters
# yes i realize it would be cooler if i'd done this programmatically
rast.14 <- list.files(path=paste(wd, "writtenrasters/covs2014", sep="/"), 
                      pattern="tif$", full.names=TRUE) #read in 2014 rasters
stack.14 <- stack(rast.14)
names(stack.14) # rename rasters to match covariate names
names(stack.14) <- c("elev", "landcov", "ndvi_amp", "precip", "radn", "slope")
names(stack.14) # sanity check
rast.15 <- list.files(path=paste(wd, "writtenrasters/covs2015", sep="/"), 
                      pattern="tif$", full.names=TRUE)  #read in 2015 rasters
stack.15 <- stack(rast.15)
names(stack.15) # rename these to match covariate names
names(stack.15) <- c("elev", "landcov", "ndvi_amp", "precip", "radn", "slope")
names(stack.15)

# function to get both response and SE predictions
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE) 
  cbind(p=as.vector(v$fit), se=as.vector(v$se.fit))
}

# predict 2014 rasters of DE and StdError (indices 1 and 2, respectively)
de2014 <- predict(stack.14, de.pred, fun=predfun, index=1:2, progress="text")
names(de2014) <- c("DE2014","StdErr14") 
#plot(de2014) # plot both
plot(de2014[["DE2014"]]) # plot one at a time
#plot(de2014[["StdErr14"]])

# predict 2015 rasters of DE and StdError (indices 1 and 2, respectively)
de2015 <- predict(stack.15, de.pred, fun=predfun, index=1:2, progress="text")
names(de2015) <- c("DE2015","StdErr15") 
#plot(de2015) # plot both
plot(de2015[["DE2015"]]) # plot one at a time
#plot(de2015[["StdErr15"]])

# export DE rasters
writeRaster(de2014, names(de2014), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)
writeRaster(de2015, names(de2015), bylayer = TRUE, 
            format = "GTiff", overwrite = TRUE)


##########################
### DELETED/MISC CODE ####
##########################

########################
# creating barplot of nutrition per landcover
library(ggplot2)

plotdat <- ref.lev %>%
  rename(Landcover = class_name) %>%
  transform(Landcover = ifelse(Landcover == "Irrigated Ag",
                             "Irrigated Agricultural Land", 
                             ifelse(Landcover == "Rx Dry Forest Burn 0-5",
                                    "Dry Forest - recent prescribed burn",
                             ifelse(Landcover == "Dry Forest Burn 0-5",
                                    "Dry Forest - recent wildfire",
                             ifelse(Landcover == "Dry Ag",
                                    "Non-irrigated Agricultural Land",
                             ifelse(Landcover == "Mesic Forest Burn 0-5",
                                    "Wet Forest - recent wildfire",
                             ifelse(Landcover == "Mesic Forest Burn 6-15",
                                    "Wet Forest - mid-successional",
                             ifelse(Landcover == "Dry Forest Burn 6-15",
                                   "Dry Forest - mid-successional",
                             ifelse(Landcover == "Mesic Forest (Burn >15)",
                                    "Wet forest - late successional",
                             ifelse(Landcover == "Dry Forest (Burn >15)",
                                    "Dry Forest - late successional",
                                    paste(Landcover))))))))))) %>%
  transform(Landcover = factor(Landcover,
              levels = (Landcover[order(MedianDE)])))

ggplot(data=plotdat, aes(x=Landcover, y=MedianDE)) +
  geom_bar(stat="identity", colour="black", 
           fill="darkgreen", width=.7) +
  geom_hline(yintercept=c(2.9, 2.75, 2.4), 
             colour = "coral4", size = 1) +
  coord_flip() +
  labs(x = "", y = expression(paste(
            "Forage Quality (kcal / ", 
             m^2, ")", sep="")))



########################
# i also tried reducing number of landcover types
# by removing fire history info 
# because fire is not the main focus of this study
# but the model was much worse, so i kept fire info in

# reassign cover classes
dat.lc <- read.csv("DE-model-data.csv") 
dat.lc$landcov[dat.lc$landcov==10] <- 1  
dat.lc$landcov[dat.lc$landcov==11] <- 1  
dat.lc$landcov[dat.lc$landcov==8] <- 2  
dat.lc$landcov[dat.lc$landcov==9] <- 2  
dat.lc$landcov[dat.lc$landcov==12] <- 2  

# reassign cover class names
clsref <- data.frame(landcov = c(1,2,3,4,5,6,7),
                     class_name = c("Mesic Forest", #1
                                    "Dry Forest", #2 
                                    "Grass/Shrub/Open Woodland", #3
                                    "Dry Ag", #4
                                    "Valley Bottom Riparian", #5
                                    "Montane Riparian", #6
                                    "Irrigated Ag")) #7
dat.lc <- dat.lc %>%
  select(-class_name) %>%
  right_join(clsref, by = "landcov")

# Set factor levels for landcover type #
ref.lev.lc <- dat.lc %>%
  dplyr::select(c(landcov, class_name, DE)) %>%
  group_by(class_name, landcov) %>%
  summarise(AvgDE = mean(DE)) %>%
  arrange(AvgDE) %>% ## Order from least to most forb GDM 
  ungroup() 
ref.lev.lc # so reference level is landcover type with lowest DE
dat.lc$landcov <- factor(dat.lc$landcov, levels = as.vector(ref.lev.lc$landcov))

# comparing models with and without fire history included
Cand.set <- list( )
Cand.set[[1]] <- lm(DE ~ elev_std + landcov + radn_std + 
                    ndvi_amp_std + precip_std + slope_std, 
                    data = dat)
Cand.set[[2]] <- lm(DE ~ elev_std + landcov + radn_std + 
                    ndvi_amp_std + precip_std + slope_std, 
                    data = dat.lc)
names(Cand.set) <- c("incl fire hist", 
                     "excl fire hist")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# best supported model is the one that includes fire history info