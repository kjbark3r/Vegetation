##########################################################
#  ESTIMATING GRAMS OF DIGESTIBLE MATTER BY LIFE FORM    #
#      ACROSS NORTH SAPPHIRES IN SUMMER 2014 & 2015      #
#                   KRISTIN BARKER                       #
#                     DEC 2016                           #
##########################################################

#################
#### Setup   ####
#################

library(sp) #for kernel centroid estimate
library(adehabitatHR) #for kernel centroid estimate
library(raster)
library(rgdal) #Access geodatabases
library(rgeos)
library(ggplot2)
library(PerformanceAnalytics) #correlation graphs
library(AICcmodavg)
library(MuMIn) #dredge
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
rasterOptions(maxmemory = 1e+09)

################
#### Data   ####
################

# GDM per plot per lifeform, plus plot info (from gdm-model.r)
plot.data <- read.csv("gdm-plot-lifeform.csv", header=T) %>%
    transform(
    GDMherb = (GDMforb + GDMgrass), # add herbaceous GDM
    GDMtotal = (GDMforb + GDMgrass + GDMshrub) # and total GDM
    )

# plots shapefile (also from gdm-model.r)
plots <- readOGR(".", layer ='GDM_plots')
#all rasters for covariates
raster_data <- list.files(
  path=paste(wd, "writtenrasters", sep="/"), 
  pattern="tif$", 
  full.names=TRUE
  ) 
s <- stack(raster_data) 
names(s)

# cover class names
clsref <- data.frame(cover_class = c(1,2,3,4,5,6,7,8,9,10,11,12),
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

##############################
# Attribute data per plot ####
##############################

ext <- extract(s, plots) # extract from raster stack for each plot

dat <- cbind(plot.data, ext) %>%	# add extracted values to plot data
  within(Date <- as.Date(Date, "%Y-%m-%d")) %>% # format dates
  mutate(  #select correct years of covariates
    Year = as.numeric(format(Date, '%Y')),
    cover_class = ifelse(Year == 2014, esp6_14, esp6_15),
    ndvi_dur = ifelse(Year == 2014, ndvi_dur_14, ndvi_dur_15),
    ndvi_ti = ifelse(Year == 2014, ndvi_ti_14, ndvi_ti_15),
    sum_precip = ifelse(Year == 2014, precip_14, precip_15)
    ) %>%
  right_join(clsref, by = "cover_class") %>% #add cover class names
  within(cover_class <- as.factor(cover_class)) %>%
  #classify "NoData" NDVI durations as NAs
  mutate(ndvi_dur = ifelse(ndvi_dur<90|ndvi_dur>365, NA, ndvi_dur)) %>%
  mutate(  #standardize covariates
    cc_std = ((cc-(mean(cc)))/(sd(cc))),
    cti_std = ((cti-(mean(cti)))/(sd(cti))),
    elev_std = ((elev-(mean(elev)))/(sd(elev))),
    gsri_std = ((gsri-(mean(gsri)))/(sd(gsri))),
    ndvi_ti_std = ((ndvi_ti-(mean(ndvi_ti)))/(sd(ndvi_ti))),
    sum_precip_std = ((sum_precip-(mean(sum_precip)))/(sd(sum_precip))),
    slope_std = ((slope-(mean(slope)))/(sd(slope))),
    ndvi_dur_std = 
      ((ndvi_dur-(mean(ndvi_dur, na.rm=T)))/(sd(ndvi_dur, na.rm=T)))
    ) %>%
  select(-contains("_1")) %>%
  select(-hillshade) #remove extraneous columns

write.csv(dat, file="GDM-model-data.csv", row.names=F)


##############################################################
#### MODEL grams of digestible matter per vegetation type ####
##############################################################

dat <- read.csv("GDM-model-data.csv") # if not run in full above

############################
####    Forbs            ###
############################

dat.fb <- dat
dat.fb.no0 <- subset(dat.fb, GDMforb > 0)

# Factor levels for landcover type #
## Reference level is worst place they could possibly be
ref.fb <- dat %>%
  dplyr::select(c(cover_class, class_name, GDMforb)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDM = median(GDMforb)) %>%
  arrange(MedGDM) ## Order from least to most forb GDM 
dat.fb$cover_class <- factor(dat.fb$cover_class, 
                          levels = as.vector(ref.fb$cover_class))


# look at distribution of response, w/ and w/o 0s
par(mfrow=c(2,1))
hist(dat.fb$GDMforb, breaks=300)
hist(dat.fb.no0$GDMforb, breaks=300)

# dist has same basic shape with or without 0s
1-nrow(dat.fb.no0)/nrow(dat.fb)
#34% of data are 0s
summary(dat.fb$GDMforb)
var(dat.fb$GDMforb)

# check correlations
cor(dat.fb)
chart.Correlation(dat.fb)

# define global model
mod.global.fb <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                      family = Gamma(link = log), data = dat.fb.no0)
summary(mod.global.fb)
plot(mod.global.fb)
# cool, dunno if this means anything, but q-q looks wayyyy better now

###### MODEL SELECTION ###

# remove ndvi_dur NAs to determine whether important covariate
## (if not, can rerun models without those plots removed
dat.fb.no0.noNA <- subset(dat.fb.no0, !is.na(ndvi_dur))

# define global model
mod.global.fb <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                      family = Gamma(link = log), data = dat.fb.no0.noNA)

# backwards stepwise AIC
step.fb <- stepAIC(mod.global.fb, direction = "both")
# eff, all this did was remove ndvi_dur (convenient but not useful...)
# try it with bic to more heavily penalize addl params?
step.fb.bic <- step(mod.global.fb, direction = "both", k = log(652))

vif(mod.global.fb) # correlations with cover_class factor
# ok lets be honest; i have no idea how to interpret that


###############################
# top models with zeros in response

# with NA ndvi_dur's removed
dat.forb.sub <- filter(dat, !is.na(ndvi_dur_std))

mod.forb.global <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                       data = dat.fb)

# stepwise aic
step <- stepAIC(mod.forb.global, direction = "both")

# top model without ndvi_dur NAs removed (bc not a covariate)
summary(glm(GDMforb ~ cc_std + cti_std + elev_std + gsri_std + ndvi_ti_std + 
    cover_class, data = dat))


# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.forb.global, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
plot(dredgemod) # i have no idea how to interpret that either
(dredgeres <- subset(dredgemod, delta < 2))

dredgemod.avg <- model.avg(dredgeres, revised.var=TRUE)
summary(dredgemod.avg)
# hey cool, came up with same covariates as the stepwise aic

###############################
# top models without zeros in response
  ## if same, sweet, run with one model
  ## if different, consider zero-inflated

# new df
dat.forb.no0 <- subset(dat, GDMforb > 0) %>%
  filter(!is.na(ndvi_dur_std))

mod.forb.global <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                       data = dat.forb.no0)

# stepwise aic
step <- stepAIC(mod.forb.global, direction = "both")
# yes this is different; includes more covariates
# fml

#################### TO INCORPORATE ###########

forshrubs <- filter(!PlotVisit == "220.2015-08-03") # GDMshrub outlier