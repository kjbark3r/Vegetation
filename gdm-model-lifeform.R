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

# plot shapefile (also from gdm-model.r)
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

dat <- read.csv("GDM-model-data.csv")

#########################################
#### Set cover class reference levels ###
#########################################

# Order from least to most total GDM avaiable
  # so reference level is worst place they could possibly be
gdm.covcls <- dat %>%
  dplyr::select(c(cover_class, class_name, GDMtotal)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDM = median(GDMtotal)) %>%
  arrange(MedGDM))
#write.csv(gdm.covcls, file = "gdm-by-landcov.csv", row.names=F)
lev.covcls <- as.vector(gdm.covcls$cover_class)
dat$cover_class <- factor(dat$cover_class, levels = lev.covcls)

############################
####    Forbs            ###
############################

dat.forb <- dat %>%
  select(cc, cti, elev, gsri, slope, ndvi_dur, ndvi_ti,
         sum_precip, GDMforb)
no0 <- subset(dat.forb, GDMforb > 0)

# look at distribution of response
par(mfrow=c(2,1))
hist(dat.forb$GDMforb, breaks=300)
hist(no0$GDMforb, breaks=300)

sub.dat.forb <- filter(dat.forb, GDMforb < 50)
sub.no0 <- filter(no0, GDMforb < 50)

hist(sub.dat.forb$GDMforb)
hist(sub.no0$GDMforb)

# dist has same basic shape with or without 0s
1-nrow(no0)/nrow(dat.forb)
#34% of data are 0s
summary(dat.forb$GDMforb)
var(dat.forb$GDMforb)
summary(no0$GDMforb)
var(no0$GDMforb)
  # oh, duh, var'ce incs bc mean incs a ton
?dnbinom


# check correlations


cor(dat.forb)
chart.Correlation(dat.forb)

mod.forb.global <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                       data = dat)

vif(mod.forb.global) # correlations with cover_class factor
# ok lets be honest; i have no idea how to interpret that


###############################
# top models with zeros in response

# with NA ndvi_dur's removed
dat.forb.sub <- filter(dat, !is.na(ndvi_dur_std))

mod.forb.global <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                       data = dat.forb.sub)

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