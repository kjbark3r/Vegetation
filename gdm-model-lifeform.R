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
library(pscl) # zero-inflated model
library(dplyr) 
library(randomForest)
library(VSURF) #random forest vrbl slxn

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

dat.fb <- dat %>%
  mutate(gForb = as.factor(ifelse(GDMforb > 0, 1, 0))) # presence/absence
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


##                  ##
## looking at stuff ##
##                  ##

# distribution of response, w/ and w/o 0s
par(mfrow=c(2,1))
hist(dat.fb$GDMforb, breaks=300)
hist(dat.fb.no0$GDMforb, breaks=300) 

# probability density of non-zero response (for funzies)
par(mfrow=c(1,1))
hist(log10(dat.fb.no0$GDMforb), freq=FALSE) 

# dist has same basic shape with or without 0s
1-nrow(dat.fb.no0)/nrow(dat.fb)
#34% of data are 0s
summary(dat.fb$GDMforb)
var(dat.fb$GDMforb)

# correlations
dat.fb.cor <- dat.fb %>%
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_dur_std, ndvi_ti_std, sum_precip_std, GDMforb)
pairs.panels(dat.fb.cor) # bassing's prof's code to handle factors
chart.Correlation(dat.fb.cor) # verifying prof's numbers


##                                    ##
## prelim attempts at model selection ##
##                                    ##

####                                 #
# NON-ZEROS IN REPONSE               #
# these will be conditional on Pr(1) #
####                                 #

# global model without 0s in response - gamma distributed, not transformed
mod.global.fb.g <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = Gamma(link = log), data = dat.fb.no0)
summary(mod.global.fb.g)
plot(mod.global.fb.g)

# global model without 0s in response - transformed, normally distributed
mod.global.fb.n <- glm(log10(GDMforb) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      data = dat.fb.no0)
summary(mod.global.fb.n)
plot(mod.global.fb.n)

# backwards stepwise AIC on each of the above
step.fb <- stepAIC(mod.global.fb.g, direction = "both", steps = 1000000)
# algorithm fails to converge (despite adding steps)
step.fb <- stepAIC(mod.global.fb.n, direction = "both")

# backwards stepwise bic to more heavily penalize addl params, on normal dist
step.fb.bic <- step(mod.global.fb.n, direction = "both", k = log(652))

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.fb.n, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
plot(dredgemod) # i have no idea how to interpret that either
dredgeres <- subset(dredgemod, delta < 2)
dredgemod.avg <- model.avg(dredgeres, revised.var=TRUE)
summary(dredgemod.avg)

# dredge with bic
dredgebic <- dredge(mod.global.fb.n, beta = "none", evaluate = TRUE, 
                    rank = "BIC")
dredgebicres <- subset(dredgebic, delta < 2)
dredgebic.avg <- model.avg(dredgebicres, revised.var=TRUE)
summary(dredgebic.avg)

# random forest
dat.fb.forest <- dat.fb.no0 %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_ti_std, sum_precip_std, GDMforb)
forest <- randomForest(log10(GDMforb) ~ ., data = dat.fb.forest)
print(forest)
round(importance(forest), 2)

# vsurf random forest (for variable selection)
forestv <- VSURF(log10(GDMforb) ~ ., data = dat.fb.forest)
summary(forestv); names(forestv)
forestv$varselect.interp
forestv$varselect.pred
forestv$terms

# methods agree on elev, gsri, and ndvi_ti as impt covariates
# some also add sum_precip; check whether it improves predictive power
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(GDMforb) ~ cc_std + elev_std + gsri_std + 
                   ndvi_ti_std, data = dat.fb.no0)
Cand.set[[2]] <- lm(log10(GDMforb) ~ cc_std + elev_std + gsri_std + 
                   ndvi_ti_std + sum_precip_std, data = dat.fb.no0)
names(Cand.set) <- c("lognormal-noprecip", 
                     "lognormal-precip")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# a little. <2AIC points though. Average models, or...?
summary(lm(log10(GDMforb) ~ cc_std + elev_std + gsri_std + 
                   ndvi_ti_std + sum_precip_std, data = dat.fb.no0))
# adj r-2 = 0.2423

# and doing the same with the gamma models, for funzies
Cand.set[[1]] <- glm(GDMforb ~ cc_std + elev_std + gsri_std + 
                      ndvi_ti_std, family = Gamma(link = log), 
                    data = dat.fb.no0)
Cand.set[[2]] <- glm(GDMforb ~ cc_std + elev_std + gsri_std + 
                      ndvi_ti_std+ sum_precip_std, 
                     family = Gamma(link = log), data = dat.fb.no0)
names(Cand.set) <- c("gamma-noprecip", 
                     "gamma-precip")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# playing with plots
# trying to figure out whether i can select bt the above
par(mfrow=c(2,2))
plot(mod.top.fb.n)
plot(mod.top.fb.g)
# i think the lognormal reggression looks best

####                              #
# ZEROS IN RESPONSE               #
####                              #
 
# global model
mod.global.fb.pr <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial,
                        data = dat.fb)
summary(mod.global.fb.pr)
plot(mod.global.fb.pr)

# backwards stepwise AIC
step.fb <- stepAIC(mod.global.fb.pr, direction = "both")

# backwards stepwise bic to more heavily penalize addl params, on normal dist
step.fb.bic <- step(mod.global.fb.pr, direction = "both", k = log(652))
# cc, elev, gsri, slope, maybe ndvi_ti

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.fb.pr, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
plot(dredgemod) # i have no idea how to interpret that either
dredgeres <- subset(dredgemod, delta < 2)
dredgemod.avg <- model.avg(dredgeres, revised.var=TRUE)
summary(dredgemod.avg)
# same cov's as above (cc, elev, gsri, slope, maybe ndvi_ti)

# dredge with bic
dredgebic <- dredge(mod.global.fb.pr, beta = "none", evaluate = TRUE, 
                    rank = "BIC")
dredgebicres <- subset(dredgebic, delta < 2)
dredgebic.avg <- model.avg(dredgebicres, revised.var=TRUE)
summary(dredgebic.avg)

# random forest
dat.fb.forest <- dat.fb %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_ti_std, sum_precip_std, gForb)
forest <- randomForest(gForb ~ ., data = dat.fb.forest)
print(forest)
round(importance(forest), 2)
# shit, this one also likes precip

# vsurf random forest (for variable selection)
forestv <- VSURF(gForb ~ ., data = dat.fb.forest)
summary(forestv); names(forestv)
forestv$varselect.interp
forestv$varselect.pred
forestv$terms
#################### TO INCORPORATE ###########

forshrubs <- filter(!PlotVisit == "220.2015-08-03") # GDMshrub outlier