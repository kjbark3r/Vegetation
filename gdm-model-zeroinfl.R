##########################################################
#  ESTIMATING GRAMS OF DIGESTIBLE MATTER BY LIFE FORM    #
#      ACROSS NORTH SAPPHIRES IN SUMMER 2014 & 2015      #
#                   KRISTIN BARKER                       #
#                DEC 2016 - JAN 2017                     #
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
library(randomForest)
library(VSURF) #random forest vrbl slxn
library(boot) #cross validation
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
rasterOptions(maxmemory = 1e+09)

################################
#### Data Setup (run once)  ####
################################

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
  dplyr::select(-contains("_1")) %>%
  dplyr::select(-hillshade) #remove extraneous columns

write.csv(dat, file="GDM-model-data.csv", row.names=F)

######################################
#### Data setup (after run once)  ####
######################################

dat <- read.csv("GDM-model-data.csv") # only if not run in full above

# reduce number of landcover types by removing burn age info
  # because negbin model fails with too many covariates
  # and fire is not the main focus of this study
  # changes documented in new-landcover-types.xlsx
dat$cover_class[dat$cover_class==10] <- 1  
dat$cover_class[dat$cover_class==11] <- 1  
dat$cover_class[dat$cover_class==8] <- 2  
dat$cover_class[dat$cover_class==9] <- 2  
dat$cover_class[dat$cover_class==12] <- 2  

# reassign cover class names
clsref <- data.frame(cover_class = c(1,2,3,4,5,6,7),
                     class_name = c("Mesic Forest", #1
                                    "Dry Forest", #2 
                                    "Grass/Shrub/Open Woodland", #3
                                    "Dry Ag", #4
                                    "Valley Bottom Riparian", #5
                                    "Montane Riparian", #6
                                    "Irrigated Ag")) #7

# replace cover class names in dataframe
dat <- dat %>%
  select(-class_name) %>%
  right_join(clsref, by = "cover_class")


############################
####    Forb Model      ####
############################


# Data Setup #


dat.fb <- dat %>%
  mutate(CountForb = round(GDMforb)) %>% # round GDM (zeroinfl reqs count data)
  mutate(gForb = as.factor(ifelse(GDMforb > 0, 1, 0))) # presence/absence (for 0 model)

# Set factor levels for landcover type #
## (reference level is worst place they could possibly be)
ref.fb <- dat.fb %>%
  dplyr::select(c(cover_class, class_name, GDMforb)) %>%
  group_by(class_name, cover_class) %>%
  summarise(AvgGDM = mean(GDMforb)) %>%
  arrange(AvgGDM) %>% ## Order from least to most forb GDM 
  ungroup()
ref.fb
dat.fb$cover_class <- factor(dat.fb$cover_class, 
                          levels = as.vector(ref.fb$cover_class))

# remove outlier (id'd in troubleshooting R - huge residual)
dat.fb.noout <- dat.fb[-36,]

# create separate dataframe for Pr(0) model
dat.fb.no0 <- subset(dat.fb.noout, GDMforb > 0)

######################
# variable selection #
# GDM values > 0     #
######################

# global model without 0s in response - transformed, normally distributed
mod.global.fb.n <- lm(log10(GDMforb) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      data = dat.fb.no0)
summary(mod.global.fb.n)
plot(mod.global.fb.n)

# backwards stepwise AIC
step.fb <- stepAIC(mod.global.fb.n, direction = "both")

# backwards stepwise bic to more heavily penalize addl params
step.fb.bic <- step(mod.global.fb.n, direction = "both", k = log(652))

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.fb.n, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
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
importance(forest)

# vsurf random forest (for variable selection)
forestv <- VSURF(log10(GDMforb) ~ ., data = dat.fb.forest)
summary(forestv); names(forestv)
forestv$varselect.interp
forestv$varselect.pred
forestv$terms

# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std, 
                    data = dat.fb.no0)
Cand.set[[2]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0)
Cand.set[[3]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   sum_precip_std, data = dat.fb.no0)
Cand.set[[4]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std, data = dat.fb.no0)
Cand.set[[5]] <- lm(log10(GDMforb) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, data = dat.fb.no0)
names(Cand.set) <- c("cc+gsri+ndvi", 
                     "cc+gsri+ndvi+elev",
                     "cc+gsri+ndvi+precip",
                     "cc+gsri+ndvi+elev+precip",
                     "global")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# top models - can't distinguish bt +elev and +elev+precip

# +elev
summary(lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0))
#adj r-squared = 0.2403; resid se = 0.7295

# +elev+precip
summary(lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std, data = dat.fb.no0))
# adj r-2 = 0.2444; resid se = 0.7275

######################
# variable selection #
# GDM values = 0     #
######################

# global model
mod.global.fb.pr <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
summary(mod.global.fb.pr)

# backwards stepwise AIC
step.fb <- stepAIC(mod.global.fb.pr, direction = "both")

# backwards stepwise bic to more heavily penalize addl params, on normal dist
step.fb.bic <- step(mod.global.fb.pr, direction = "both", k = log(652))

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

# vsurf random forest (for variable selection)
forestv <- VSURF(gForb ~ ., data = dat.fb.forest)
summary(forestv); names(forestv)
forestv$terms
forestv$varselect.interp
forestv$varselect.pred

# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- glm(gForb ~ gsri_std, 
                      family = binomial(link = "logit"), data = dat.fb)
Cand.set[[2]] <- glm(gForb ~ cc_std + gsri_std, 
                     family = binomial(link = "logit"), data = dat.fb)
Cand.set[[3]] <- glm(gForb ~ gsri_std + cover_class, 
                     family = binomial(link = "logit"), 
                     data = dat.fb)
Cand.set[[4]] <- glm(gForb ~ elev_std + gsri_std, 
                     family = binomial(link = "logit"), 
                     data = dat.fb)
Cand.set[[5]] <- glm(gForb ~ gsri_std + sum_precip_std, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
Cand.set[[6]] <- glm(gForb ~ cc_std + elev_std + gsri_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
Cand.set[[7]] <- glm(gForb ~ cc_std + elev_std + gsri_std + cover_class
                     + sum_precip_std, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
Cand.set[[8]] <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
names(Cand.set) <- c("gsri", 
                     "gsri+cc",
                     "gsri+cover_class",
                     "gsri+elev",
                     "grsi+precip",
                     "grsi+cc+cover_class+elev",
                     "grsi+cc+cover_class+elev+precip",
                     "global")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# global model ftw

#######################
# Zero-inflated model #
#######################

# define zero-inflated model 
mod.fb <- zeroinfl(CountForb ~ cc_std + gsri_std + ndvi_ti_std + elev_std | 
                               cc_std + cti_std + elev_std + gsri_std + slope_std +
                               ndvi_ti_std + sum_precip_std + cover_class,
                         dist = "negbin", link = "logit",
                         data = dat.fb.noout)
summary(mod.fb)

# verify model handled overdispersion issue
resid.fb <- resid(mod.fb, type = "pearson")
dispersion.fb <- sum(resid.fb^2)/(nrow(dat.fb.noout) - 20) #20=df
dispersion.fb # close to 1 = good

# assess model fit #





############################
####    Grass Model    ####
############################


# Data Setup #

dat.gr <- dat %>%
  mutate(CountGrass = round(GDMgrass)) %>% # round GDM (zeroinfl reqs count data)
  mutate(gGrass = as.factor(ifelse(GDMgrass > 0, 1, 0))) # presence/absence (for 0 model)

# Set factor levels for landcover type #
## (reference level is worst place they could possibly be)
ref.gr <- dat.gr %>%
  dplyr::select(c(cover_class, class_name, GDMgrass)) %>%
  group_by(class_name, cover_class) %>%
  summarise(AvgGDM = mean(GDMgrass)) %>%
  arrange(AvgGDM) %>% ## Order from least to most grass GDM 
  ungroup()
ref.gr
dat.gr$cover_class <- factor(dat.gr$cover_class, 
                          levels = as.vector(ref.gr$cover_class))

# create separate dataframe for Pr(0) model
dat.gr.no0 <- subset(dat.gr, GDMgrass > 0)

######################
# variable selection #
# GDM values > 0     #
######################

# global model without 0s in response - transformed, normally distributed
mod.global.gr.n <- lm(log10(GDMgrass) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      data = dat.gr.no0)
summary(mod.global.gr.n)
plot(mod.global.gr.n)

# backwards stepwise AIC
step.gr <- stepAIC(mod.global.gr.n, direction = "both")

# backwards stepwise bic to more heavily penalize addl params
step.gr.bic <- step(mod.global.gr.n, direction = "both", k = log(652))

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.gr.n, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
dredgeres <- subset(dredgemod, delta < 2)
dredgemod.avg <- model.avg(dredgeres, revised.var=TRUE)
summary(dredgemod.avg)

# dredge with bic
dredgebic <- dredge(mod.global.gr.n, beta = "none", evaluate = TRUE, 
                    rank = "BIC")
dredgebicres <- subset(dredgebic, delta < 2)
dredgebic.avg <- model.avg(dredgebicres, revised.var=TRUE)
summary(dredgebic.avg)

# random forest
dat.gr.forest <- dat.gr.no0 %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_ti_std, sum_precip_std, GDMgrass)
forest <- randomForest(log10(GDMgrass) ~ ., data = dat.gr.forest)
print(forest)
importance(forest)

# vsurf random forest (for variable selection)
forestv <- VSURF(log10(GDMgrass) ~ ., data = dat.gr.forest)
summary(forestv); names(forestv)
forestv$varselect.interp
forestv$varselect.pred
forestv$terms

# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(GDMgrass) ~ elev_std, 
                    data = dat.gr.no0)
Cand.set[[2]] <- lm(log10(GDMgrass) ~ cc_std + elev_std, 
                    data = dat.gr.no0)
Cand.set[[3]] <- lm(log10(GDMgrass) ~ elev_std + ndvi_ti_std, 
                    data = dat.gr.no0)
Cand.set[[4]] <- lm(log10(GDMgrass) ~ cc_std + ndvi_ti_std + elev_std,
                    data = dat.gr.no0)
Cand.set[[5]] <- lm(log10(GDMgrass) ~ cover_class + ndvi_ti_std + elev_std, 
                    data = dat.gr.no0)
Cand.set[[6]] <- lm(log10(GDMgrass) ~ cc_std + ndvi_ti_std + elev_std
                    + cover_class, data = dat.gr.no0)
Cand.set[[7]] <- lm(log10(GDMgrass) ~ cc_std + ndvi_ti_std + elev_std
                    + sum_precip_std, data = dat.gr.no0)
Cand.set[[8]] <- lm(log10(GDMgrass) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, data = dat.gr.no0)
names(Cand.set) <- c("elev", 
                     "elev+cc",
                     "elev+ndvi_ti",
                     "elev+cc+ndvi_ti",
                     "elev+ndvi_ti+cover_class",
                     "elev+ndvi_ti+cover_class+cc",
                     "elev+ndvi_ti+cover_class+precip",
                     "global")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# elev+ndvi_ti+cover_class+cc

summary(lm(log10(GDMgrass) ~ cc_std + ndvi_ti_std + elev_std
                    + cover_class, data = dat.gr.no0))

######################
# variable selection #
# GDM values = 0     #
######################

# global model
mod.global.gr.pr <- glm(gGrass ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.gr)
summary(mod.global.gr.pr)

# backwards stepwise AIC
step.gr <- stepAIC(mod.global.gr.pr, direction = "both")

# backwards stepwise bic to more heavily penalize addl params, on normal dist
step.gr.bic <- step(mod.global.gr.pr, direction = "both", k = log(652))

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.gr.pr, beta = "none", evaluate = TRUE, 
                    rank = "AIC")
plot(dredgemod) # i have no idea how to interpret that either
dredgeres <- subset(dredgemod, delta < 2)
dredgemod.avg <- model.avg(dredgeres, revised.var=TRUE)
summary(dredgemod.avg)

# dredge with bic
dredgebic <- dredge(mod.global.gr.pr, beta = "none", evaluate = TRUE, 
                    rank = "BIC")
dredgebicres <- subset(dredgebic, delta < 2)
dredgebic.avg <- model.avg(dredgebicres, revised.var=TRUE)
summary(dredgebic.avg)

# random forest
dat.gr.forest <- dat.gr %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_ti_std, sum_precip_std, gGrass)
forest <- randomForest(gGrass ~ ., data = dat.gr.forest)
print(forest)
round(importance(forest), 2)

# vsurf random forest (for variable selection)
forestv <- VSURF(gGrass ~ ., data = dat.gr.forest)
summary(forestv); names(forestv)
forestv$terms
forestv$varselect.interp
forestv$varselect.pred

# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- glm(gGrass ~ cc_std, 
                      family = binomial(link = "logit"), data = dat.gr)
Cand.set[[2]] <- glm(gGrass ~ sum_precip_std, 
                     family = binomial(link = "logit"), data = dat.gr)
Cand.set[[3]] <- glm(gGrass ~ sum_precip_std + cc_std, 
                     family = binomial(link = "logit"), 
                     data = dat.gr)
Cand.set[[4]] <- glm(gGrass ~ sum_precip_std + cc_std + elev_std, 
                     family = binomial(link = "logit"), 
                     data = dat.gr)
Cand.set[[5]] <- glm(gGrass ~ sum_precip_std + cc_std + gsri_std, 
                      family = binomial(link = "logit"),
                        data = dat.gr)
Cand.set[[6]] <- glm(gGrass ~ sum_precip_std + cc_std + gsri_std + elev_std, 
                      family = binomial(link = "logit"),
                        data = dat.gr)
Cand.set[[7]] <- glm(gGrass ~ elev_std + sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.gr)
Cand.set[[8]] <- glm(gGrass ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.gr)
names(Cand.set) <- c("cc", 
                     "precip",
                     "precip+cc",
                     "precip+cc+elev",
                     "precip+cc+gsri",
                     "precip+cc+grsi+elev",
                     "elev+precip+cover_class",
                     "global")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# global model ftw

#######################
# Zero-inflated model #
#######################

# define zero-inflated model 
mod.gr <- zeroinfl(CountGrass ~ cc_std + ndvi_ti_std + elev_std + cover_class | 
                               cc_std + cti_std + elev_std + gsri_std + slope_std +
                               ndvi_ti_std + sum_precip_std + cover_class,
                         dist = "negbin", link = "logit",
                         data = dat.gr)
summary(mod.gr)

# verify model handled overdispersion issue
resid.gr <- resid(mod.gr, type = "pearson")
dispersion.gr <- sum(resid.gr^2)/(nrow(dat.gr) - 25) #25=df
dispersion.gr # niiiiice

# assess model fit #


