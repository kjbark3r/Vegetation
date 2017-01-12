#############################################
#  MISC CODE FROM CREATING PREDICTIVE MODEL #
#          OF GDM BY LIFEFORM               #
#             KRISTIN BARKER                #
#           DEC 2016  / JAN 2017            #
#############################################

################################################
## zero-inflated models on rounded GDM vals ####

####    #
# Forb  #
####    #

# round GDM vals to nearest integer
test.fb <- dat.fb %>%
  mutate(CountForb = round(GDMforb))
hist(test.fb$CountForb, breaks = 300)


#####
# global vs selected
####

# global model 
mod.global.fb <- zeroinfl(CountForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         data = test.fb)
summary(mod.global.fb)

# models from my model selection
mod.selected.fb <- zeroinfl(CountForb ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std | cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         data = test.fb)
summary(mod.selected.fb)

# comparisons #

#overdispersion
resid.glob <- resid(mod.global.fb, type = "pearson")
dispersion.glob <- sum(resid.glob^2)/(nrow(test.fb) - 38) #38=#DF
dispersion.glob
#5.28, overdispersed (exactly 1 would be nondispersed)
resid.selected <- resid(mod.selected.fb, type = "pearson")
dispersion.selected <- sum(resid.glob^2)/(nrow(test.fb) - 25) #25=DF
dispersion.selected
#better but not by much

#attempting to assess model fit

#likelihood ratio test
lik.ratio <- 2 * (logLik(mod.global.fb) - logLik(mod.selected.fb))
pval <- 1 - pchisq(abs(lik.ratio), 1) #1=df (copied from bk, no idea why =1)
#trying it backwards
lik.ratio <- 2 * (logLik(mod.selected.fb) - logLik(mod.global.fb))
#same number, just negative rather than positive, duh
pval <- 1 - pchisq(abs(lik.ratio), (36-25))
#still 0. so i think there's a diff but i don't know which way

#checking whether need to remove outlier, a la zuur et al
#by looking at residuals ~ fitted values
#shit for global model
currentmodel <- mod.global.fb
#currentmodel <- mod.selected.fb
Gammas.logistic <- coef(currentmodel, model = "zero")
X.logistic <- model.matrix(currentmodel, model = "zero")
eta.logistic <- X.logistic %*% Gammas.logistic
p <- exp(eta.logistic) / (1 + exp(eta.logistic))
Betas.log <- coef(currentmodel, model = "count")
X.log <- model.matrix(currentmodel, model = "count")
eta.log <- X.log %*% Betas.log
mu <- exp(eta.log)
ExpY <- mu * (1-p)
VarY <- (1 -p) * (mu + p * mu^2)
En <- resid(currentmodel, type = "pearson")
plot(x = ExpY, y = En, xlab = "Fitted Vals",
     ylab = "Pearson residuals")
identify(x = ExpY, y = En) #outlier w huge resid - row 36
dat.fb[36,]

#do same for grasses
currentmodel <- mod.global.fb
#currentmodel <- mod.selected.fb
Gammas.logistic <- coef(currentmodel, model = "zero")
X.logistic <- model.matrix(currentmodel, model = "zero")
eta.logistic <- X.logistic %*% Gammas.logistic
p <- exp(eta.logistic) / (1 + exp(eta.logistic))
Betas.log <- coef(currentmodel, model = "count")
X.log <- model.matrix(currentmodel, model = "count")
eta.log <- X.log %*% Betas.log
mu <- exp(eta.log)
ExpY <- mu * (1-p)
VarY <- (1 -p) * (mu + p * mu^2)
En <- resid(currentmodel, type = "pearson")
plot(x = ExpY, y = En, xlab = "Fitted Vals",
     ylab = "Pearson residuals")
identify(x = ExpY, y = En) #outlier w huge resid - row 36
dat.fb[36,]

#rerun models without outlier
test.fb.noout <- test.fb[-36,] #remove outliers

# global model 
mod.global.fb <- zeroinfl(CountForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         data = test.fb.noout)
summary(mod.global.fb)

# models from my model selection
mod.selected.fb <- zeroinfl(CountForb ~ cc_std + gsri_std + ndvi_ti_std +
                         elev_std + sum_precip_std | cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         data = test.fb.noout)
summary(mod.selected.fb)

#overdispersion
resid.glob <- resid(mod.global.fb, type = "pearson")
dispersion.glob <- sum(resid.glob^2)/(nrow(test.fb) - 38) #38=#DF
dispersion.glob
#4.5, better but still bad
resid.selected <- resid(mod.selected.fb, type = "pearson")
dispersion.selected <- sum(resid.glob^2)/(nrow(test.fb) - 25) #25=DF
dispersion.selected
#4.43, ditto above
#so i think i should try negbin model

#####
# negbin zeroinfl (negbin better with overdisperson)
####

# global model 
mod.global.fb <- zeroinfl(CountForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         dist = "negbin", link = "logit",
                         data = test.fb.noout)
summary(mod.global.fb)

# models from my model selection
mod.selected.fb <- zeroinfl(CountForb ~ cc_std + gsri_std + ndvi_ti_std +
                         elev_std + sum_precip_std | cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         dist = "negbin", link = "logit",
                         data = test.fb.noout)
summary(mod.selected.fb)
#can't do negbin when cover class is included.
#try reducing number of landcover classifications (rm fire)
#ok done (see below)

#overdispersion
resid.glob <- resid(mod.global.fb, type = "pearson")
dispersion.glob <- sum(resid.glob^2)/(nrow(test.fb) - 38) #38=#DF
dispersion.glob
#1.29, better but still bad
resid.selected <- resid(mod.selected.fb, type = "pearson")
dispersion.selected <- sum(resid.glob^2)/(nrow(test.fb) - 25) #25=DF
dispersion.selected
#1.26, very slightly better
#also i am freaking stoked (/shocked) that i managed to make this work

#ok, so now how do i assess the fit of this model???

# compare to null model (intercept-only)
mod.null.fb <- update(mod.selected.fb, .~1)
summary(mod.null.fb)

# chi-square
pchisq(2 * (logLik(mod.selected.fb) - logLik(mod.null.fb)), df = 3, lower.tail = F)
# super small, so significantly diff than null i think(?)

# vuong
vuong(mod.selected.fb, mod.null.fb)
# all results indicate zeroinfl model sig > null model

# oops wait maybe i shouldn't have used standardized coefficients
m.fb <- zeroinfl(CountForb ~ cc + gsri + ndvi_ti +
                         elev + sum_precip | cc + cti + elev + 
                         gsri + slope + ndvi_ti + 
                         sum_precip + cover_class,
                         dist = "negbin", link = "logit",
                         data = dat.fb.noout)
summary(mod.selected.fb)

##################################
# using zero-infl model to predict forb gdm across rasters ####
# can hopefully compare these vals to measured vals to assess fit (?)

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
pred2014 <- predict(s.14, mod.selected.fb, fun=predfun, index=1:2, progress="text") # predfun returns two variables (response and se), so need to include index=1:2
names(pred2014) <- c("gGDM2014","se14") 
plot(pred2014) # plot both
plot(pred2014[["gGDM2014"]]) # plot one at a time
plot(pred2014[["se14"]])

# run model on unstandardized coefficients
mod.unstd.fb <- zeroinfl(CountForb ~ cc + gsri + ndvi_ti + elev + sum_precip | 
                                     cc + cti + elev + gsri + slope + ndvi_ti + 
                                     sum_precip + cover_class,
                         dist = "negbin", link = "logit",
                         data = dat.fb.noout)
summary(mod.unstd.fb)
# issue: NAs in elev and gsri
# standardizing covariates is the only fix i can find
# and i don't think it makes sense to standardize my rasters
# or use poisson

mod.unstd.fb <- zeroinfl(CountForb ~ cc + gsri + ndvi_ti + elev + sum_precip | 
                                     cc + cti + elev + gsri + slope + ndvi_ti + 
                                     sum_precip + cover_class,
                         data = dat.fb.noout)
# error (greek to me...)
#######################
## reducing number of landcover types ####

# first just change in forbs and see if that helps
# using test data

# reassign fire-related cover classes
test.fb <- dat
test.fb$cover_class[test.fb$cover_class==10] <- 1  
test.fb$cover_class[test.fb$cover_class==11] <- 1  
test.fb$cover_class[test.fb$cover_class==8] <- 2  
test.fb$cover_class[test.fb$cover_class==9] <- 2  
test.fb$cover_class[test.fb$cover_class==12] <- 2  
unique(test.fb$cover_class)

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
test.fb <- test.fb %>%
  select(-class_name) %>%
  right_join(clsref, by = "cover_class")
unique(test.fb$class_name)

# re-figure reference levels
test.ref.fb <- test.fb %>%
  dplyr::select(c(cover_class, class_name, GDMforb)) %>%
  group_by(class_name, cover_class) %>%
  summarise(AvgGDM = mean(GDMforb)) %>%
  arrange(AvgGDM) %>% ## Order from least to most forb GDM 
  ungroup()
test.fb$cover_class <- factor(test.fb$cover_class, 
                          levels = as.vector(ref.fb$cover_class))
test.ref.fb

# rerun models 
# renaming test data to regular data so can straight copy-paste
# need to rerun real code to get orig info back
dat.fb <- test.fb
dat.fb <- dat.fb %>%
  mutate(gForb = as.factor(ifelse(GDMforb > 0, 1, 0))) # presence/absence
dat.fb.no0 <- subset(dat.fb, GDMforb > 0)

################################################
## trying automated method of model selection/averaging ####
# uses MuMIn

# define models
mod1 <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std, 
                    data = dat.fb.no0)
mod2 <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0)
mod3 <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   sum_precip_std, data = dat.fb.no0)
mod4 <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std, data = dat.fb.no0)
mod5 <- lm(log10(GDMforb) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, data = dat.fb.no0)
out.put <- model.sel(mod1, mod2, mod3, mod4, mod5)
out.put 
sub <- subset(out.put, delta <2) # good, same results
fb.avgmod <- model.avg(sub, revised.var = TRUE)
fb.avgmod

# model average using that aicpackage instead ####
Cand.set2 <- list( )
Cand.set2[[1]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0)
Cand.set2[[2]] <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std, data = dat.fb.no0)
names(Cand.set2) <- c("cc+gsri+ndvi+elev",
                     "cc+gsri+ndvi+elev+precip")
cand.names <- c("cc+gsri+ndvi+elev",
                     "cc+gsri+ndvi+elev+precip")
modavg(Cand.set2, parm = "cc_std", modnames = cand.names)
# ok, i guess

###########################
## just adding Year out of curiosity ##
# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
Cand.set[[2]] <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class + Year, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
names(Cand.set) <- c("global", "global+year")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# meh, no obvious difference here

# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- lm(log10(GDMgrass) ~ cc_std + elev_std + cover_class,
                    data = dat.gr.no0)
Cand.set[[2]] <- lm(log10(GDMgrass) ~ cc_std + elev_std + cover_class
                    + Year,
                    data = dat.gr.no0)
names(Cand.set) <- c("topmod", "topmod+Yr")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# whew, ok

#############################
## playing with cross validation ####

# for forb(0) mod with fewer covariates
# compare top selected covariate options
Cand.set <- list( )
Cand.set[[1]] <- glm(gForb ~ cc_std + elev_std + gsri_std, 
                      family = binomial(link = "logit"), data = dat.fb)
Cand.set[[2]] <- glm(gForb ~ cc_std + elev_std + gsri_std +
                     slope_std, family = binomial(link = "logit"), data = dat.fb)
Cand.set[[3]] <- glm(gForb ~ cc_std + elev_std + gsri_std +
                     ndvi_ti_std, family = binomial(link = "logit"), 
                     data = dat.fb)
Cand.set[[4]] <- glm(gForb ~ cc_std + elev_std + gsri_std +
                     slope_std + ndvi_ti_std, family = binomial(link = "logit"), 
                     data = dat.fb)
Cand.set[[5]] <- glm(gForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      family = binomial(link = "logit"),
                        data = dat.fb)
names(Cand.set) <- c("cc+elev+gsri", 
                     "cc+elev+gsri+slope",
                     "cc+elev+gsri+ndvi",
                     "cc+elev+gsri+slope+ndvi",
                     "global")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
# global model way outperforms the others... hmm...

glm.fit.test <- glm(gForb ~ cc_std + elev_std + gsri_std, 
                      family = binomial(link = "logit"), data = dat.fb)
cv.err.test <- cv.glm(dat.fb, glm.fit.test, K = 10)
cv.err.test$delta
# higher (so less predictive) than global

## forb(>0) - choosing between the two indistinguishable models
# +elev
glm.fit.testa <- glm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0)
cv.err.testa <- cv.glm(dat.fb.no0, glm.fit.testa, K = 10)
cv.err.testa$delta

# +elev+precip
glm.fit.testb <- glm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std + sum_precip_std, data = dat.fb.no0, K = 10)
cv.err.testb <- cv.glm(dat.fb.no0, glm.fit.testb)
cv.err.testb$delta
# slightly lower - but still pretty terrible (0.538 vs 0.540)

#######################
# using stepAIC to check for interactions ####

mod.global.gr.n <- lm(log10(GDMgrass) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class, 
                      data = dat.gr.no0)
step.gr <- stepAIC(mod.global.gr.n, scope = list(
  upper = ~cc_std*cti_std*elev_std*gsri_std*slope_std*ndvi_ti_std*sum_precip_std*cover_class,
  lower = ~1))
summary(lm(log10(GDMgrass) ~ cc_std + cti_std + elev_std + gsri_std + slope_std + 
    ndvi_ti_std + cover_class + elev_std:cover_class + gsri_std:cover_class + 
    cti_std:slope_std + cc_std:elev_std + elev_std:slope_std + 
    cc_std:ndvi_ti_std + elev_std:ndvi_ti_std + gsri_std:ndvi_ti_std + 
    gsri_std:slope_std + cc_std:elev_std:ndvi_ti_std, data = dat.gr.no0))
#ahahahahahahahaha now i fully understand why people think this is bullshit
gr.overfit <- glm(log10(GDMgrass) ~ cc_std + cti_std + elev_std + gsri_std + slope_std + 
    ndvi_ti_std + cover_class + elev_std:cover_class + gsri_std:cover_class + 
    cti_std:slope_std + cc_std:elev_std + elev_std:slope_std + 
    cc_std:ndvi_ti_std + elev_std:ndvi_ti_std + gsri_std:ndvi_ti_std + 
    gsri_std:slope_std + cc_std:elev_std:ndvi_ti_std, data = dat.gr.no0)
cv.overfit <- cv.glm(dat.gr.no0, gr.overfit)
cv.overfit$delta
#although it does perform well. . . . .

#################
## pre-made variance plots
# from Faraway

par(mfrow=c(3,3))

#strong nonconstant variance
for(i in 1:9) plot(1:50, (1:50)*rnorm(50))

#weak nonconstant variance
for(i in 1:9) plot(1:50, sqrt((1:50))*rnorm(50))

#my variance
par(mfrow=c(2,1))
f1 <- lm(log10(GDMforb) ~ cc_std + gsri_std + ndvi_ti_std +
                   elev_std, data = dat.fb.no0)
g1 <- lm(log10(GDMgrass) ~ cc_std + elev_std + ndvi_ti_std +
                   cover_class, data = dat.gr.no0)
plot(fitted(f1), abs(residuals(f1)),
     xlab = "Fitted", ylab = "|Resid|",
     main = "Forbs")
plot(fitted(g1), abs(residuals(g1)),
     xlab = "Fitted", ylab = "|Resid|",
     main = "Grass")
# hm, this doesn't seem great, all clumpy at the bottom
# for the record, i checked out residuals on untransformed response
  # and they were out of this world terrible

#######################
## number of 0s in biomass ####

a <- read.csv("biomass-plot.csv")
a.f0 <- subset(a, ForageForbBiomass == 0)
nrow(a.f0)/nrow(a)


################
## CUT CODE ####
################

#########################################
#### Set cover class reference levels (global) ###
#########################################

# Order from least to most total GDM avaiable
  # so reference level is worst place they could possibly be
gdm.covcls <- dat %>%
  dplyr::select(c(cover_class, class_name, GDMtotal)) %>%
  group_by(class_name, cover_class) %>%
  summarise(MedGDM = median(GDMtotal)) %>%
  arrange(MedGDM)
#write.csv(gdm.covcls, file = "gdm-by-landcov.csv", row.names=F)
lev.covcls <- as.vector(gdm.covcls$cover_class)
dat$cover_class <- factor(dat$cover_class, levels = lev.covcls)

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

###########################################
## trying different distributions, regressions, etc ###
##########################################

#poisson distn cuz some stackecxhange dude said i could
# even tho it's continuous and not count data
mod.global.fb <- glm(GDMforb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                      family = poisson(link = log), data = dat.fb.no0)
summary(mod.global.fb)
plot(mod.global.fb)
# qq sucks on this one

# using bic rather than aic to penalize addl params more
step.fb.bic <- step(mod.global.fb, direction = "both", k = log(652))
top.bic.fb <- glm(GDMforb ~ cc_std + elev_std + gsri_std + ndvi_ti_std,
                  family = Gamma(link = log), data = dat.fb.no0)
plot(top.boc.fb)

#normal distn 
mod.global.fb <- glm(log10(GDMforb) ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_dur_std + 
                         ndvi_ti_std + sum_precip_std + cover_class, 
                       data = dat.fb.no0)
summary(mod.global.fb)
plot(mod.global.fb)
# qq sucks... slightly less??

####
# MACHINE LEARNING ATTEMPTS ##
######

# random forest
library(randomForest)

# no 0s, no NAs
dat.fb.forest <- dat.fb.no0.noNA %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_dur_std, ndvi_ti_std, sum_precip_std, GDMforb)
forest <- randomForest(log10(GDMforb) ~ ., data = dat.fb.forest)
print(forest)
# omfg this apparently explains 24.52% of var
# tho i don't understand how, or what the model even is...
# let's see how it does with the 0s included
dat.fb.forest <- dat.fb %>% 
  subset(!is.na(ndvi_dur)) %>%
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_dur_std, ndvi_ti_std, sum_precip_std, GDMforb)
forest <- randomForest(log10(GDMforb) ~ ., data = dat.fb.forest)
print(forest)
# oh right, can't do that. because log(0)... duh.
# ok let's see about using the 24.52% model for non-zero
# then modeling 0s separately

# no 0s, no NAs
dat.fb.forest <- dat.fb.no0.noNA %>% 
  select(cover_class, cc_std, cti_std, elev_std, gsri_std, slope_std,
         ndvi_dur_std, ndvi_ti_std, sum_precip_std, GDMforb)
forest <- randomForest(log10(GDMforb) ~ ., data = dat.fb.forest)
print(forest)
## Look at variable importance:
round(importance(forest), 2)
# ok so i see which variables are more or less impt
# but i don't know how to define my cutoff
# apparently this package will help(?)
library(VSURF)
a <- toys
View(a)
forest2 <- VSURF(log10(GDMforb) ~ ., data = dat.fb.forest)
summary(forest2)
names(forest2)
forest2$varselect.interp
forest2$varselect.pred
forest2$terms
