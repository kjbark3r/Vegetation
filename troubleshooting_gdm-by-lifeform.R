#############################################
#  MISC CODE FROM CREATING PREDICTIVE MODEL #
#          OF GDM BY LIFEFORM               #
#             KRISTIN BARKER                #
#                     DEC 2016              #
#############################################

################################################
## round values to nearest whole number, ####
## then use zero-inflated on count data ###

####              #
# Forb  #
####              #

test.fb <- dat.fb %>%
  mutate(CountForb = round(GDMforb))
hist(test.fb$CountForb, breaks = 300)

# global model 
mod.global.fb <- zeroinfl(CountForb ~ cc_std + cti_std + elev_std + 
                         gsri_std + slope_std + ndvi_ti_std + 
                         sum_precip_std + cover_class,
                         data = test.fb)
summary(mod.global.fb)
plot(mod.global.fb)

# backwards stepwise AIC on each of the above
step.fb <- stepAIC(mod.global.fb, direction = "both")

# backwards stepwise bic to more heavily penalize addl params, on normal dist
step.fb.bic <- step(mod.global.fb, direction = "both", k = log(652))

# dredge
options(na.action = "na.fail")
dredgemod <- dredge(mod.global.fb, beta = "none", evaluate = TRUE, 
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
# can't distinguish bt +elev and +elev+precip
summary(lm(log10(GDMforb) ~ cc_std + elev_std + gsri_std + 
                   ndvi_ti_std + sum_precip_std, data = dat.fb.no0))
# adj r-2 = 0.2411




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
