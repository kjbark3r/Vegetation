#############################################
#  MISC CODE FROM CREATING PREDICTIVE MODEL #
#          OF GDM BY LIFEFORM               #
#             KRISTIN BARKER                #
#                     DEC 2016              #
#############################################




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
