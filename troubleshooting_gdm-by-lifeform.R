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
  arrange(MedGDM))
#write.csv(gdm.covcls, file = "gdm-by-landcov.csv", row.names=F)
lev.covcls <- as.vector(gdm.covcls$cover_class)
dat$cover_class <- factor(dat$cover_class, levels = lev.covcls)