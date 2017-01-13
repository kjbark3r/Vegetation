###########################################################
######## FORAGE QUALITY DIGESTIBILITY ESTIMATION  #########
###########################################################

## *This is edited version of Jesse DeVoes's foragequality_DE.R, 
## altered to be specific to North Sapphire summer forage data

###  TO CONNECT TO ACCESS DATABASES, MUST USE 32 BIT R  ###
###  TO RUN IN 64 BIT, EXPORT DB TABLES TO CSV:         ###
###  -From SapphireElkProject_VegetationDatabase.accdb: ###
###        - NSERP_SP_list.csv                          ###
###        - Classification.csv                         ###
###        - Phenology.csv                              ###
###        - PlotInfo.csv                               ###
###  -From SBrootVegData.accdb:                         ###
###        - Classification_broot.csv                   ###
###        - Phenophase_broot.csv                       ###
###        - PlotInfo.csv                               ###
###  -From ForagePlantDatabase.accdb:                   ###
###        - DMDdata.csv                                ###

#### WD ####

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Vegetation"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Vegetation"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
        cat("Are you SURE you got that file path right?\n")
      }
    }

#### PACKAGES ####

library(RODBC)
library(rgdal) # read/write shp's
library(lubridate)
library(readr)
library(tidyr)
library(dplyr)

##########################################
#### DATA - READ IN AND SET UP ####

#Connect to Access vegetation and forage plant databases
    if(file.exists(wd_workcomp)) {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                   dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/SapphireElkProject_VegetationDatabase.accdb")
      channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/ForagePlantDatabase.accdb")
     } else {
      if(file.exists(wd_laptop)) {
        channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                     dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/SapphireElkProject_VegetationDatabase.accdb")
        channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/ForagePlantDatabase.accdb")
     } else {
        cat("Are you SURE you got that file path right?\n")
      }
          }
rm(wd_workcomp, wd_laptop)

#### ~~~~~ SAPPHIRE DATA ~~~~~~ ####

# SPECIES NAMES AND LIFE FORMS
#spp <- read.csv("NSERP_SP_list.csv") # if reading from csv
#spp <- select(spp, PlantCode, LifeForm, NameScientific) # if reading from csv
spp <- sqlQuery(channel, paste("select PlantCode, LifeForm, NameScientific
                               from NSERP_SP_list"))
spp <- rename(spp, Species = PlantCode)
spp$LifeForm <- trimws(spp$LifeForm)
spp$Genus <- sapply(strsplit(as.character(spp$NameScientific), " "), "[", 1)
spp$Genus2 <- paste(spp$Genus, "sp", sep=" ") # for defining forage later

# CLASSIFICATION - plus quadrat id, quadrat-visit ID, plot-visit ID, life form, genus
#classn <- read.csv("Classification.csv") # if reading from csv
classn <- sqlQuery(channel, paste("select * from Classification"))
colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
classn <- classn %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  left_join(spp, by = "Species")
classn$Genus <- sapply(strsplit(as.character(classn$NameScientific), " "), "[", 1)
classn$LifeForm <- ifelse(grepl(' GRASS| JUNCACEAE|CARE ', classn$Species), "graminoid", 
                               ifelse(grepl('UNK ', classn$Species), "forb", classn$LifeForm))


#### NORTH SAPPHIRE SUMMER FORAGE PLANTS ####
## retain species that make up 95% of diet
forage <- read.csv("NS_foragespecies_summer.csv") %>%
  filter(cumave < 95) %>%
  mutate(Genus2 = trimws(gsub(' leaf| stem', '', SpeciesName))) %>%
  rename("Species"=SpeciesName)
forage <- forage[!duplicated(forage$Genus2),] # remove duplicates



##########################################
#### DATA - MANIPULATIONS/CALCULATIONS ####

#### DEFINING FORAGE ####

# pull forage plants the lab identified to species
spp.forage.b <- forage %>%
  separate(Species, c("NameGenus","NameSpecies"), "\\s+", extra="drop") %>% # separate into genus and species (for PlantCode)
  filter(NameSpecies != "sp") %>%  # remove those with "sp" for species (these are accounted for later)
  mutate(NameSpecies = ifelse(NameSpecies=="altaica","campestris", NameSpecies)) # properly ID Festuca campestris
spp.forage.b$NameGenus <- toupper(spp.forage.b$NameGenus)  # capitalize
spp.forage.b$NameSpecies <- toupper(spp.forage.b$NameSpecies)  
spp.forage.b$PlantCode <- paste(substr(spp.forage.b$NameGenus, 1, 3), substr(spp.forage.b$NameSpecies, 1, 3), sep = "") # PlantCode to join on
spp.forage.b <- semi_join(spp, spp.forage.b, by=c("Species"="PlantCode")) # retain data matching to Species level
# add all species of forage plants the lab only identified to genus
spp.forage <- semi_join(spp, forage, by = "Genus2") %>% # retain all spp of forage plants only ID'd to genus
  bind_rows(spp.forage.b) %>% # add forage plants IDd to species 
  mutate(ForagePlant = "Yes")
spp.forage <- spp.forage[!duplicated(spp.forage),] # remove duplicates
rm(spp.forage.b, forage) # clean up workspace
write.csv(spp.forage, file="NSERP_ForagePlants_Summer.csv", row.names=FALSE)

#### QUADRAT FORAGE SPECIES COVER ####

forage.quadrat <- classn %>%
  semi_join(spp.forage, by = "Species") %>% #only keep forage plants
  group_by(QuadratVisit) %>%
  mutate(Cover = sum(Total)) %>%
  ungroup() %>%
  filter(LifeForm != "tree") %>%
  mutate(RescaledCover = Total/Cover)

length(unique(classn$PlotVisit)) # 657 total plots
length(unique(forage.quadrat$PlotVisit)) # 654 total plots w/ summer forage

#### PHENOLOGY & DIGESTIBILITY ####

# read in PHENOLOGY data & format
phenology <- sqlQuery(channel, paste("select * from Phenology"))
#phenology <- read.csv("Phenology.csv") # if reading from csv
#phenology$Date <- as.Date(phenology$Date, "%m/%d/%Y") # if from csv
phenology <- phenology %>% 
  select(Date, `Plot ID`, Species, EM, FL, FR, MS, SE) %>% 
  rename(PlotID = `Plot ID`)
#  select(Date, Plot.ID, Species, EM, FL, FR, MS, SE) %>% # if from csv
#  rename(PlotID = Plot.ID) # if from csv
phenology$Species <- trimws(phenology$Species) # remove whitespace causing join problems
phenology <- mutate(phenology, PlotVisit = paste(PlotID, ".", Date, sep=""))

phenology.spp <- phenology %>%        # there are a few species missing phenology info, so averaging for each species
  select(Species, EM, FL, FR, MS, SE) %>%
  group_by(Species) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  mutate(EM.sp=ifelse(EM>0.5, 1, 0),
         FL.sp=ifelse(FL>0.5, 1, 0),
         FR.sp=ifelse(FR>0.5, 1, 0), 
         MS.sp=ifelse(MS>0.5, 1, 0),
         SE.sp=ifelse(SE>0.5, 1, 0)) %>%   # bc of the nature of how handling phenology, need to convert to 0's and 1's
  select(Species, EM.sp, FL.sp, FR.sp, MS.sp, SE.sp)

# read in DE data & format
DE.data <- sqlQuery(channel.DMD, paste("select * from data_DMD where NOT (StudyArea = 'ELKHORNS')"))

# DE per species
DE.spp <- DE.data %>%
  group_by(PlantCode, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  spread(Stage, meanDE) %>%
  rename(EM.de=emergent, 
         FL.de=flowering, 
         FR.de=fruiting, 
         MS.de=mature, 
         SE.de=cured)

# DE per genus
DE.gns <- DE.data %>%
  mutate(NameGenus = paste(NameGenus, "sp", sep=" ")) %>%
  group_by(NameGenus, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  spread(Stage, meanDE) %>%
  rename(EM.gns.de=emergent, 
         FL.gns.de=flowering, 
         FR.gns.de=fruiting, 
         MS.gns.de=mature, 
         SE.gns.de=cured)

# DE per class
DE.cls <- DE.data %>%
  group_by(Class, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  spread(Stage, meanDE) %>%
  rename(EM.cls.de=emergent, 
         FL.cls.de=flowering, 
         FR.cls.de=fruiting, 
         MS.cls.de=mature, 
         SE.cls.de=cured)

# DE per species (AVE EM, FL, FR for phenologies 10000)
DE.spp.10000 <- DE.data %>%
  group_by(PlantCode, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  filter(Stage == "emergent" | Stage == "flowering" | Stage == "fruiting") %>%
  ungroup() %>%
  group_by(PlantCode) %>%
  summarise(EMFLFR.de = mean(meanDE))

# DE per genus (AVE EM, FL, FR for phenologies 10000)
DE.gns.10000 <- DE.data %>%
  mutate(NameGenus = paste(NameGenus, "sp", sep=" ")) %>%
  group_by(NameGenus, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  filter(Stage == "emergent" | Stage == "flowering" | Stage == "fruiting") %>%
  ungroup() %>%
  group_by(NameGenus) %>%
  summarise(EMFLFR.gns.de = mean(meanDE))

# DE per class (AVE EM, FL, FR for phenologies 10000)
DE.cls.10000 <- DE.data %>%
  group_by(Class, Stage) %>%
  summarize(meanDE = mean(DE)) %>%
  filter(Stage == "emergent" | Stage == "flowering" | Stage == "fruiting") %>%
  ungroup() %>%
  group_by(Class) %>%
  summarise(EMFLFR.cls.de = mean(meanDE))



################
#### DE for SUMMER FORAGE SPECIES ####

# join phenology by species
phenospp.quadrat <- forage.quadrat %>% # forage.quadrat already specific to NSapph forage definition
  left_join(phenology, by=c("PlotVisit","Species")) %>%  # join by plot and species
  left_join(phenology.spp, by="Species") %>%  # join to phenology averaged per species to fill in missing values
  mutate(EM=ifelse(is.na(EM), EM.sp, EM),  # if missing phenology, fill in average per species
         FL=ifelse(is.na(FL), FL.sp, FL),
         FR=ifelse(is.na(FR), FR.sp, FR),
         MS=ifelse(is.na(MS), MS.sp, MS),
         SE=ifelse(is.na(SE), SE.sp, SE)) %>%
  select(-c(EM.sp, FL.sp, FR.sp, MS.sp, SE.sp)) # clean up a little

phenospp.quadrat$FR <- ifelse(phenospp.quadrat$MS == 1, 1, phenospp.quadrat$FR) # keeping data consistent with field methods; if MS, then FR

# calculating cover per species per phenophase
# things to keep in mind: 
#  -everything either EM or SE (if both only, than cover split between both)
#  -if EM + other(s), cover assigned to other(s), none to EM
#  -if SE + other(s), cover assigned to other(s), none to SE
#  -if MS, then always FR, so cover assigned only to MS, none to FR
#  -if EM,FL,FR,MS,&SE, cover split between FL & MS
phenospp.quadrat <- mutate(phenospp.quadrat, index = paste(EM, FL, FR, MS, SE, sep=""))
unique(phenospp.quadrat$index) # confirm all index values are included in ifelse statement below
phenospp.quadrat <- phenospp.quadrat %>% 
  mutate(EM.cov = ifelse(index=="10000", 1,
                         ifelse(index=="10001", 0.5, 0)),
         FL.cov = ifelse(index=="11000", 1,
                         ifelse(index=="11100" | index=="11110" | index=="01111" | index=="11111", 0.5, 0)),
         FR.cov = ifelse(index=="10100" | index=="00101" | index=="10101", 1,
                         ifelse(index=="11100", 0.5, 0)),
         MS.cov = ifelse(index=="10110" | index=="00111" | index=="10111", 1,
                         ifelse(index=="11110" | index=="01111"  | index=="11111", 0.5, 0)),
         SE.cov = ifelse(index=="00001", 1,
                         ifelse(index=="10001", 0.5, 0)))

# Average forage quality per species per quadrat
de.phenospp.quadrat <- phenospp.quadrat %>%
  left_join(DE.spp, by=c("Species" = "PlantCode")) %>%         # join to DE per species
  left_join(DE.gns, by=c("Genus2" = "NameGenus")) %>%           # join to DE per Genus
  left_join(DE.cls, by=c("LifeForm" = "Class")) %>%           # join forage to DE per lifeform
  left_join(DE.spp.10000, by=c("Species" = "PlantCode")) %>%         # join to DE per species (ave for EM, FL, FR; as per becca)
  left_join(DE.gns.10000, by=c("Genus2" = "NameGenus")) %>%           # join to DE per Genus (ave for EM, FL, FR; as per becca)
  left_join(DE.cls.10000, by=c("LifeForm" = "Class")) %>%           # join forage to DE per lifeform (ave for EM, FL, FR; as per becca)
  mutate(EM.de = ifelse(is.na(EM.de), EM.gns.de, EM.de),  # fill in missing DE with genus average values
         FL.de = ifelse(is.na(FL.de), FL.gns.de, FL.de),
         FR.de = ifelse(is.na(FR.de), FR.gns.de, FR.de),
         MS.de = ifelse(is.na(MS.de), MS.gns.de, MS.de),
         SE.de = ifelse(is.na(SE.de), SE.gns.de, SE.de)) %>%
  mutate(EM.de = ifelse(is.na(EM.de), EM.cls.de, EM.de),    # if still missing DE, fill in with class average DE values
         FL.de = ifelse(is.na(FL.de), FL.cls.de, FL.de),
         FR.de = ifelse(is.na(FR.de), FR.cls.de, FR.de),
         MS.de = ifelse(is.na(MS.de), MS.cls.de, MS.de),
         SE.de = ifelse(is.na(SE.de), SE.cls.de, SE.de)) %>%
  mutate(EM.de = ifelse(index == 10000, EMFLFR.de, EM.de),          # for plots indicated as EM only, assign species average de of EM, FL, FR (as per Becca), otherwise, keep previously assigned
         EM.de = ifelse(is.na(EM.de), EMFLFR.gns.de, EM.de),       # if previous was NA, then assign genus average de of EM, FL, FR
         EM.de = ifelse(is.na(EM.de), EMFLFR.cls.de, EM.de)) %>%   # and if previous was still NA, then assign class average de of EM, FL, FR
  select(-c(EM.cls.de, FL.cls.de, FR.cls.de, MS.cls.de, SE.cls.de,
            EM.gns.de, FL.gns.de, FR.gns.de, MS.gns.de, SE.gns.de)) %>%
  mutate(EM.de = EM.cov*EM.de,   
         FL.de = FL.cov*FL.de,
         FR.de = FR.cov*FR.de,
         MS.de = MS.cov*MS.de,
         SE.de = SE.cov*SE.de,
         DE = EM.de + FL.de + FR.de + MS.de + SE.de) # calculate average forage quality per species per quadrat
write.csv(de.phenospp.quadrat, "sapp_phenology_byforagessp_perquadrat.csv")

# THIS SECTION FOR SUMMARIZING DE BY IMPORTANT FORAGE SPECIES (to be joined with SB data and summarized)
spp.summary.NS <- de.phenospp.quadrat %>%
  select(PlotVisit, QuadratVisit, Species, LifeForm, RescaledCover, DE) %>%
  ungroup() %>%
  group_by(PlotVisit, Species) %>%
  summarise(mean=mean(DE)) %>%
  mutate(area="NS")

# forage quality per quadrat, per m2
de.quadrat <- de.phenospp.quadrat %>%
  mutate(RescaledDE = DE*RescaledCover) %>%
  group_by(QuadratVisit) %>%
  summarise(PlotVisit = first(PlotVisit),
            DE = sum(RescaledDE)) %>%
  ungroup()

# average forage quality per plot, per m2
de.plot <- de.quadrat %>%
  group_by(PlotVisit) %>%
  summarise(DE = mean(DE)) %>% # mean is OK bc no 0s in these data
  ungroup()

# add back in plots with no forage species/no forage quality
classn.add <- classn %>%
  group_by(PlotVisit) %>%
  summarise(PlotID=first(PlotID)) %>%
  mutate(DE=0) %>%
  select(-PlotID)

classn.add <- classn.add[!classn.add$PlotVisit %in% de.plot$PlotVisit,] # these are plots with no forage species
de.plot <- bind_rows(de.plot, classn.add)
length(unique(de.plot$PlotVisit)) # 657 total plots

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel, paste("select PlotID, Date, Latitude, Longitude from PlotInfo"))
#plotinfo <- read.csv("PlotInfo.csv") # if reading from csv
plotinfo <- mutate(plotinfo, PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

# add to DE per plot
de.plot <- de.plot %>% left_join(plotinfo, by="PlotVisit") 
write.csv(de.plot, file = "de-plot-allvisits.csv", row.names = FALSE)


# subset only jul/aug data; only use first visit of phenology plots
## summer = jul1 - aug31 (same as dates used to create summer KDEs to calc VI)
de.plot.summ <- de.plot
de.plot.summ$Date <- as.Date(de.plot.summ$Date)
de.plot.summ <- de.plot.summ %>%
  subset(Date >= "2014-07-01" & Date <= "2014-08-31" | 
         Date >= "2015-07-01" & Date <= "2015-08-31") %>%
  mutate(PlotYear = paste(PlotID, ".", format(Date, '%Y'), sep=""))
de.plot.summ <- de.plot.summ[!duplicated(de.plot.summ$PlotYear),] 
write.csv(de.plot.summ, file = "de-plot-summeronly.csv", row.names=FALSE)

# export summer de data as shapefile
latlong = CRS("+init=epsg:4326") # define projection
xy <- data.frame("x"=de.plot.summ$Longitude,"y"=de.plot.summ$Latitude)
ll <- SpatialPointsDataFrame(xy, de.plot.summ, proj4string = latlong)
writeOGR(ll, dsn = ".", layer="de-plot-summer", driver="ESRI Shapefile",
           overwrite_layer = TRUE)

odbcCloseAll() # Closes connections to databases




#### ~~~~~~~ General summary of Species DE values across Bitterroot ~~~~~~~~~~ ####
spp.summary <- bind_rows(spp.summary.NS, spp.summary.SB)
spp.summary1 <- spp.summary %>%
  ungroup() %>%
  group_by(Species) %>%
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))

spp.summary %>% ungroup() %>% filter(grepl("POA", Species)) %>% # all poa species
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))
spp.summary %>% ungroup() %>% filter(grepl("CAR", Species)) %>% # all carex species
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))
spp.summary %>% ungroup() %>% filter(grepl("LUP", Species)) %>% # all lupinus species
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))
spp.summary %>% ungroup() %>% filter(grepl("SAL", Species)) %>% # all salix species
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))
spp.summary %>% ungroup() %>% filter(grepl("VAC", Species)) %>% # all Vaccinium species
  summarize(meanDM=mean(mean),
            sdDM=sd(mean))

#### ~~~~~~~ Number of Forage Species with DE values ~~~~~~~~~~ ####
spp.forage.de <- DE.data %>% #Saphire data
  semi_join(spp.forage, by=c("PlantCode"="Species"))
ns.de <- data.frame("Species"=unique(spp.forage.de$Species))

forage.quadrat.de <- DE.data %>% # S broot data, have to run code sections above independently from Sapph data
  semi_join(forage, by=c("Species"))
br.de <- data.frame("Species"=unique(forage.quadrat.de$Species))

forage.de.same <- semi_join(br.de, ns.de, by="Species") # species w/ de values in both datasets
forage.de.dif.br <- anti_join(br.de, ns.de, by="Species") # species w/ de values only in s broot
forage.de.dif.ns <- anti_join(ns.de, br.de, by="Species") # species w/ de values only in s broot

nrow(forage.de.same) + nrow(forage.de.dif.br) + nrow(forage.de.dif.ns)