###########################################################
######## FORAGE QUALITY DIGESTIBILITY ESTIMATION  #########
###########################################################

## ** This file is edited version of foragequality.R, for calculating ave digestible energy per plot

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
wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Vegetation"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
        setwd(wd_worklaptop)
      }
    }

#### PACKAGES ####

library(RODBC)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

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
        if(file.exists(wd_worklaptop)) {
        channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                     dbq=C:/Users/kristin/Documents/DatabasesEtc/SapphireElkProject_VegetationDatabase.accdb")
        channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kristin/Documents/DatabasesEtc/ForagePlantDatabase.accdb")
		channel.sbroot <-  odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kristin/Documents/DatabasesEtc/SBrootVegData.accdb")
      }
     }
     }
rm(wd_workcomp, wd_laptop, wd_worklaptop)
		  
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


#### SUMMER FORAGE PLANTS ####
##- retain species that make up 95% of diet
#forage <- read.csv("NS_foragespecies_summer.csv") %>%
forage <- read.csv("ALLBROOT_foragespecies_summer.csv") %>% # already filtered to 95%
  #filter(cumave < 95) %>%
  #rename(Species=SpeciesName) %>%
  mutate(Species = as.character(trimws(Species))) %>%
  mutate(Genus2 = trimws(gsub(' leaf| stem', '', Species)))
forage <- forage[!duplicated(forage$Genus2),] # remove duplicates


#### WINTER FORAGE PLANTS ####
#forage.WIN <- read.csv("NS_foragespecies_winter.csv") %>%
forage.WIN <- read.csv("ALLBROOT_foragespecies_winter.csv") %>% # filtered to 95%
  #filter(cumave < 95) %>% 
  #rename(Species=SpeciesName)  %>% 
  mutate(Species = as.character(trimws(Species))) %>%
  mutate(Genus2 = trimws(gsub(' leaf| stem', '', Species)))
forage.WIN <- forage.WIN[!duplicated(forage.WIN$Genus2),] # remove duplicates


#forage <- forage.WIN


##########################################
#### DATA - MANIPULATIONS/CALCULATIONS ####

#### DEFINING FORAGE ####

# pull forage plants the lab identified to species
spp.forage.b <- forage %>%
  separate(Species, c("NameGenus","NameSpecies"), "\\s+", extra="drop") %>% # separate into genus and species (for PlantCode)
  filter(NameSpecies != "sp")  # remove those with "sp" for species (these are accounted for later)
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

#### QUADRAT FORAGE SPECIES COVER ####

forage.quadrat <- classn %>%
  semi_join(spp.forage, by = "Species") %>% #only keep forage plants
  group_by(QuadratVisit) %>%
  mutate(Cover = sum(Total)) %>%
  ungroup() %>%
  filter(LifeForm != "tree") %>%
  mutate(RescaledCover = Total/Cover)

length(unique(classn$PlotVisit)) # 657 total plots
length(unique(forage.quadrat$PlotVisit)) # 655 total plots w/ summer forage

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
phenospp.quadrat <- forage.quadrat %>%
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
  summarise(DE = mean(DE)) %>%
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

# add to DM per plot
de.plot <- de.plot %>% left_join(plotinfo, by="PlotVisit") 

# using only the first record of revisited phenology plots for each season
# first create dataframe of those with multiple revisits
de.plot.revisits <- de.plot %>%
  mutate(year=year(Date)) %>%
  group_by(PlotID, year) %>%
  mutate(n=n()) %>%
  filter(n>1)
# second create dataframe where PlotVisit randomly selected from first dataframe
de.plot.revisits.keep <- de.plot.revisits %>%
  summarise(PlotVisit=first(PlotVisit)) # select first PlotVisit that occurs
#summarise(PlotVisit=nth(PlotVisit, sample(1:max(n), 1))) # randomly select PLotVisit
# then create dataframe of revisits to remove from main data based on above selection
de.plot.revisits.remove <- anti_join(de.plot.revisits, de.plot.revisits.keep, by="PlotVisit")
# and finally remove those revisits
de.plot <- de.plot[!(de.plot$PlotVisit %in% de.plot.revisits.remove$PlotVisit),]

# subset to desired date range for summer season (July 7 - Aug 31)
de.plot$Date <- as.Date(de.plot$Date, format = "%Y-%m-%d")
de.plot <- de.plot %>%
  subset(Date >= "2014-07-07" & Date <= "2014-08-31" |
           Date >= "2015-07-07" & Date <= "2015-08-31")

de.plot <- de.plot %>%
  mutate(Area=ifelse(Latitude > 46.157, "Nsapph", "Sbroot"), # designate study area by Latitude (N of 46.157 = Nsapph)
         Date=format(Date, "%m/%d/%Y"),
         Season= "Summer")

de.plot.SUM <- de.plot



#####################
#### DE for WINTER FORAGE SPECIES ####
# RUN CODE ABOVE AGAIN reading in WINTER FORAGE PLANTS instead 
# (disregarding DM for SUMMER FORAGE SPECIES section)

# join phenology and digestibility by species (genus, class)
de.phenospp.quadrat <- forage.quadrat %>%
  left_join(DE.spp, by=c("Species" = "PlantCode")) %>%             # join to DE per species
  select(-c(EM.de, FL.de, FR.de, MS.de)) %>%                    # keeping only cured/senescent DE
  left_join(DE.gns, by=c("Genus2" = "NameGenus")) %>%              # join to DE averaged to genus
  select(-c(EM.gns.de, FL.gns.de, FR.gns.de, MS.gns.de)) %>%    # keeping only cured/senescent DE
  left_join(DE.cls, by=c("LifeForm" = "Class")) %>%                # join to DE averaged to lifeform
  select(-c(EM.cls.de, FL.cls.de, FR.cls.de, MS.cls.de)) %>%    # keeping only cured/senescent DE
  mutate(WIN.de = ifelse(is.na(SE.de), SE.gns.de, SE.de)) %>%   # fill in missing DE for cured/senescent phenostage with genus average DE values
  mutate(WIN.de = ifelse(is.na(SE.de), SE.cls.de, SE.de)) %>%   # if still missing DE, fill in with class average values
  mutate(DE = WIN.de*RescaledCover)                                # forage quality per species per quadrat
de.phenospp.quadrat <- select(de.phenospp.quadrat, PlotVisit, QuadratVisit, Species, LifeForm, RescaledCover, DE)

# forage quality per quadrat, per m2
de.quadrat <- de.phenospp.quadrat %>%
  group_by(QuadratVisit) %>%
  summarise(PlotVisit = first(PlotVisit),
            DE = sum(DE, na.rm=TRUE)) %>%
  ungroup()

# average forage quality per plot, per m2
de.plot <- de.quadrat %>%
  group_by(PlotVisit) %>%
  summarise(DE = mean(DE)) %>%
  ungroup()

# add back in plots with no forage species/no forage quality
classn.add <- classn %>%
  group_by(PlotVisit) %>%
  summarise(PlotID=first(PlotID)) %>%
  mutate(DE=0) %>%
  select(-PlotID)

classn.add <- classn.add[!classn.add$PlotVisit %in% de.plot$PlotVisit,] # these are plots with no forage species
de.plot <- bind_rows(de.plot, classn.add)
length(unique(de.plot$PlotVisit)) # 657 winter forage plots

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel, paste("select PlotID, Date, Latitude, Longitude from PlotInfo"))
#plotinfo <- read.csv("PlotInfo.csv") # if reading from csv
plotinfo <- mutate(plotinfo, PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

# add to DM per plot
de.plot <- de.plot %>% left_join(plotinfo, by="PlotVisit") 

# using only the first record of revisited phenology plots for each season
# first create dataframe of those with multiple revisits
de.plot.revisits <- de.plot %>%
  mutate(year=year(Date)) %>%
  group_by(PlotID, year) %>%
  mutate(n=n()) %>%
  filter(n>1)
# second create dataframe where PlotVisit randomly selected from first dataframe
de.plot.revisits.keep <- de.plot.revisits %>%
  summarise(PlotVisit=first(PlotVisit)) # select first PlotVisit that occurs
  #summarise(PlotVisit=nth(PlotVisit, sample(1:max(n), 1))) # randomly select PLotVisit
# then create dataframe of revisits to remove from main data based on above selection
de.plot.revisits.remove <- anti_join(de.plot.revisits, de.plot.revisits.keep, by="PlotVisit")
# and finally remove those revisits
de.plot <- de.plot[!(de.plot$PlotVisit %in% de.plot.revisits.remove$PlotVisit),]

de.plot <- de.plot %>%
  mutate(Area=ifelse(Latitude > 46.157, "Nsapph", "Sbroot"), # designate study area by Latitude (N of 46.157 = Nsapph)
         Date=format(Date, "%m/%d/%Y"),
         Season= "Winter")

de.plot.WIN <- de.plot



#####################
#### Join & Export SUMMER & WINTER DM data####
#de.data <- bind_rows(de.plot.SUM, de.plot.WIN)
de.data <- de.plot.SUM
de.data <- select(de.data, PlotID, Date, Longitude, Latitude, DE, Area, Season) # reorganizing
sapp.de.data <- de.data
write.csv(sapp.de.data, file = "sapp_DE_data.csv", row.names = FALSE)




#### ~~~~~~~~~~~~~~~~~~~~~ S BITTEROOT DATA ~~~~~~~~~~~~~~~~~~~~~~~~ ####

## Running this section will replace dataframes and objects from above code

# CLASSIFICATION - plus quadrat id, quadrat-visit ID, plot-visit ID, life form, genus
classn <- sqlQuery(channel.sbroot, paste("select * from Classification_broot"))
colnames(classn) <- c("VisitDate", "PlotID", "Species", "5m", "10m", "15m", "20m", "25m")
classn <- classn %>%
  gather("PlotM", "Total", 4:8) %>%
  mutate(Species = as.character(Species),
         Quadrat = paste(PlotID,"-", parse_number(PlotM), sep=""),
         QuadratVisit = paste(PlotID,".", VisitDate,".", parse_number(PlotM), sep=""),
         PlotVisit = paste(PlotID, ".", VisitDate, sep=""),
         Species = gsub("[.]","", Species))

# fix messy stuff
classn$Species[classn$Species=="Acer glab" | classn$Species=="Acer sp"] <- "Acer glabrum"
classn$Species[classn$Species=="Arnica angustifolium" | classn$Species=="Arnica lat" | classn$Species=="Arnica aln"] <- "Arnica sp"
classn$Species[classn$Species=="Artemisa sp"] <- "Artemisia sp"
classn$Species[classn$Species=="Artemisia tridentada"] <- "Artemisia tridentata"
classn$Species[classn$Species=="Aster conspicuus" | classn$Species=="Aster laevis"] <- "Aster sp"
classn$Species[classn$Species=="Bromus inermus"] <- "Bromus inermis"
classn$Species[classn$Species=="Geranium viscossissimum" | classn$Species=="Geranium sp"] <- "Geranium viscosissimum"
classn$Species[classn$Species=="Agoseris aurantiaca"] <- "Agoseris sp"
classn$Species[classn$Species=="Allium schoenoprasum"] <- "Allium sp"
classn$Species[classn$Species=="Antennaria alpina" | classn$Species=="Antennaria marginata" | classn$Species=="Antennaria parvifolia" | 
                 classn$Species=="Antennaria pulcherrima"] <- "Antennaria sp"
classn$Species[classn$Species=="Arenaria capillaris" | classn$Species=="Arenaria con"] <- "Arenaria sp"
classn$Species[classn$Species=="Mahonia repens"] <- "Berberis repens"
classn$Species[classn$Species=="Epilobium augustifolium" | classn$Species=="Epilobium sp"] <- "Chamerion angustifolium"
classn$Species[classn$Species=="Poa  sp"] <- "Poa sp"
classn$Species[classn$Species=="Ceanothus sp"] <- "Ceanothus velutinus"
classn$Species[classn$Species=="Cerastium sp"] <- "Cerastium arvense"
classn$Species[classn$Species=="Chimaphila sp"] <- "Chimaphila umbellata"
classn$Species[classn$Species=="Chrysothamnus nauseosus" | classn$Species=="Chrysothamnus sp"] <- "Ericameria nauseosa"
classn$Species[classn$Species=="Clematis sp"] <- "Clematis occidentalis"
classn$Species[classn$Species=="Collinsia sp"] <- "Collinsia parviflora"
classn$Species[classn$Species=="Collomia sp"] <- "Collomia linearis"
classn$Species[classn$Species=="Cynoglossum offincinale"] <- "Cynoglossum officinale"
classn$Species[classn$Species=="Danthonia alpinum"] <- "Danthonia sp"
classn$Species[classn$Species=="Deschamsia sp"] <- "Deschampsia sp"
classn$Species[classn$Species=="Descurainia sopha"] <- "Descurainia sp"
classn$Species[classn$Species=="Erigeron peregrinus"] <- "Erigeron sp"
classn$Species[classn$Species=="Erigonum montana"] <- "Erigonum sp"
classn$Species[classn$Species=="Erythronium sp"] <- "Erythronium grandiflorum"
classn$Species[classn$Species=="Delphinium  glaucum"] <- "Delphinium glaucum"
classn$Species[classn$Species=="Fairy bells"] <- "Prosartes sp"
classn$Species[classn$Species=="Festuca rubra"] <- "Festuca sp"
classn$Species[classn$Species=="Galium sp"] <- "Galium boreale"
classn$Species[classn$Species=="Gentian calycosa"] <- "Gentiana calycosa"
classn$Species[classn$Species=="Goodyera oblongfolia"] <- "Goodyera oblongifolia"
classn$Species[classn$Species=="Hairy grass"] <- "Unknown grass"
classn$Species[classn$Species=="Hieracium gracile"] <- "Hieracium sp"
classn$Species[classn$Species=="Juncus mertensianus"] <- "Juncus sp"
classn$Species[classn$Species=="Juniperus sp"] <- "Juniperus communis" # difficult to tell what species based on clip plots, but going to assume communis
classn$Species[classn$Species=="Koelaria sp"] <- "Koeleria macrantha"
classn$Species[classn$Species=="Lomatium montanum"] <- "Lomatium sp"
classn$Species[classn$Species=="Lonicera" & (classn$PlotID=="BM534" | classn$PlotID=="BM834")] <- "Lonicera utahensis" # based on clip plots, appears to be shrub species
classn$Species[classn$Species=="Lonicera" & classn$PlotID=="BM1014"] <- "Lonicera ciliosa" # based on clip plots, appears to be forb species
classn$Species[classn$Species=="Lupinus sericues"] <- "Lupinus sericeus"
classn$Species[classn$Species=="Maiathemum stellatum"] <- "Maianthemum stellatum"
classn$Species[classn$Species=="Menziesia"] <- "Menziesia ferruginea"
classn$Species[classn$Species=="Mustard invader"] <- "Unknown forb"
classn$Species[classn$Species=="Pedicularis race"] <- "Pedicularis racemosa"
classn$Species[classn$Species=="Pentaphylloides floribunda"] <- "Dasiphora fruticosa"
classn$Species[classn$Species=="Phacelia hast"] <- "Phacelia hastata"
classn$Species[classn$Species=="Picea englemanni"] <- "Picea engelmannii"
classn$Species[classn$Species=="Platanthera sp"] <- "Platanthera stricta"
classn$Species[classn$Species=="Poa alpina" | classn$Species=="Poa glauca"] <- "Poa sp"
classn$Species[classn$Species=="Potentilla glandulosa"] <- "Drymocallis glandulosa var glandulosa"
classn$Species[classn$Species=="Potentilla sp"] <- "Potentilla gracilis"  # forb based on clipplots evidence, so just assigning to forb potentilla
classn$Species[classn$Species=="Prunus virg" | classn$Species=="Prunus sp"] <- "Prunus virginiana"
classn$Species[classn$Species=="Ribes cer"] <- "Ribes cereum"
classn$Species[classn$Species=="Rumex sp"] <- "Rumex acetosella"
classn$Species[classn$Species=="Senecio can" | classn$Species=="Senecio sp"] <- "Packera pseudaurea"
classn$Species[classn$Species=="sicily"] <- "Osmorhiza berteroi"
classn$Species[classn$Species=="Solidago sp"] <- "Solidago canadensis"
classn$Species[classn$Species=="Spiderhead heart leaved vine"] <- "Unknown forb"
classn$Species[classn$Species=="Stellaria cal"] <- "Stellaria calycantha"
classn$Species[classn$Species=="Stellaria long"] <- "Stellaria longipes"
classn$Species[classn$Species=="Swertia albicaulis" | classn$Species=="Swertia sp"] <- "Frasera albicaulis"
classn$Species[classn$Species=="Thalictrum sp"] <- "Thalictrum occidentale"
classn$Species[classn$Species=="Tree in plot" | classn$Species=="Unknown conifer"] <- "Unknown tree"
classn$Species[classn$Species=="Trillium sp"] <- "Trillium ovatum"
classn$Species[classn$Species=="Unknown spicata"] <- "Unknown grass"
classn$Species[classn$Species=="Violet sp"] <- "Viola sp"
classn$Species[classn$Species=="Zygadenus elegans"] <- "Anticlea elegans"
classn$Species[classn$Species=="Unknown Shrub"] <- "Unknown shrub"

classn <- classn %>%
  mutate(Genus2 = paste(sapply(strsplit(as.character(Species), " "), "[", 1), "sp", sep=" "))


#### SUMMER FORAGE PLANTS ####
##- retain species that make up 95% of diet
#forage <- read.csv("SB_foragespecies_summer.csv") %>%
forage <- read.csv("ALLBROOT_foragespecies_summer.csv") %>% # already filtered 95%
  #filter(cumave < 95) %>% # retain species that make up 95% of diet
  #rename("Species"=SpeciesName) %>%
  mutate(Species = as.character(trimws(Species))) %>%
  mutate(Species = trimws(gsub(' leaf| stem', '', Species)))
forage <- forage[!duplicated(forage),] # remove duplicates


#### WINTER FORAGE PLANTS ####
#forage.WIN <- read.csv("SB_foragespecies_winter.csv") %>%
forage.WIN <- read.csv("ALLBROOT_foragespecies_winter.csv") %>% # already filtered 95%
  #filter(cumave < 95) %>% 
  #rename("Species"=SpeciesName) %>%
  mutate(Species = as.character(trimws(Species))) %>%
  mutate(Species = trimws(gsub(' leaf| stem', '', Species)))
forage.WIN <- forage.WIN[!duplicated(forage.WIN),] # remove duplicates


#forage <- forage.WIN


##########################################
#### DATA - MANIPULATIONS/CALCULATIONS ####

#### DEFINING FORAGE ####

# pull forage plants the lab identified to species
spp.forage.A <- forage %>%
  mutate(Species2 = Species) %>%
  separate(Species2, c("NameGenus","NameSpecies"), "\\s+", extra="drop") %>% # separate into genus and species (for PlantCode)
  filter(NameSpecies != "sp")  # remove those with "sp" for species (these are accounted for later)
forage.quadrat.A <- classn %>% 
  semi_join(spp.forage.A, by="Species") %>% # retain data matching to Species level
  left_join(spp.forage.A, by="Species") # add lifeform
# add all species of forage plants the lab only identified to genus
forage.quadrat.B <- classn %>%
  semi_join(forage, by = c("Genus2"="Species")) %>%  # retain all spp of forage plants only ID'd to genus
  left_join(forage, by = c("Genus2"="Species")) # add lifeform
forage.quadrat <- bind_rows(forage.quadrat.A, forage.quadrat.B) 
forage.quadrat <- forage.quadrat[!duplicated(forage.quadrat),] # remove duplicates
rm(forage.quadrat.A, forage.quadrat.B, spp.forage.A) # clean up workspace

#### QUADRAT FORAGE SPECIES COVER ####

forage.quadrat <- forage.quadrat %>%
  group_by(QuadratVisit) %>%
  filter(Total != 0) %>% # remove species with 0 cover - these came from gathering quadrat columns to rows
  mutate(Cover = sum(Total)) %>%
  ungroup() %>%
  mutate(RescaledCover = Total/Cover)

length(unique(classn$PlotID)) # 233 total plots
length(unique(forage.quadrat$PlotID)) # 233 total plots w/ summer forage

#### PHENOLOGY & DIGESTIBILITY ####

# read in PHENOLOGY data & format
phenology.data <- sqlQuery(channel.sbroot, paste("select * from Phenophase_broot")) %>%
  rename(Date = `Sampling Date`,
         Species = SpeciesName, 
         EM = Ave_New, 
         FL = Ave_Flower, 
         FR = Ave_Fruiting, 
         MS = Ave_Mature, 
         SE = Ave_Cured) %>%
  mutate(Species = as.character(Species)) %>%
  mutate(Species = gsub("[.]","", Species))
  
  # fix messy stuff - only fixing species that are also found in forage species list
phenology.data$Species[phenology.data$Species=="Bromus inermus"] <- "Bromus inermis"
phenology.data$Species[phenology.data$Species=="Mahonia repens"] <- "Berberis repens"
phenology.data$Species[phenology.data$Species=="Epilobium augustifolium"] <- "Chamerion angustifolium"
phenology.data$Species[phenology.data$Species=="Poa  sp"] <- "Poa sp"
phenology.data$Species[phenology.data$Species=="Agropyron spp."] <- "Agropyron sp"
phenology.data$Species[phenology.data$Species=="Salix spp."] <- "Salix sp"

phenology.data <- phenology.data %>%
  mutate(Species2 = Species) %>%
  separate(Species2, c("NameGenus","NameSpecies"), "\\s+", extra="drop") %>% # ignore "too few values" warning # thx jesse :)
  mutate(NameGenus = paste(NameGenus, "sp", sep=" ")) %>%
  left_join(forage, by="Species") %>% # joining to forage simply to acquire class/lifeform
  left_join(forage, by=c("NameGenus"="Species")) %>%
  mutate(Class.x = as.character(Class.x),
         Class.y = as.character(Class.y),
         Class=ifelse(is.na(Class.x), Class.y, Class.x)) %>%
  select(Species, NameGenus, Class, Date, EM, FL, FR, MS, SE)

# constraining phenology data to July 7 - Aug 31 and removing rows of all NAs
phenology.data$Date <- as.Date(phenology.data$Date, format = "%Y-%m-%d")
phenology.data <- phenology.data %>%
  subset(Date >= "2012-07-07" & Date <= "2012-08-31" |
         Date >= "2013-07-07" & Date <= "2013-08-31")

phenology <- phenology.data %>%
  select(-NameGenus, -Class, -Date) %>%
  group_by(Species) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

phenology.gns <- phenology.data %>%
  select(-Species, -Class, -Date) %>%
  group_by(NameGenus) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  rename(EM.gns = EM,
         FL.gns = FL,
         FR.gns = FR,
         MS.gns = MS,
         SE.gns = SE)

phenology.cls <- phenology.data %>%
  select(-Species, -NameGenus, -Date) %>%
  group_by(Class) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  rename(EM.cls = EM,
         FL.cls = FL,
         FR.cls = FR,
         MS.cls = MS,
         SE.cls = SE)

# read in DE data & format
DE.data <- sqlQuery(channel.DMD, paste("select * from data_DMD where NOT (StudyArea = 'ELKHORNS')"))
spp <- sqlQuery(channel.DMD, paste("select NameScientific, PlantCode from PlantSpeciesList")) # need full species name to join to phenospp.quadrat
DE.data <- DE.data %>%
  left_join(spp, by="PlantCode") %>%
  rename(Species = NameScientific) %>%
  mutate(Species = gsub("[.]","", Species))

# DE per species
DE.spp <- DE.data %>%
  group_by(Species, Stage) %>%
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



####################
#### DE for SUMMER FORAGE SPECIES ####

# join phenology data by species (genus, class)
phenospp.quadrat <- forage.quadrat %>% 
  left_join(phenology, by="Species") %>%
  left_join(phenology.gns, by=c("Species" = "NameGenus")) %>%
  left_join(phenology.cls, by="Class") %>%
  mutate(EM.cov = ifelse(is.na(EM), EM.gns, EM), # fill in missing phenology with genus average values
         FL.cov = ifelse(is.na(FL), FL.gns, FL),
         FR.cov = ifelse(is.na(FR), FR.gns, FR),
         MS.cov = ifelse(is.na(MS), MS.gns, MS),
         SE.cov = ifelse(is.na(SE), SE.gns, SE)) %>%
  mutate(EM.cov = ifelse(is.na(EM), EM.cls, EM), # if still missing phenology, fill with class average values
         FL.cov = ifelse(is.na(FL), FL.cls, FL),
         FR.cov = ifelse(is.na(FR), FR.cls, FR),
         MS.cov = ifelse(is.na(MS), MS.cls, MS),
         SE.cov = ifelse(is.na(SE), SE.cls, SE)) %>%
  select(-c(EM, FL, FR, MS, SE, 
            EM.gns, FL.gns, FR.gns, MS.gns, SE.gns, 
            EM.cls, FL.cls, FR.cls, MS.cls, SE.cls))

yo <- forage.quadrat %>%
  anti_join(phenology, by="Species") %>%
  anti_join(phenology.gns, by=c("Species" = "NameGenus")) %>%
  anti_join(phenology.cls, by="Class")  # these are the species records missing phenology data = 0

# Average forage quality per species per quadrat
de.phenospp.quadrat <- phenospp.quadrat %>%
  left_join(DE.spp, by="Species") %>%         # join forage per species to DE per species
  left_join(DE.gns, by=c("Genus2" = "NameGenus")) %>%         # join forage per species to DE per genus
  left_join(DE.cls, by="Class") %>%           # join forage per species to DE per lifeform
  mutate(EM.de = ifelse(is.na(EM.de), EM.gns.de, EM.de),    # fill in missing DE with genus average DE values
         FL.de = ifelse(is.na(FL.de), FL.gns.de, FL.de),
         FR.de = ifelse(is.na(FR.de), FR.gns.de, FR.de),
         MS.de = ifelse(is.na(MS.de), MS.gns.de, MS.de),
         SE.de = ifelse(is.na(SE.de), SE.gns.de, SE.de)) %>%
  mutate(EM.de = ifelse(is.na(EM.de), EM.cls.de, EM.de),    # if still missing DE, fill with class average DE values
         FL.de = ifelse(is.na(FL.de), FL.cls.de, FL.de),
         FR.de = ifelse(is.na(FR.de), FR.cls.de, FR.de),
         MS.de = ifelse(is.na(MS.de), MS.cls.de, MS.de),
         SE.de = ifelse(is.na(SE.de), SE.cls.de, SE.de)) %>%
  mutate(FL.de = ifelse(is.na(FL.de), 0, FL.de), # For PICO, only data for emergent, so to multiply and sum properly, converting remaining NA's to zero
         FR.de = ifelse(is.na(FR.de), 0, FR.de),
         MS.de = ifelse(is.na(MS.de), 0, MS.de),
         SE.de = ifelse(is.na(SE.de), 0, SE.de)) %>%
  select(-c(EM.cls.de, FL.cls.de, FR.cls.de, MS.cls.de, SE.cls.de,
            EM.gns.de, FL.gns.de, FR.gns.de, MS.gns.de, SE.gns.de)) %>%
  mutate(EM.de = EM.cov*EM.de,   
         FL.de = FL.cov*FL.de,
         FR.de = FR.cov*FR.de,
         MS.de = MS.cov*MS.de,
         SE.de = SE.cov*SE.de,
         DE = EM.de + FL.de + FR.de + MS.de + SE.de)    # calculate average forage quality per species per quadrat
write.csv(de.phenospp.quadrat, "broot_phenology_byforagespecies_perquadrat.csv")

# THIS SECTION FOR SUMMARIZING DE BY IMPORTANT FORAGE SPECIES (to be joined with NS data and summarized)
spp.summary.SB <- de.phenospp.quadrat %>%
  select(PlotVisit, QuadratVisit, Species, Class, RescaledCover, DE) %>%
  ungroup() %>%
  group_by(PlotVisit, Species) %>%
  summarise(mean=mean(DE)) %>%
  mutate(area="SB")

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
  summarise(DE = mean(DE)) %>%
  ungroup()

# add back in plots with no forage species/no forage quality
classn.add <- classn %>%
  group_by(PlotVisit) %>%
  summarise(PlotID=first(PlotID)) %>%
  mutate(DE=0) %>%
  select(-PlotID)

classn.add <- classn.add[!classn.add$PlotVisit %in% de.plot$PlotVisit,] # these are plots with no forage species
de.plot <- bind_rows(de.plot, classn.add)

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel.sbroot, paste("select PlotID, Date, Latitude, Longitude from PlotInfo")) %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

# add to DM per plot
de.plot <- de.plot %>%
  left_join(plotinfo, by="PlotVisit") %>% 
  mutate(Area = "Sbroot",
         Season = "Summer",
         Date = format(Date, "%m/%d/%Y"))

de.plot.broot.SUM <- de.plot
write.csv(de.plot, file = "broot_DE_data.csv", row.names = FALSE)



#####################
#### DE for WINTER FORAGE SPECIES ####
# RUN S BITTERROOT CODE ABOVE AGAIN reading in WINTER FORAGE PLANTS instead 
#   (disregarding DM for SUMMER FORAGE SPECIES section)

# join phenology/de data by species (genus, class)
de.phenospp.quadrat <- forage.quadrat %>%
  left_join(DE.spp, by="Species") %>%                      # join to DE per species
  select(-c(FL.de, FR.de, MS.de)) %>%                    # keeping only cured/senescent DE (AND EM for PICO only - only de data available for PICO)
  left_join(DE.gns, by=c("Genus2" = "NameGenus")) %>%              # join to DE averaged to lifeform (no need for DE averaged to class in this dataset)
  select(-c(EM.gns.de, FL.gns.de, FR.gns.de, MS.gns.de)) %>%    # keeping only cured/senescent DE
  mutate(WIN.de = ifelse(is.na(SE.de), SE.gns.de, SE.de)) %>%   # fill in missing de values with genus average values
  mutate(WIN.de = ifelse(is.na(WIN.de), EM.de, WIN.de)) %>%     # this only applies to PICO in this dataset
  mutate(DE = WIN.de*RescaledCover) %>%                            # forage quality per species per quadrat
  select(PlotVisit, QuadratVisit, Species, Class, RescaledCover, DE)

# forage quality per quadrat, per m2
de.quadrat <- de.phenospp.quadrat %>%
  group_by(QuadratVisit) %>%
  summarise(PlotVisit = first(PlotVisit),
            DE = sum(DE, na.rm=TRUE)) %>%
  ungroup()

# average forage quality per plot, per m2
de.plot <- de.quadrat %>%
  group_by(PlotVisit) %>%
  summarise(DE = mean(DE)) %>%
  ungroup()

# add back in plots with no forage species/no forage quality
classn.add <- classn %>%
  group_by(PlotVisit) %>%
  summarise(PlotID=first(PlotID)) %>%
  mutate(DE=0) %>%
  select(-PlotID)

classn.add <- classn.add[!classn.add$PlotVisit %in% de.plot$PlotVisit,] # these are plots with no forage species
de.plot <- bind_rows(de.plot, classn.add)

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel.sbroot, paste("select PlotID, Date, Latitude, Longitude from PlotInfo")) %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

de.plot <- de.plot %>%
  left_join(plotinfo, by="PlotVisit") %>%
  mutate(Area = "Sbroot",
         Season = "Winter",
         Date = format(Date, "%m/%d/%Y"))

de.plot.broot.WIN <- de.plot



####################
#### Join & Export SUMMER & WINTER DM data####
de.data <- bind_rows(de.plot.broot.SUM, de.plot.broot.WIN)
de.data <- select(de.data, PlotID, Date, Longitude, Latitude, DE, Area, Season) # reorganizing
broot.de.data <- de.data

write.csv(de.data, file = "broot_DE_data.csv", row.names = FALSE)




#####################
#### ~~~~~~~~~ Join & Export NSAPP & SBROOT -- SUMMER & WINTER DM data ~~~~~~~~~~~ ####
sapp.de.data <- read.csv("sapp_DE_data.csv")
broot.de.data <- read.csv("broot_DE_data.csv")

broot.de.data <- broot.de.data %>%
  select(colnames(sapp.de.data)) # make sure columns match before bindrows

sapp.de.data$PlotID <- as.character(sapp.de.data$PlotID)

all.de.data <- bind_rows(sapp.de.data, broot.de.data)
write.csv(all.de.data, file = "ALL_DE_data.csv", row.names = FALSE)


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