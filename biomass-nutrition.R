###########################################################
######## BIOMASS AND NUTRITION ESTIMATION - NSERP #########
###########################################################

###  TO CONNECT TO ACCESS DATABASES, MUST USE 32 BIT R  ###
###  TO RUN IN 64 BIT, EXPORT DB TABLES TO CSV:         ###
###  -From SapphireElkProject_VegetationDatabase.accdb: ###
###        - NSERP_SP_list.csv                          ###
###        - Classification.csv                         ###
###        - ClipPlots.csv                              ###
###        - MeasurePlants.csv                          ###
###        - Phenology.csv                              ###
###  -From ForagePlantDatabase.accdb:                   ###
###        - DMDdata.csv                                ###


#### WD ####

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Vegetation"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Vegetation"
wd_jesse <- "E:\\NSERP_DeVoe\\Sapphire Elk\\Data\\ElkForageDiet\\ForagePlantAnalysis\\biomass_gdm"
wd_kelly <- "F:\\SapphireVegModels\\DataCode"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
    if(file.exists(wd_jesse)) {
      setwd(wd_jesse)
    } else {
      if(file.exists(wd_kelly)) {
        setwd(wd_kelly)
      } else {
        cat("Are you SURE you got that file path right?\n")
      }
    }}}

#### PACKAGES ####

library(RODBC)
library(dplyr)
library(tidyr)
library(lubridate)

##########################################
#### DATA - READ IN AND SET UP ####

#Connect to Access vegetation and forage plant databases
if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/SapphireElkProject_VegetationDatabase.accdb")
  channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/ForagePlantDatabase.accdb")
  } else {
  if(file.exists(wd_laptop)) {
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/SapphireElkProject_VegetationDatabase.accdb")
    } else {
    if(file.exists(wd_jesse)) {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                   dbq=E:/NSERP_DeVoe/Sapphire Elk/Data/ElkForageDiet/VegetationDatabase/SapphireElkProject_VegetationDatabase_2016-08-17.accdb")
      channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=E:/NSERP_DeVoe/Sapphire Elk/Data/ElkForageDiet/ForagePlantAnalysis/ForagePlantDatabase.accdb")
          } else {
      if(file.exists(wd_kelly)) {
        channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                     dbq=F:/SapphireVegModels/DataCode/SapphireElkProject_VegetationDatabase_2016-08-17.accdb")
      } else {
        cat("Are you SURE you got that file path right?\n")
      }
          }}}
rm(wd_workcomp, wd_laptop, wd_jesse, wd_kelly)

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

# HERBACEOUS COVER - creating manually because some recorded numbers are incorrect
cover <- classn %>%
  #subset(!PlotM == 10 & !PlotM == 30) %>% #remove non-clipplots
  group_by(QuadratVisit, LifeForm) %>%
  summarise(Wt = sum(Total)) %>%
  spread(LifeForm, Wt) %>%
  rename(ForbCov = forb, GrassCov = graminoid) %>%
  ungroup()
cover$ForbCov[is.na(cover$ForbCov)] <- 0; cover$GrassCov[is.na(cover$GrassCov)] <- 0

# CLIP PLOTS - plus quadrat ID, quadrat-visit ID
#clip <- read.csv("ClipPlots.csv") # if reading from csv
clip <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(clip) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
clip <- clip %>%
  mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep=""))

# SUMMER FORAGE PLANTS - retain species that make up 95% of diet
forage <- read.csv("NS_foragespecies_summer.csv") %>%
  filter(cumave < 95) %>%
  mutate(Genus2 = trimws(gsub(' leaf| stem', '', SpeciesName))) %>%
  rename("Species"=SpeciesName)

# WINTER FORAGE PLANTS
#forage.WIN <- read.csv("NS_foragespecies_winter.csv") %>%
#  filter(cumave < 95) %>% # retain species that make up 95% of diet
#  mutate(Genus2 = trimws(SpeciesName)) %>% # for defining forage later
#  rename("Species"=SpeciesName)
#forage <- forage.WIN

##########################################
#### DATA - MANIPULATIONS/CALCULATIONS ####

##########
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
#write.csv(spp.forage, file="NSERP_ForagePlants_Winter.csv", row.names=FALSE)

##########
#### HERBACEOUS BIOMASS ####

# herbaceous biomass per quadrat - by life form
quadrat <- clip %>%
  select(QuadratVisit, PlotVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) 
quadrat$ForbWt[is.na(quadrat$ForbWt)] <- 0 #replace NA with 0 
quadrat$GrassWt[is.na(quadrat$GrassWt)] <- 0  

# herbaceous biomass per quadrat - by species
quadrat.herb <- left_join(cover, classn, by = "QuadratVisit") %>%
  select(-PlotVisit) #avoid duplicated column name after next join
quadrat.herb$RescaledCover <- ifelse(quadrat.herb$LifeForm == "forb", quadrat.herb$Total/quadrat.herb$ForbCov,
                                      ifelse(quadrat.herb$LifeForm == "graminoid", 
                                             quadrat.herb$Total/quadrat.herb$GrassCov, 
                                             ifelse(NA)))
quadrat.herb <- left_join(quadrat.herb, quadrat, by = "QuadratVisit")
quadrat.herb <- subset(quadrat.herb, select = c(PlotVisit, QuadratVisit, Species, Genus, 
                                              RescaledCover, LifeForm, ForbCov, GrassCov, 
                                              ForbWt, GrassWt))
quadrat.herb$ClipGrams <- ifelse(quadrat.herb$LifeForm == "forb", quadrat.herb$RescaledCover*quadrat.herb$ForbWt,
                                ifelse(quadrat.herb$LifeForm == "graminoid", quadrat.herb$RescaledCover*quadrat.herb$GrassWt,
                                       ifelse(NA)))
quadrat.herb <- quadrat.herb[!is.na(quadrat.herb$ClipGrams),] #remove quadrats without clip plots

# herbaceous biomass per plot - by species
plot.herb.species <- quadrat.herb %>%
  group_by(PlotVisit, Species) %>%
  summarise(Biomass = sum(ClipGrams)*(4/3)) %>% # sum/3 averages all 3 clip plots (to incl clip plots w/ 0 weights)
  left_join(spp, by = "Species") %>%            # *4 to scale up to g/m^2
  select(-c(Genus, Genus2)) %>%
  ungroup()
plot.herb.species$LifeForm <- ifelse(grepl(' GRASS| JUNCACEAE|CARE ', plot.herb.species$Species), "graminoid", 
                             ifelse(grepl('UNK ', plot.herb.species$Species), "forb", plot.herb.species$LifeForm))

# herbaceous forage biomass per plot - by species
forage.herb <- plot.herb.species %>%
  select(-NameScientific) %>%
  semi_join(spp.forage, by = "Species") #only keep forage plants

# herbaceous biomass per plot - by life form
lifeform.herb <- plot.herb.species %>%
  group_by(PlotVisit, LifeForm) %>%
  summarise(Biomass = sum(Biomass)) %>% #already scaled up to g/m^2
  ungroup() %>%
  spread(LifeForm, Biomass) %>% 
  rename(ForbBiomass = forb, GrassBiomass = graminoid)
lifeform.herb$ForbBiomass[is.na(lifeform.herb$ForbBiomass)] <- 0
lifeform.herb$GrassBiomass[is.na(lifeform.herb$GrassBiomass)] <- 0
lifeform.herb <- mutate(lifeform.herb, HerbaceousBiomass = ForbBiomass+GrassBiomass) 

# herbaceous forage biomass per plot - by life form
lifeform.herb.forage <- quadrat.herb %>%
  select(-c(Genus, LifeForm)) %>% #avoid duplicated columns after join
  left_join(spp.forage, by = "Species") %>%
  filter(ForagePlant == "Yes") %>%
  group_by(QuadratVisit, LifeForm) %>%
  summarise(Biomass = sum(ClipGrams)) %>% #still at quadrat level
  spread(LifeForm, Biomass) %>% 
  rename(ForageForbBiomass = forb, ForageGrassBiomass = graminoid) %>%
  ungroup() #0s have lifeform in plot but not clip plot; NAs don't have lifeform in plot
#below 4 lines are for 2 clip plots that are missing forb species data in access database
#i estimated proportion of forbs that are forage forbs based on cover in other quadrats
#see missingdata-estimations.R and Nutrition Quicknotes for more info
lifeform.herb.forage[lifeform.herb.forage$QuadratVisit %in% "323.2014-06-30.20","ForageForbBiomass"] <- 
  quadrat[quadrat$QuadratVisit %in% "323.2014-06-30.20","ForbWt"]*0.2 #ff323
lifeform.herb.forage[lifeform.herb.forage$QuadratVisit %in% "344.2014-06-16.20","ForageForbBiomass"] <-  
  quadrat[quadrat$QuadratVisit %in% "344.2014-06-16.20","ForbWt"]*0.3337 #ff344
lifeform.herb.forage$ForageForbBiomass[is.na(lifeform.herb.forage$ForageForbBiomass)] <- 0
lifeform.herb.forage$ForageGrassBiomass[is.na(lifeform.herb.forage$ForageGrassBiomass)] <- 0
lifeform.herb.forage <- lifeform.herb.forage %>% #now scale up to plot level, g/m^2
  mutate(PlotVisit = sub("(.*)[.].*", "\\1", QuadratVisit)) %>%
  group_by(PlotVisit) %>% 
  summarise(ForageForbBiomass = sum(ForageForbBiomass)*(4/3), ForageGrassBiomass = sum(ForageGrassBiomass)*(4/3)) %>%
  ungroup()
lifeform.herb.forage$HerbaceousForageBiomass <- lifeform.herb.forage$ForageForbBiomass+lifeform.herb.forage$ForageGrassBiomass

# herbaceous biomass and herbaceous forage biomass per plot - by life form
plot.herb.lifeform <- full_join(lifeform.herb, lifeform.herb.forage, by = "PlotVisit")
plot.herb.lifeform[is.na(plot.herb.lifeform)] <- 0

##########
#### SHRUB BIOMASS ####

# equations to estimate biomass
shrub.eqn <- read.csv("shrub-biomass-equations.csv")
shrub.eqn$Species <- as.character(shrub.eqn$Species)

# functions to estimate biomass
L <- function(x, a, b) {
  grams <- a*x + b
  return(grams)
}

E <- function(x, a, b) {
  grams <- a*(exp(1)^(b*x))
  return(grams)
}

P <- function(x, a, b) {
  grams <- a*(x^b)
  return(grams)
}

# shrub basal area and biomass per quadrat
#quadrat.shrub <- read.csv("MeasurePlants.csv") # if reading from csv
quadrat.shrub <- sqlQuery(channel, paste("select * from MeasurePlants"))
colnames(quadrat.shrub) <- c("Date", "PlotID", "PlotM", "Species", "Basal1", "Basal2", "Basal3", 
                             "Basal4", "Basal5", "nStems")
quadrat.shrub$Species <- trimws(quadrat.shrub$Species)
quadrat.shrub[, 5:9][quadrat.shrub[, 5:9] == 0] <- NA #make 0s NAs so rowMeans works
quadrat.shrub <- quadrat.shrub %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".", Date,".",PlotM, sep="")) %>%
  left_join(shrub.eqn, by = "Species")
quadrat.shrub$AvgBasal <- rowMeans(subset(quadrat.shrub, select = (Basal1:Basal5)), na.rm=TRUE)
quadrat.shrub$g.Leaves <- ifelse(quadrat.shrub$fcn.Leaves == "E", quadrat.shrub$nStems*E(quadrat.shrub$AvgBasal, quadrat.shrub$a.Leaves, quadrat.shrub$b.Leaves), 
                         ifelse(quadrat.shrub$fcn.Leaves == "L", quadrat.shrub$nStems*L(quadrat.shrub$AvgBasal, quadrat.shrub$a.Leaves, quadrat.shrub$b.Leaves),
                                ifelse(quadrat.shrub$fcn.Leaves == "P", quadrat.shrub$nStems*P(quadrat.shrub$AvgBasal, quadrat.shrub$a.Leaves, quadrat.shrub$b.Leaves),
                                       NA)))
quadrat.shrub$g.Stems <- ifelse(quadrat.shrub$fcn.Stems == "E", quadrat.shrub$nStems*E(quadrat.shrub$AvgBasal, quadrat.shrub$a.Stems, quadrat.shrub$b.Stems), 
                        ifelse(quadrat.shrub$fcn.Stems == "L", quadrat.shrub$nStems*L(quadrat.shrub$AvgBasal, quadrat.shrub$a.Stems, quadrat.shrub$b.Stems),
                               ifelse(quadrat.shrub$fcn.Stems == "P", quadrat.shrub$nStems*P(quadrat.shrub$AvgBasal, quadrat.shrub$a.Stems, quadrat.shrub$b.Stems),
                                      NA)))
quadrat.shrub$g.Total <- quadrat.shrub$g.Leaves+quadrat.shrub$g.Stems
quadrat.shrub <- select(quadrat.shrub, c(Date, PlotID, PlotM, Species, PlotVisit, 
                                            QuadratVisit, g.Leaves, g.Stems, g.Total))

# shrub biomass per plot - by species
plot.shrub.species <- quadrat.shrub %>%
  group_by(PlotVisit, Species) %>% #no need to scale up to m^2, bc shrubs measured in entire 1m^2 quadrat
  summarise(Biomass = sum(g.Total)/5) %>% #average of all 5 quadrats (accounts for quadrats without measurements)
  ungroup()

# forage shrub biomass per plot - by species
forage.shrub <- plot.shrub.species %>%
  semi_join(spp.forage, by = "Species")

# shrub biomass and forage shrub biomass per plot
plot.shrub.lifeform <- plot.shrub.species %>% 
  rename(ShrubBiomass = Biomass) %>%
  full_join(forage.shrub, by = c("PlotVisit", "Species")) %>%
  rename(ForageShrubBiomass = Biomass) 
plot.shrub.lifeform[is.na(plot.shrub.lifeform)] <- 0
plot.shrub.lifeform <- plot.shrub.lifeform %>%
  group_by(PlotVisit) %>%
  summarise(ShrubBiomass = sum(ShrubBiomass), ForageShrubBiomass = sum(ForageShrubBiomass)) %>% 
  ungroup()


##########
#### COMBINING BIOMASS DATA ####

# all biomass and forage biomass, by life form
biomass.lifeform <- left_join(plot.herb.lifeform, plot.shrub.lifeform, by = "PlotVisit") 
biomass.lifeform[is.na(biomass.lifeform)] <- 0
biomass.lifeform <- biomass.lifeform %>%
    mutate(Biomass = ShrubBiomass+HerbaceousBiomass, 
         ForageBiomass = ForageShrubBiomass+HerbaceousForageBiomass) %>%
  mutate(PlotID = sub("\\..*", "", PlotVisit), Date = sub("(.*)[.](.*)", "\\2", PlotVisit))
biomass.lifeform <- biomass.lifeform[,c(12, 13, 1, 10, 11, 4, 5, 2, 7, 8, 3, 9)]
#write.csv(biomass.lifeform, file = "biomass_summer.csv", row.names=FALSE)
#write.csv(biomass.lifeform, file = "biomass_winter.csv", row.names=FALSE)
write.csv(biomass.lifeform, file = "biomass-plot.csv", row.names=F)

# all biomass, by species
biomass.species <- plot.herb.species %>%
  select(PlotVisit, Species, Biomass) %>%
  bind_rows(plot.shrub.species) %>%
  mutate(PlotID = sub("\\..*", "", PlotVisit), Date = sub("(.*)[.](.*)", "\\2", PlotVisit)) 

# biomass by plot
plot.biomass <- biomass.species %>%
  group_by(PlotVisit) %>%
  summarise(Biomass = sum(Biomass))

# forage biomass only, by species (for nutrition estimation)
plot.foragespp <- forage.shrub %>%
  mutate(LifeForm = "shrub") %>%
  bind_rows(forage.herb)
write.csv(plot.foragespp, file = "foragebiomass_summer.csv", row.names = FALSE)
#write.csv(plot.foragespp, file = "foragebiomass_winter.csv", row.names = FALSE)

# forage biomass by plot
plot.forage <- plot.foragespp %>%
  group_by(PlotVisit) %>%
  summarise(Biomass = sum(Biomass))

##########
#### NUTRITION ####

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

# read in DMD data & format
channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                 dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/ForagePlantDatabase.accdb")
#channel.DMD <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#                                 dbq=E:/NSERP_DeVoe/Sapphire Elk/Data/ElkForageDiet/ForagePlantAnalysis/ForagePlantDatabase.accdb")
DMD.data <- sqlQuery(channel.DMD, paste("select * from DMDdata"))
#DMD.data <- read.csv("DMDdata.csv") # if reading from csv
DMD.data$DMD <- DMD.data$DMD/100 # turn percent to proportion

# DMD per species
DMD.spp <- DMD.data %>%
  group_by(PlantCode, Stage) %>%
  summarize(meanDMD = mean(DMD)) %>%
  spread(Stage, meanDMD) %>%
  rename(EM.dmd=emergent, FL.dmd=flowering, FR.dmd=fruiting, MS.dmd=mature, SE.dmd=cured)

# DMD per class
DMD.clss <- DMD.data %>%
  group_by(Class, Stage) %>%
  summarize(meanDMD = mean(DMD)) %>%
  spread(Stage, meanDMD) %>%
  rename(EM.cls.dmd=emergent, FL.cls.dmd=flowering, FR.cls.dmd=fruiting, MS.cls.dmd=mature, SE.cls.dmd=cured)


#### GDM for SUMMER FORAGE SPECIES ####

###
# GDM per plot - by species and phenophase
###

# join with forage biomass by species
plot.phenospp <- left_join(plot.foragespp, phenology, by=c("PlotVisit","Species")) 
anti_join(plot.foragespp, phenology, by=c("PlotVisit","Species")) # these are the species records missing phenology data
plot.phenospp <- plot.phenospp[!is.na(plot.phenospp$Date),] # remove these
plot.phenospp$FR <- ifelse(plot.phenospp$MS == 1, 1, plot.phenospp$FR) # keeping data consistent with field methods; if MS, then FR

# calculating biomass per species per phenophase
# things to keep in mind: 
#  -everything either EM or SE (if both only, than biomass split between both)
#  -if EM + other(s), biomass assigned to other(s), none to EM
#  -if SE + other(s), biomass assigned to other(s), none to SE
#  -if MS, then always FR, so biomass assigned only to MS, none to FR
plot.phenospp <- mutate(plot.phenospp, index = paste(EM, FL, FR, MS, SE, sep=""))
unique(plot.phenospp$index) # confirm all index values are included in ifelse statement below
plot.phenospp.biom <- plot.phenospp %>% mutate(EM.biom = ifelse(index=="10000", Biomass,
                                                           ifelse(index=="10001", Biomass*0.5, 0)),
                                          FL.biom = ifelse(index=="11000", Biomass,
                                                           ifelse(index=="11100" | index=="11110" | index=="01111", Biomass*0.5, 0)),
                                          FR.biom = ifelse(index=="10100" | index=="00101" | index=="10101", Biomass,
                                                           ifelse(index=="11100", Biomass*0.5, 0)),
                                          MS.biom = ifelse(index=="10110" | index=="00111" | index=="10111", Biomass,
                                                           ifelse(index=="11110" | index=="01111", Biomass*0.5, 0)),
                                          SE.biom = ifelse(index=="00001", Biomass,
                                                           ifelse(index=="10001", Biomass*0.5, 0)))

# Digestible forage biomass per plot - by species and phenophase
gdm.plot.phenospp <- plot.phenospp.biom %>%
  left_join(DMD.spp, by=c("Species" = "PlantCode")) %>%         # join forage biomass per species to DMD per species
  left_join(DMD.clss, by=c("LifeForm" = "Class")) %>%           # join forage biomass per species to DMD per lifeform
  mutate(EM.dmd = ifelse(is.na(EM.dmd), EM.cls.dmd, EM.dmd),    # fill in missing DMD with class average DMD values
         FL.dmd = ifelse(is.na(FL.dmd), FL.cls.dmd, FL.dmd),
         FR.dmd = ifelse(is.na(FR.dmd), FR.cls.dmd, FR.dmd),
         MS.dmd = ifelse(is.na(MS.dmd), MS.cls.dmd, MS.dmd),
         SE.dmd = ifelse(is.na(SE.dmd), SE.cls.dmd, SE.dmd)) %>%
  mutate(EM.gdm = EM.biom*EM.dmd,    # calculate grams of digestible forage biomass per species-phenophase
         FL.gdm = FL.biom*FL.dmd,
         FR.gdm = FR.biom*FR.dmd,
         MS.gdm = MS.biom*MS.dmd,
         SE.gdm = SE.biom*SE.dmd) %>%
  mutate(TOTAL.gdm = EM.gdm+FL.gdm+FR.gdm+MS.gdm+SE.gdm) %>%  # calculate total grams of digestible forage biomass per plot per species
  mutate(EM.dmd.p = EM*EM.dmd,      # calculate percent dmd per phenospp present in the plot (for calc. ave. dmd weighted by biomass)
         FL.dmd.p = FL*FL.dmd,
         FR.dmd.p = FR*FR.dmd,
         MS.dmd.p = MS*MS.dmd,
         SE.dmd.p = SE*SE.dmd) %>%
  mutate(EM.dmd.p = ifelse(EM.dmd.p > 0, EM.dmd.p, NA),   # turn zeros to NAs so not incorporated into average
         FL.dmd.p = ifelse(FL.dmd.p > 0, FL.dmd.p, NA),
         FR.dmd.p = ifelse(FR.dmd.p > 0, FR.dmd.p, NA),
         MS.dmd.p = ifelse(MS.dmd.p > 0, MS.dmd.p, NA),
         SE.dmd.p = ifelse(SE.dmd.p > 0, SE.dmd.p, NA)) %>%
  group_by(PlotVisit, Species) %>%
  mutate(AVE.dmd = mean(c(EM.dmd.p, FL.dmd.p, FR.dmd.p, MS.dmd.p, SE.dmd.p), na.rm = TRUE)) %>%  # take average of phenospecific dmds
  ungroup()

gdm.plot.phenospp <- select(gdm.plot.phenospp, PlotVisit, Species, LifeForm, Biomass, TOTAL.gdm, AVE.dmd)

# grams of digestible forage biomass per lifeform
gdm.plot.lifeform <- gdm.plot.phenospp %>%
  group_by(PlotVisit, LifeForm) %>%
  summarise(GDM = round(sum(TOTAL.gdm),3),
            BIOMASS = round(sum(Biomass), 3))
  ungroup()

# grams of digestible forage biomass per plot
gdm.plot <- gdm.plot.lifeform %>%
  group_by(PlotVisit) %>%
  summarise(GDM = sum(GDM),
            BIOMASS = sum(BIOMASS)) %>%
  ungroup()

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel, paste("select PlotID, Date, Latitude, Longitude from PlotInfo"))
#plotinfo <- read.csv("PlotInfo.csv") # if reading from csv
plotinfo <- mutate(plotinfo, PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

# add to GDM per plot
gdm.plot <- left_join(gdm.plot, plotinfo, by="PlotVisit")
#write.csv(gdm.plot, file = "gdm-plot.csv", row.names=FALSE)

# subset only jul/aug data; only use first visit of phenology plots
## summer = jul1 - aug31 (same as dates used to create summer KDEs to calc VI)
gdm.plot.summ <- gdm.plot
gdm.plot.summ$Date <- as.Date(gdm.plot.summ$Date)
gdm.plot.summ <- gdm.plot.summ %>%
  subset(Date >= "2014-07-01" & Date <= "2014-08-31" | 
         Date >= "2015-07-01" & Date <= "2015-08-31") %>%
  mutate(PlotYear = paste(PlotID, ".", year(Date), sep=""))
gdm.plot.summ <- gdm.plot.summ[!duplicated(gdm.plot.summ$PlotYear),] 
write.csv(gdm.plot.summ, file = "gdm-plot-summer.csv", row.names=FALSE)
  
###
# GDM per plot - by lifeform
###

# add to GDM per lifeform per plot
gdm.plot.lifeform <- left_join(gdm.plot.lifeform, plotinfo, by="PlotVisit")
#gdm.plot.lifeform$Area <- with(gdm.plot.lifeform, ifelse(Latitude > 46.157, "Nsapph", "Sbroot")) # designate study area by Latitude (N of 46.157 = Nsapph)

# for plots missing a row for a lifeform, need to add a row with 0 for GDM
gdm.plot.lifeform.temp <- spread(gdm.plot.lifeform, LifeForm, GDM, fill=0) # spread out LifeForm into columns, filling with GDMs, missing data are filled with 0s
gdm.plot.lifeform <- gather(gdm.plot.lifeform.temp, "LifeForm", "GDM", forb:shrub) # gather back together

# using only the first record of revisited phenology plots for each season
# first create dataframe of those with multiple revisits
gdm.plot.lifeform.revisits <- gdm.plot.lifeform %>%
  mutate(year=year(Date)) %>%
  group_by(PlotID, year) %>%
  mutate(n=n()) %>%
  filter(n>3)
# second create dataframe where PlotVisit randomly selected from first dataframe
gdm.plot.lifeform.revisits.keep <- gdm.plot.lifeform.revisits %>%
  #summarise(PlotVisit=first(PlotVisit)) # select first PlotVisit that occurs
  summarise(PlotVisit=nth(PlotVisit, sample(1:max(n), 1))) # randomly select PLotVisit
# then create dataframe of revisits to remove from main data based on above selection
gdm.plot.lifeform.revisits.remove <- anti_join(gdm.plot.lifeform.revisits, gdm.plot.lifeform.revisits.keep, by="PlotVisit")
# and finally remove those revisits
gdm.plot.lifeform <- gdm.plot.lifeform[!(gdm.plot.lifeform$PlotVisit %in% gdm.plot.lifeform.revisits.remove$PlotVisit),]

# subset to desired date range for summer season (July 7 - Aug 31)
gdm.plot.lifeform$Date <- as.POSIXlt(gdm.plot.lifeform$Date, format = "%Y-%m-%d")
# set min/max month and day of interest
min.month <- 7
min.day <- 7
max.month <- 8
max.day <- 31
gdm.plot.lifeform <- gdm.plot.lifeform[(month(gdm.plot.lifeform$Date) >= min.month & day(gdm.plot.lifeform$Date)  >= min.day) &
                                                (month(gdm.plot.lifeform$Date) <= max.month & day(gdm.plot.lifeform$Date) <= max.day),]

gdm.plot.lifeform$Date <- format(gdm.plot.lifeform$Date, "%m/%d/%Y") # format to match broot data
gdm.plot.lifeform$Season <- "Summer"
gdm.plot.lifeform.SUM <- gdm.plot.lifeform


#### GDM for WINTER FORAGE SPECIES ####
# RUN CODE ABOVE AGAIN reading in WINTER FORAGE PLANTS instead (disregarding GDM for SUMMER FORAGE SPECIES section)

# Forage biomass per plot - by species and phenophase

# join with forage biomass by species
gdm.plot.phenospp <- plot.foragespp %>%
  left_join(DMD.spp, by=c("Species" = "PlantCode")) %>%             # join forage biomass per species to DMD per species
  select(-c(EM.dmd, FL.dmd, FR.dmd, MS.dmd)) %>%                    # keeping only cured/senescent DMD
  left_join(DMD.clss, by=c("LifeForm" = "Class")) %>%               # join forage biomass per species to DMD averaged to lifeform
  select(-c(EM.cls.dmd, FL.cls.dmd, FR.cls.dmd, MS.cls.dmd)) %>%    # keeping only cured/senescent DMD
  mutate(WIN.dmd = ifelse(is.na(SE.dmd), SE.cls.dmd, SE.dmd)) %>%   # fill in missing DMD for cured/senescent phenostage with class average DMD values
  mutate(WIN.gdm = Biomass*WIN.dmd)                                 # calculate GDM per species-phenophase
gdm.plot.phenospp <- select(gdm.plot.phenospp, PlotVisit, Species, LifeForm, Biomass, WIN.gdm)

# grams of digestible forage biomass per lifeform
gdm.plot.lifeform <- gdm.plot.phenospp %>%
  group_by(PlotVisit, LifeForm) %>%
  summarise(GDM = round(sum(WIN.gdm),3)) %>%
  ungroup()

# grams of digestible forage biomass per plot
gdm.plot <- gdm.plot.lifeform %>%
  group_by(PlotVisit) %>%
  summarise(GDM = sum(GDM)) %>%
  ungroup()
#write.csv(gdm.plot, file="gdm-plot.csv", row.names=F)

# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel, paste("select PlotID, Date, Latitude, Longitude from PlotInfo"))
#plotinfo <- read.csv("PlotInfo.csv") # if reading from csv
plotinfo <- mutate(plotinfo, PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on
gdm.plot.lifeform <- left_join(gdm.plot.lifeform, plotinfo, by="PlotVisit")

# for plots missing a row for a lifeform, need to add a row with 0 for GDM
gdm.plot.lifeform.temp <- spread(gdm.plot.lifeform, LifeForm, GDM, fill=0) # spread out LifeForm into columns, filling with GDMs, missing data are filled with 0s
gdm.plot.lifeform.temp$shrub <- 0  # add shrub lifeform (bc no shrubs in diet) # do not run this line if shrubs in diet!!
gdm.plot.lifeform <- gather(gdm.plot.lifeform.temp, "LifeForm", "GDM", forb:shrub) # gather back together 

# using only the first record of revisited phenology plots for each season
# first create dataframe of those with multiple revisits
gdm.plot.lifeform.revisits <- gdm.plot.lifeform %>%
  mutate(year=year(Date)) %>%
  group_by(PlotID, year) %>%
  mutate(n=n()) %>%
  filter(n>3)
# second create dataframe where PlotVisit randomly selected from first dataframe
gdm.plot.lifeform.revisits.keep <- gdm.plot.lifeform.revisits %>%
  #summarise(PlotVisit=first(PlotVisit)) # select first PlotVisit that occurs
  summarise(PlotVisit=nth(PlotVisit, sample(1:max(n), 1))) # randomly select PLotVisit
# then create dataframe of revisits to remove from main data based on above selection
gdm.plot.lifeform.revisits.remove <- anti_join(gdm.plot.lifeform.revisits, gdm.plot.lifeform.revisits.keep, by="PlotVisit")
# and finally remove those revisits
gdm.plot.lifeform <- gdm.plot.lifeform[!(gdm.plot.lifeform$PlotVisit %in% gdm.plot.lifeform.revisits.remove$PlotVisit),]

# add some stuff
gdm.plot.lifeform$Area <- ifelse(gdm.plot.lifeform$Latitude > 46.157, "Nsapph", "Sbroot") # designate study area by Latitude (N of 46.157 = Nsapph)
gdm.plot.lifeform$Season <- "Winter"
gdm.plot.lifeform$Date <- format(gdm.plot.lifeform$Date, "%m/%d/%Y") # format to match broot data

gdm.plot.lifeform.WIN <- gdm.plot.lifeform


#### Join & Export SUMMER & WINTER GDM data####
gdm.data <- bind_rows(gdm.plot.lifeform.SUM, gdm.plot.lifeform.WIN)
gdm.data <- select(gdm.data, PlotID, Date, Longitude, Latitude, GDM, LifeForm, Area, Season) # reorganizing

write.csv(gdm.data, file = "sapp_GDM_data.csv", row.names = FALSE)

odbcCloseAll() # Closes connections to databases





