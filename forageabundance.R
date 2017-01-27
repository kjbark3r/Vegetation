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
library(rgdal)

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
#spp <- dplyr::select(spp, PlantCode, LifeForm, NameScientific) # if reading from csv
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
  dplyr::select(QuadratVisit, PlotVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) 
quadrat$ForbWt[is.na(quadrat$ForbWt)] <- 0 #replace NA with 0 
quadrat$GrassWt[is.na(quadrat$GrassWt)] <- 0  

# herbaceous biomass per quadrat - by species
quadrat.herb <- left_join(cover, classn, by = "QuadratVisit") %>%
  dplyr::select(-PlotVisit) #avoid duplicated column name after next join
quadrat.herb$RescaledCover <- ifelse(quadrat.herb$LifeForm == "forb", quadrat.herb$Total/quadrat.herb$ForbCov,
                                      ifelse(quadrat.herb$LifeForm == "graminoid", 
                                             quadrat.herb$Total/quadrat.herb$GrassCov, 
                                             ifelse(NA)))
quadrat.herb <- left_join(quadrat.herb, quadrat, by = "QuadratVisit")
quadrat.herb <- select(quadrat.herb, c(PlotVisit, QuadratVisit, Species, Genus, 
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
  dplyr::select(-c(Genus, Genus2)) %>%
  ungroup()
plot.herb.species$LifeForm <- ifelse(grepl(' GRASS| JUNCACEAE|CARE ', plot.herb.species$Species), "graminoid", 
                             ifelse(grepl('UNK ', plot.herb.species$Species), "forb", plot.herb.species$LifeForm))

# herbaceous forage biomass per plot - by species
forage.herb <- plot.herb.species %>%
  dplyr::select(-NameScientific) %>%
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
  dplyr::select(-c(Genus, LifeForm)) %>% #avoid duplicated columns after join
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
quadrat.shrub <- dplyr::select(quadrat.shrub, c(Date, PlotID, PlotM, Species, PlotVisit, 
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

# all biomass, by species
biomass.species <- plot.herb.species %>%
  dplyr::select(PlotVisit, Species, Biomass) %>%
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

# forage biomass by plot
plot.forage <- plot.foragespp %>%
  group_by(PlotVisit) %>%
  summarise(Biomass = sum(Biomass))

###########################################################
## fix up and export data ##


# add Lat/Long from PlotInfo
plotinfo <- sqlQuery(channel, paste("select PlotID, Date, Latitude, Longitude from PlotInfo"))
#plotinfo <- read.csv("PlotInfo.csv") # if reading from csv
plotinfo <- mutate(plotinfo, PlotVisit = paste(PlotID, ".", Date, sep="")) # create PlotVisit column to join on

# add to biomass per plot
b.plot <- biomass.lifeform %>% 
  select(-c(PlotID, Date)) %>% #avoid duplicate columns
  left_join(plotinfo, by="PlotVisit") 
write.csv(b.plot, file = "biomass-plot-allvisits.csv", row.names = FALSE)

# subset only jul/aug data; only use first visit of phenology plots
## summer = jul1 - aug31 (same as dates used to create summer KDEs to calc VI)
b.plot.summ <- b.plot
b.plot.summ$Date <- as.Date(b.plot.summ$Date)
b.plot.summ <- b.plot.summ %>%
  subset(Date >= "2014-07-01" & Date <= "2014-08-31" | 
         Date >= "2015-07-01" & Date <= "2015-08-31") %>%
  mutate(PlotYear = paste(PlotID, ".", format(Date, '%Y'), sep=""))
b.plot.summ <- b.plot.summ[!duplicated(b.plot.summ$PlotYear),] 
write.csv(b.plot.summ, file = "biomass-plot-summeronly.csv", row.names=FALSE)

# export abundance data as shapefile
#BUT writeOGR requires 64-bit, of course
#so change from 32 to 64, reopen rstudio, and re-read in the data
#because R hates you
b.plot.summ <- read.csv("biomass-plot-summeronly.csv")
latlong = CRS("+init=epsg:4326") # define projection
xy <- data.frame("x"=b.plot.summ$Longitude,"y"=b.plot.summ$Latitude)
ll <- SpatialPointsDataFrame(xy, b.plot.summ, proj4string = latlong)
writeOGR(ll, dsn = ".", layer="biomass-plot-summer", driver="ESRI Shapefile",
           overwrite_layer = TRUE)

odbcCloseAll() # Closes connections to databases
