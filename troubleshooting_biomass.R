##########################################################
#######  MISC CODE AND TROUBLESHOOTING RELATED TO  #######
#### HERBACEOUS BIOMASS ESTIMATION - NSERP STUDY AREA ####
################## KJB  July 2016  #######################
##########################################################

## WD
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
	rm(wd_workcomp, wd_laptop)


## FIGURING OUT ACCESS CONNECTION STUFF

library(RODBC)
library(dplyr)

############################################
## RUN CODE BELOW THIS LINE PIECEMEAL
############################################

####################	
# goal 1: pull classification info

channel <- odbcConnectAccess("C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology_2015-10-16Kelly")
  #eff, need 32-bit Windows or R, or try different function. Diff fcn first

	#diff fcn in 64 bit R
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology_2015-10-16Kelly.accdb")
  #nope. switching to 32 bit R
  #EEEEEE it worked!

class <- sqlQuery(channel, paste("select * from Classification"))

# goal 2: create unique quadrat IDs

class %>%
  rename(PlotID = Plot ID)
  #ugh, it's confused by spaces. Rename instead.

class <- make.names(class)
# only works for vectors; would have to do manually either way

colnames(class) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")

class <- class %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM))
#works but has space between "-" and each number

class <- class %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep=""))
#perf


# goal 3: add plant type
## can pull this from NSERP_SP_list table - PlantCode and LifeForm

# figuring out whay just some of them didn't work
class[class$LifeForm %in% NA,]
#i think they have spaces in the names

class$PlantCode <- make.names(class$PlantCode)
#nope, makes spaces in to periods.

class$PlantCode <- gsub(" ", "", class$PlantCode, fixed = TRUE)
#this does work, but you didn't actually want it to remove spaces
  #from SP entries. need to write out exceptions.

#what this needs to do:
# remove typo'd spaces
  #but not the spaces from
    # XXXX SP
    # UNK X
# then pull LifeForm based on PlantCode
  #but not for
    # XXXX SP
      #for these base it on the first 4 letters
    # UNK X  
      #for these base it on the last letters (after UNK)

# doing it without changing anything first to see what names are left
class <- left_join(class, spp, by = "PlantCode")
class[class$LifeForm %in% NA,]
  #write exceptions based on NAs

#1. identify values that include "UNK"
  #does dplyr select work?
  a <- select(class$PlantCode, contains("UNK"))
  #not so much

  # %in% ?
  class[class$PlantCode %in% "UNK"*,]
  #tried a few variations and can't get it to work

  #how bout the grep thing?
  grep('UNK ', class$PlantCode, value = TRUE)
  #nice
  
#2. label LifeForm for UNKs
  if(grep('UNK ', class$PlantCode, value = TRUE)){
    class$LifeForm <- "forb"
  }
  #this doesn't work - need a y/n answer to if
  #try grepl - returns "logical vector"

  if(grepl('UNK ', class$PlantCode)){
    class$LifeForm <- "forb"
  }
  #nope - need ifelse if using grepl 
  #bc ifelse is vectorized
  
  ifelse(grepl('UNK ', class$PlantCode), "forb", )
  #close. have to have "no" argument; can't leave it blank
  
  class$LifeForm <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           class$LifeForm)
  #worked for the UNKs but screwed up the others
  
  for (i in 1:nrow(class)){
    class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           class$LifeForm[i])
  }
  #effs them all up (makes all NA)
  
    for (i in 1:nrow(class)){
    class$LifeForm[i,] <- ifelse(grepl('UNK ', class$PlantCode[i,]), "forb", 
                           class$LifeForm[i,])
  }
  #error
  
  for (i in 1:nrow(class)){
  class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode[i]), "forb", 
                         class$LifeForm[i])
  }
  #warning, i made all your data into NAs, hope that's cool. love, r

  for (i in 1:nrow(class)){
  class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm[i])
  }
  #ditto above

  class$LifeForm[1:length(class)] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm)  
  #2 warnings, but didn't make all NAs (just did nothing)
  
  class$LifeForm[1:nrow(class)] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm)  
  #warning, invalid factor level, made all NAs

    class$LifeForm <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           next)
    #error, you didn't actually make this a for loop, duh
    
  for(i in 1:nrow(class)) {
    class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode[i]), "forb", 
                           next)
  }
    #geez, took ya long enough

## REMOVING LEADING/TRAILING SPACES FROM PLANT CODES (TYPOS)

    class$PlantCode <- gsub(" ", "", class$PlantCode, fixed = TRUE)
    #removes all spaces (incl SP or UNK entries), not just leading/trailing
  
    #trying to use Trim function from Access
    #Access code would be::: TrimmedName: Trim([CategoryName])
    
    class$PlantCode <- sqlQuery(channel, paste("Trim([PlantCode])"))
    #whoa, makes PlantCode into super weird stuff
    
    class$PlantCode <- sqlQuery(channel, paste("PlantCode: Trim([PlantCode])"))
    #ditto    
    
    class$PlantCode <- sqlQuery(channel, paste("PlantCode2: Trim([PlantCode])"))
    #samesies
    
    class$PlantCode <- sqlQuery(channel, paste("Expr1: Trim([PlantCode])"))
    #yup...
    
    class$PlantCode <- sqlQuery(channel, paste("Trim([PlantCode])"))
    #stillllll
    
    #oh my god this is a thing?
    trimws(class$PlantCode)

    
### RESCALE SPECIES % COVER (MAKE % OF LIFE FORM IN QUADRAT)
    
    # % total forb cover in quadrat: from Cover table
    cover <- sqlQuery(channel, paste("select * from Cover"))
    
      #remove spaces from column names
    cover <- make.names(cover[,])
      #eh screw it, there are also dashes and slashes and ugh. GOing manual.
  
#make unique quadrat-visit identifier
    test <- class %>%
      mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))
    
    testcov <- cover %>%
      mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))
    
    visit <- unique(test$QuadratVisit)
    
    bignasty <- left_join(test, testcov, by = "QuadratVisit")
    
    bignasty$RescaledCover <- ifelse(bignasty$LifeForm == "forb", bignasty$Total/bignasty$Forb,
                                      ifelse(bignasty$LifeForm == "graminoid", bignasty$Total/bignasty$Grass,
                                             NA)
                               )
    

## ARE SUB-SHRUBS INCLUDED IN PHENOLOGY PLOTS?
    ## no
spp[spp$LifeForm %in% "sub-shrub",]
cover[cover$LifeForm %in% "subshrub",]
class[class$LifeForm %in% "subshrub",]
class[class$Species %in% "ARCUVA",]
class[class$Species %in% "BALSAG",]  

## need for loop here? (code cleanup)

hm <- select(class, -LifeForm)

	for(i in 1:nrow(hm)) {
	  hm$LifeForm[i] <- ifelse(grepl('UNK ', hm$Species[i]), "forb", 
								ifelse(hm$LifeForm == "sub-shrub", "forb", next))
	}

  #OR

bignasty$RescaledCover <- ifelse(bignasty$LifeForm == "forb", bignasty$Total/bignasty$Forb,
                                      ifelse(bignasty$LifeForm == "graminoid", bignasty$Total/bignasty$Grass,
                                             NA)
                               )
#############
## ACCESS CONNECTION ON LAPTOP

if(file.exists("C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")){
  cat("Yay!")} else {
    cat("BOO")}
#ok, no problem finding the file. something about the driver/connection.

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")

#############
## make separate columns for grass dry weight and forb dry wt

drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) 

drywt <- rename(drywt, ForbWt = Forb, GrassWt = Grass)

#############
## scale spp-specific biomass to plot level

biomass <- drywt %>%
  group_by(PlotVisit, Species) %>%
  mutate(g0.75m = sum(grams))
##only pulled some values, lots of NAs, not sure what happened

biomass <- drywt %>%
  group_by(PlotVisit, Species) %>%
  summarise(g0.75m = sum(grams))
##cleaner result but still not working for everything

biomass <- drywt %>%
  group_by(Species, PlotVisit) %>%
  summarise(g0.75m = sum(grams))
##similar to above - not sure why some work and others not
##setup is good; numbers are wrong

sum(drywt$grams[1:3])
#works as expected

test <- drywt %>%
  group_by(PlotVisit) %>%
  summarise(totalg = sum(grams))
##gives all NAs

test <- group_by(drywt, PlotVisit)
summarise(test, sum(grams))
##whyyyyy?
##grams are numeric...

test <- group_by(drywt, PlotVisit)
test$totalg <- mean(test$grams)
##same problem. so not an issue with summarise or sum
##maybe bc NAs? 

test <- drywt
test$grams[is.na(test$grams)] <- 0
biomass <- test %>%
  group_by(PlotVisit, Species) %>%
  mutate(g0.75m = sum(grams))
##i think this worked but didn't collapse data by plot visit
##bc somebody forgot to summarise, not mutate...

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  summarise(g0.75m = sum(grams))
##hooray
rm(test)

#now same as above but keep LifeForm data  

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  filter(g0.75m == sum(grams))
#Rsplosion

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit) %>%
  filter(g0.75m == sum(grams))
#ditto above
#filter prob doesn't make new columns

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  filter(grams == sum(grams))
#well... it functionally did something...
#(something totally wrong)

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit) %>%
  filter(grams == sum(grams))
#this one's even worse
#screw it, i'll just re-join lifeform
rm(test)

#####
#sum biomass per plot visit; add PlotID and Date (for ease of use later); export
all.herb.biomass <- biomass %>%
  group_by(PlotVisit) %>%
  summarise(Biomass = sum(g1m)) %>%
  mutate(PlotID = substr(PlotVisit, 1, 3)) %>%
  mutate(Date = substr(PlotVisit, 5, 14)) %>%
  select(PlotID, Date, PlotVisit, Biomass)
#wrong numbers - not sure why
write.csv(all.herb.biomass, file = "herbaceousbiomass.csv", row.names = FALSE)

##next steps:
#look for NAs in base data
  #found some rows all NA except QuadratVisit in drywt df
spp[spp$Species %in% NA,]
cover[cover$Species %in% NA,]
classn[classn$Species %in% NA,]
clip[clip$PlotVisit %in% NA,]
drywt[drywt$Species %in% NA,]
  #only NAs are in here - all are plot 325, visit 2014-06-18
  #also missing some forb and grass weights in here
  
#investigating drywt step by step
drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt)
  #issues already... 4 drywts are 9999.00

#determining whether those 9999s are the only thing screwing up my #s
biomass[biomass$PlotVisit == "681.2014-09-04" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "344.2014-09-03" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "340.2015-05-27" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "344.2015-07-06" & biomass$LifeForm == "graminoid",]

#checking for incorrect NAs at every step
#cuz they sure as shit show up at some point
any(is.na(cover$ForbCov)); any(is.na(cover$GrassCov))
  #nope, cover df is good
any(is.na(drywt$DryWt))
  #none here until spread()
  #those NAs are valid - but should prob be 0s
    #for the purposes of this analysis
any(is.na(drywt$ForbWt)); any(is.na(drywt$GrassWt))
drywt[is.na(drywt$ForbWt),]
  #NAs show back up when add cover to df
any(is.na(cover$RescaledCover))
  #ah ha
a <- cover[is.na(cover$RescaledCover),]
unique(a$LifeForm)
  #makes sense; happens for shrubs
any(is.na(drywt$grams))
  #whyyyy

########
## making sure cover df and drywt df have same quadratvisits
(wtf <- setdiff(cover$QuadratVisit, drywt$QuadratVisit))
  #nope. . .

########
## check ALL the NAs!

#sppcover
any(is.na(sppcover[,1])); any(is.na(sppcover[,2])); any(is.na(sppcover[,3]))
any(is.na(sppcover[,4])); any(is.na(sppcover[,5])); any(is.na(sppcover[,6]))
any(is.na(sppcover[,7]))

#drywt
any(is.na(drywt[,1])); any(is.na(drywt[,2])); any(is.na(drywt[,3]))
any(is.na(drywt[,4])); any(is.na(drywt[,5])); any(is.na(drywt[,6]))
any(is.na(drywt[,7])); any(is.na(drywt[,8])); any(is.na(drywt[,9]))

########
## comparing plot-level herbaceous vs forage-only biomass

hm <- biomass.plot %>%
  full_join(biomass.forage, by = "PlotVisit") %>%
  rename(gForage = grams) %>%
  mutate(diff = gHerb-gForage)
#hm... 4 plots have higher forage biomass than herbaceous
#and those aren't plots we have weird data for (afaik)
#which means i effed something up

#things to check:
## calculate herbaceous biomass by summing species data
    ## instead of using the general forb/graminoid data
test.biomass.plot <- biomass.spp %>%
  inner_join(drywt, by = "QuadratVisit") 
test.biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333) #biomass to plot-level, g/m^2

biomass.forage <- biomass.spp 
biomass.forage$NameScientific <- as.character(biomass.forage$NameScientific) #add genus
biomass.forage$Genus <- sapply(strsplit(biomass.forage$NameScientific, " "), "[", 1)
biomass.forage <- semi_join(biomass.forage, forage, by = "Genus") #forage plants only
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)


#per plot - biomass, all herbaceous (by life form)
a.lifeform <- summarise(group_by(quadrat, PlotVisit), sum(AllHerbWt)*1.33333)
colnames(plot) <- c("PlotVisit", "SumHerb")
  #SAME THING BUT USING SPECIES WEIGHTS (to check numbers)
a.spp <- summarise(group_by(quadrat.spp, PlotVisit), sum(ClipGrams)*1.3333)
colnames(plot.test) <- c("PlotVisit", "SumSpp")
  #EFF.
hm <- full_join(plot, plot.test, by = "PlotVisit")
  hm$diff <- hm[,2]-hm[,3]

  #####ugh ok let's start at the beginning here
#base data
  # number of plots
length(unique(classn$PlotID))
length(unique(clip$PlotID))
length(unique(cover$PlotID))
  #all = 12
length(unique(classn$PlotVisit)); length(unique(clip$PlotVisit))
  #both 109 (PlotVisit not recorded in cover)
length(unique(classn$QuadratVisit)); length(unique(clip$QuadratVisit)); length(unique(cover$QuadratVisit))
  #545 quadrat visits; 325 clip plot visits
#worked up data
length(unique(quadrat$PlotID)); length(unique(quadrat$PlotVisit)); length(unique(quadrat$QuadratVisit))
length(unique(quadrat.spp$PlotID)); length(unique(quadrat.spp$PlotVisit)); length(unique(quadrat.spp$QuadratVisit))

#try without converting first
#per plot - biomass, all herbaceous (by life form)
a.lifeform <- summarise(group_by(quadrat, PlotVisit), sum(AllHerbWt))
colnames(a.lifeform) <- c("PlotVisit", "SumHerb")
  #SAME THING BUT USING SPECIES WEIGHTS (to check numbers)
a.spp <- summarise(group_by(quadrat.spp, PlotVisit), sum(ClipGrams))
colnames(a.spp) <- c("PlotVisit", "SumSpp")
a <- full_join(a.lifeform, a.spp, by = "PlotVisit")
a$diff <- round(a[,2]-a[,3])

#ok after drilling in to the database and investigating manually in excel
#i think some of the forb/grass cover sums are recorded incorrectly
#e.g. 322.2014-06-30.20 recorded grass cover is 6, but 
    #indiv spp data in classification data adds to 8
  #i am full of hatred for whoever has made these eggregious errors


###########averaging biomass
#per plot: herbaceous and forage biomass
herb <- quadrat %>%
  group_by(QuadratVisit) %>%
  summarise(PlotVisit, 
            ForbWt = sum(ForbWt)*1.33333333,
            GrassWt = sum(GrassWt)*1.33333333,
            HerbWt = sum(AllHerbWt)*1.33333333) 
  
  group_by(PlotVisit) %>%
  summarise(HerbBiomass = mean(AllHerbWt))

forage <- quadrat.spp %>%
  semi_join(foragespp, by = "Genus") %>%
  group_by(PlotVisit) %>%
  summarise(ForageBiomass = sum(ClipGrams))

forage[forage$QuadratVisit %in% "344.2014-06-16.20","ForageBiomass"] 


forage2[forage2$QuadratVisit %in% "344.2014-06-16.20","ForageBiomass"] 

###############
## srsly why don't my forage biomass numbers make sense?
##############

#filter so you're only using forage species (genuses)
#sum forb and grass clip grams for each quadrat
#average the above sum and multiply by 1.3333 to get g/m^2 of forage forbs and grasses
#sum forbs and grasses for total

#start with quadrat.spp
test.inner <- quadrat.spp %>%
  inner_join(foragespp, by = "Genus")
(a <- unique(test$Genus))
(b <- unique(foragespp$Genus))
#n=2403, yes, this pulls only genuses included in the forage genus list

test.semi <- quadrat.spp %>%
  semi_join(foragespp, by = "Genus")
#n=1654, this also only pulls forage genuses but pulls fewer
#inner duplicates some. you were definitely right to do the semi
rm(test.inner, test.semi)

#start with quadrat.spp
test <- quadrat.spp %>%
  #filter so you're only using forage species (genuses)
  semi_join(foragespp, by = "Genus") %>%
  #sum forb and grass clip grams for each quadrat
  group_by(QuadratVisit, LifeForm) %>%
  summarise(ForageGrams = sum(ClipGrams)) %>%
    #verified this works
  #average the above sum and multiply by 1.3333 to get g/m^2 of forage forbs and grasses
  spread(LifeForm, ForageGrams) %>% #0s have that lifeform in plot but not in clip plot. NAs have no lifeform
  rename(ForageForbG = forb, ForageGrassG = graminoid) %>%
  mutate(PlotVisit = substr(QuadratVisit, 1, 14))
test$ForageForbG[is.na(test$ForageForbG)] <- 0
test$ForageGrassG[is.na(test$ForageGrassG)] <- 0

test <- test %>%
  ungroup() %>%
  group_by(PlotVisit) %>%
  summarise(ForageForbBiomass = mean(ForageForbG)*1.33333, ForageGrassBiomass = mean(ForageGrassG)*1.33333) 
test$ForageBiomass <- test$ForageForbBiomass + test$ForageGrassBiomass
testbiomass <- full_join(herb, test, by = "PlotVisit")
testbiomass$GrassDiff <- testbiomass$GrassBiomass - testbiomass$ForageGrassBiomass
testbiomass$ForbDiff <- testbiomass$ForbBiomass - testbiomass$ForageForbBiomass
testbiomass$BiomassDiff <- testbiomass$HerbBiomass - testbiomass$ForageBiomass
testbiomass <- biomass

#looks OK (yay!) except for 364.2014-06-16 - forage values are higher than all herbaceous values
classn[classn$PlotVisit %in% "364.2014-06-16",] 
clip[clip$PlotVisit %in% "364.2014-06-16",] 
#looked at cover
quadrat.spp[quadrat.spp$PlotVisit %in% "364.2014-06-16",]
quadrat[quadrat$PlotVisit %in% "364.2014-06-16",]
sum(quadrat.spp[quadrat.spp$PlotVisit %in% "364.2014-06-16","ClipGrams"])


a <- as.data.frame(quadrat.spp[quadrat.spp$PlotVisit %in% "364.2014-06-16",])
#problem is for plots that (should) have 0 for both forb and grass forage biomass
#the zeros aren't included when it averages out to whole plot
#because there's no row for those plots
##so instead of filtering only forage species, need to add an indicator,
##make ForageGrams column, and put 0 for non-forage species

forage <- foragespp %>%
  select(Genus, CumAve) %>%
  transmute(Genus, ForagePlant = ifelse(is.na(CumAve), "No", "Yes")) %>%
  right_join(quadrat.spp, by = "Genus") 
forage$ForagePlant <- ifelse(is.na(forage$ForagePlant), "No", "Yes")
forage$ForageGrams <- ifelse(forage$ForagePlant == "Yes", forage$ClipGrams, 0)
forage <- forage[!duplicated(forage),]
forage <- forage %>%
  group_by(QuadratVisit, LifeForm) %>%
  summarise(ForageG = sum(ForageGrams)) %>%
  spread(LifeForm, ForageG) %>% #0s have lifeform in plot but not clip plot. NAs don't have lifeform
  rename(ForageForbG = forb, ForageGrassG = graminoid) %>%
  mutate(PlotVisit = substr(QuadratVisit, 1, 14))
  forage$ForageForbG[is.na(forage$ForageForbG)] <- 0
  forage$ForageGrassG[is.na(forage$ForageGrassG)] <- 0


foragebiomass <- biomass
foragebiomass$GrassDiff <- foragebiomass$GrassBiomass - foragebiomass$ForageGrassBiomass
foragebiomass$ForbDiff <- foragebiomass$ForbBiomass - foragebiomass$ForageForbBiomass
foragebiomass$BiomassDiff <- foragebiomass$HerbBiomass - foragebiomass$ForageHerbBiomass  

###############
## figuring out if phen and biomass plots all have different IDs
##############

library(RODBC)
library(dplyr)

#phen
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")
phen <- sqlQuery(channel, paste("select * from Classification"))
colnames(phen) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
phenplots <- as.data.frame(unique(phen$PlotID))
colnames(phenplots) <- "PlotID"


#bio
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Database_2016-06-28.accdb")
bio <-  sqlQuery(channel, paste("select * from Classification")) 
colnames(bio) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
bioplots <- as.data.frame(unique(bio$PlotID))
colnames(bioplots) <- "PlotID"
bioplots

#both
plots <- anti_join(phenplots, bioplots, by = "PlotID") %>% select(PlotID)
plots <- semi_join(phenplots, bioplots, by = "PlotID") %>% select(PlotID)

#yeah a bunch of them do... greeeeat

###############
## pulling plotvisit from quadrat visit using regex
##############

qv <- as.character("10.2014-08-29.0")

#any numbers before the 1st period = PlotID
  #sub() matches first instance only?
id <- sub("[0-9]\\.", "", qv)
  #nope
id2 <- sapply(strsplit(qv, "\\."), `[[`, 1)
  #fuck yeah, thanks stackoverflow
ohplz <- sapply(strsplit(forage$QuadratVisit, "\\."), `[[`, 1)
unique(ohplz)
  #make column using dplyr?
ftest <- forage %>%
  mutate(PlotID = sapply(strsplit(QuadratVisit, "\\."), `[[`, 1))
  #wooooot

#all characters before the last (second) period = PlotVisit
ohplz <- sub(".*\\.", "",qv)
hm <- sub("(.*)[.].*", "\\1", qv)
  #yesss this one

###############
## checking out diff in biomass 
## bt jesse's and my forage def
##############

check <- plot.foragespp %>%
  ungroup() %>%
  group_by(PlotVisit) %>%
  summarise(biomassJ = sum(biomass)) %>%
  ungroup() %>%
  full_join(biomass.herb, by = "PlotVisit") %>%
  select(PlotVisit, biomassJ, HerbBiomass) %>%
  rename(biomassK = HerbBiomass)

a <- as.data.frame(unique(quad.foragespp$Species))

###############
## forage shrub biomass = regular shrub biomass
## right, or wrong?
###############

test <- bind_cols(shrub, forage.shrub)
test$diff <- test$ShrubBiomass-test$ForageShrubBiomass

#compare forage shrub spp to shrub spp recorded in classn
atest <- classn %>% 
  filter(LifeForm =="shrub")
a <- as.data.frame(unique(atest$Species))

btest <- spp.forage %>%
  filter(LifeForm == "shrub")
b <- as.data.frame(unique(btest$Species))

ctest <- quadrat.shrub
c <- as.data.frame(unique(ctest$Species))

#shit. pretty sure that's super wrong...

###############
## defining forage
## keeping [shrub] sp *stem/leaf*
###############

#  prob should fix it early, in here
forage <- read.csv("NS_foragespecies_summer.csv") %>%
  filter(cumave < 95) %>%
  mutate(Genus2 = trimws(SpeciesName)) %>% # for defining forage later
  rename("Species"=SpeciesName)

test <- forage
test$Genus2 <- ifelse(grepl(" leaf| stem", test$Genus2), "NiceWork", forage$Genus2)
# k works, now just need to figure out how to tell it to remove those characters

test$Genus2 <-gsub(' leaf| stem', '', test$Genus2) 

test$Genus2 <- ifelse(grepl(" leaf| stem", test$Genus2), "NiceWork", forage$Genus2)

#########################
## NUTRITION-RELATED CODE ####
##########################

#################
# figuring out how to deal with all these freaking 0s

table(data$GDMforb > 0)
  # more non-zeros than zeros (~2x), i THINK this  
  # lends support to the hurdle model plan
hist(data$GDMforb, breaks = 200)
test <- filter(data, GDMforb>0)
hist(log10(test$GDMforb))
summary(test$GDMforb)


# statsy info
var(test$GDMforb); mean(test$GDMforb)
# omg this is like so overdispersed
  # so negative binomial?

#################
# add NDVI tiff to GDM model
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")
ndvi <- raster("./ndvi/kristinExport20140218.tif")
ndvi_stateplane <- projectRaster(ndvi, crs = stateplane)

########
#actually, just fix those two plots that had lat/long typos
#for efficiency/sanity purposes
  #PlotVisit == "727.2015-08-12"; PlotVisit == "1105.2015-08-11"

upd.ndvi <- raster("./ndvi/kristinExport20150813.tif")

upd.rmt <- read.csv("gdm-plot-summer.csv") %>%
  filter(PlotVisit == "727.2015-08-12" | PlotVisit == "1105.2015-08-11") %>%
  dplyr::select(c(Longitude, Latitude))

upd.ext <- extract(upd.ndvi, upd.rmt) 

upd.plots <- read.csv("upd-remote-plot.csv") %>%
  select(-EVI)
upd.plots$NDVI <- ifelse(upd.plots$PlotVisit == "1105.2015-08-11", upd.ext[1],
                         ifelse(upd.plots$PlotVisit == "727.2015-08-12", upd.ext[2],
                                upd.plots$NDVI))
write.csv(upd.plots, file = "ndvi-plot.csv", row.names=F)

###
# plotting relationships
par(mfrow = c(1,1))
plot(log(GDM) ~ cti, data = dat.GDM)
plot(log(GDM) ~ elev, data = dat.GDM)
plot(log(GDM) ~ slope, data = dat.GDM)
plot(log(GDM) ~ hillshade, data = dat.GDM)
plot(log(GDM) ~ ndvi_dur, data = dat.GDM)
plot(log(GDM) ~ ndvi_ti, data = dat.GDM)
plot(log(GDM) ~ ndvi, data = dat.GDM)
plot(log(GDM) ~ sum_precip, data = dat.GDM)

par(mfrow = c(1,1))
plot(log(GDM) ~ cti_std, data = dat.GDM)
plot(log(GDM) ~ elev_std, data = dat.GDM)
plot(log(GDM) ~ slope_std, data = dat.GDM)
plot(log(GDM) ~ hillshade_std, data = dat.GDM)
plot(log(GDM) ~ ndvi_dur_std, data = dat.GDM)
plot(log(GDM) ~ ndvi_ti_std, data = dat.GDM)
plot(log(GDM) ~ ndvi_std, data = dat.GDM)
plot(log(GDM) ~ sum_precip_std, data = dat.GDM)

# trying my hand at testing all possible models instead of backwards step
  # in case there's similar support for a simpler one
  # and just for funzies to learn the diffs bt methods
library(leaps)
leaps<-regsubsets(log(GDM) ~ cover_class + cti_std + elev_std + hillshade_std + ndvi_dur_std + ndvi_ti_std + sum_precip_std + slope_std + ndvi_std,data=dat.GDM,nbest=3)
# attempting to figure out what the heck this tells me
# goal: find which models have best adjusted r-squared
a <- summary(leaps)
names(a)
eff <- matrix(nrow = 24, ncol = 2)
a
# ok seems like each cover_class is treated separately, so i'm
# confused about how to interpret
(summary(leaps) $adjr2)
(arrange(desc(a)))
summary(a$rsq)
plot(leaps, scale = "r2")

#########################
## NDVI RASTER STUFF ####
##########################

#STEPS#
#X pull in NDVI tifs from mid-Jul - Aug as rasters
#X calculate average of the rasters
  #X for 2014 and 2015
#X figure out boundbox of NDVI
ndvipath <- paste(getwd(), "/ndvi", sep="")
tifs14 <- list.files(ndvipath, pattern = "20140728|20140813|20140829")
tifs15 <- list.files(ndvipath, pattern = "20150728|20150813|20150829")
stack14 <- stack(paste(ndvipath, "/", tifs14, sep=""))
stack15 <- stack(paste(ndvipath, "/", tifs15, sep=""))
ndvi14 <- projectRaster(mean(stack14), crs = esp6_15_orig@crs)
ndvi15 <- projectRaster(mean(stack15), crs = esp6_15_orig@crs)
ndvi14@extent; ndvi14@crs; ndvi15$extent

#projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

esp6_15_orig <- raster("writtenrasters\\esp6_15.tif") 
esp6_15_orig@extent; esp6_15_orig@crs

# re-clip/whatever-else all other rasters accordingly (xmin, xmax, ymin, ymax)
boundbox <- extent(150000, 350000, 110000, 350000) ##Entire study area
esp6_15 <- crop(esp6_15_orig, boundbox)				##Crop to boundbox extent
esp6_14 <- crop(esp6_14_orig, boundbox)	
t_fire_15 <- crop(t_fire_15_orig, boundbox)
t_fire_15 <- resample(t_fire_15, esp6_15, "ngb") 
t_fire_14 <- crop(t_fire_14_orig, boundbox)
t_fire_14 <- resample(t_fire_14, esp6_15, "ngb")
elev<- crop(elev_orig, boundbox)   ##if extent and resolution different, set extent and resolution in order to stack rasters 
elev<- resample(elev, esp6_15, "bilinear")  # resample down to esp level
slope<- crop(slope_orig, boundbox)
slope<- resample(slope, esp6_15, "bilinear")
cc<- crop(cc_orig, boundbox)
cc<- resample(cc, esp6_15, "bilinear")
precip_2014<- crop(precip_2014_orig, boundbox)
precip_2014<- resample(precip_2014, esp6_15, "bilinear")
precip_2015<- crop(precip_2015_orig, boundbox)
precip_2015<- resample(precip_2015, esp6_15, "bilinear")
spr_precip_2014<- crop(spr_precip_2014_orig, boundbox)
spr_precip_2014<- resample(spr_precip_2014, esp6_15, "bilinear")
spr_precip_2015<- crop(spr_precip_2015_orig, boundbox)
spr_precip_2015<- resample(spr_precip_2015, esp6_15, "bilinear")
hillshade<- crop(hillshade_orig, boundbox)
hillshade<- resample(hillshade, esp6_15, "bilinear")
cti<- crop(cti_orig, boundbox)
cti<- resample(cti, esp6_15, "bilinear")
ndvi_ti_2014<- crop(ndvi_ti_2014_orig, boundbox)
ndvi_ti_2014<- calc(ndvi_ti_2014, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_ti_2014<- resample(ndvi_ti_2014, esp6_15, "bilinear")
ndvi_ti_2014<- focal(ndvi_ti_2014, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) 
ndvi_ti_2015<- crop(ndvi_ti_2015_orig, boundbox)
ndvi_ti_2015<- calc(ndvi_ti_2015, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_ti_2015<- resample(ndvi_ti_2015, esp6_15, "bilinear")
ndvi_ti_2015<- focal(ndvi_ti_2015, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) 
ndvi_amp_2014<- crop(ndvi_amp_2014_orig, boundbox)
ndvi_amp_2014<- calc(ndvi_amp_2014, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_amp_2014<- resample(ndvi_amp_2014, esp6_15, "bilinear") 
ndvi_amp_2014<- focal(ndvi_amp_2014, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) # only for NA's, replace with focal mean of 11x11
ndvi_amp_2015<- crop(ndvi_amp_2015_orig, boundbox)
ndvi_amp_2015<- calc(ndvi_amp_2015, fun=function(x) { ifelse(x==0 | x==255, NA, x)}) 
ndvi_amp_2015<- resample(ndvi_amp_2015, esp6_15, "bilinear") 
ndvi_amp_2015<- focal(ndvi_amp_2015, w=matrix(1,11,11), fun=mean, na.rm=TRUE, NAonly=TRUE) # only for NA's, replace with focal mean of 11x11
gsri<- crop(gsri_orig, boundbox)
gsri<- resample(gsri, esp6_15, "bilinear") 

# stack em
# use stack to predict GDM across Sapph in 2014 and 2015
  # may need 2 stacks for this i guess
# try not to dry drown

#########################
## COVER CLASS RASTER STUFF ####
##########################

# testing predict on 2014 data in kinda hacky way
rast_14 <- list.files(path=paste(wd, "writtenrasters/covs2014", sep="/"), pattern="tif$", full.names=TRUE) 
s14 <- stack(rast_14) 
names(s14)
names(s14) <- c("cti", "elev", "cover_class", "hillshade", "ndvi_avg", "ndvi_ti", "precip", "slope")
m14 <- lm(log(GDM) ~ cover_class + cti + elev + ndvi_ti + slope + ndvi_avg, data=dat.GDM)
pred.14 <- predict(s14, m14, fun=predfun, index=1:2, progress="text") # predfun returns two variables (response and se), so need to include index=1:2

# having trouble with cover_class; trying with just the easy continuous ones to see if i can make it work
m14 <- lm(log(GDM) ~ cti + elev + ndvi_ti + slope + ndvi_avg, data=dat.GDM)
pred.14 <- predict(s14, m14, fun=predfun, index=1:2, progress="text")
names(pred.14) <- c("GDM", "se")
plot(pred.14[["GDM"]])
# oh. em. eff. geeeeeee. too cool.

# ok, so how do i make cover_class work???

cc_wtf <- raster("C:/Users/kristin.barker/Documents/GitHub/Vegetation/writtenrasters/covs2014/esp6_14.tif")
plot(cc_wtf)
  # maybe it thinks those numbers are numeric, not factors?
  # or i need to rename to match covariate names in lm
  # and don't forget about factor levels; not sure those are captured in rasters
  
# in lm: used cover_class, factor with levels  
  # which is in a dataframe consisting of plot info and extracted values from rasters
  # cover_class is pulled from esp6_14 and esp6_15 rasters
	# data with 13 slots
cc_wtf <- raster("C:/Users/kristin.barker/Documents/GitHub/Vegetation/writtenrasters/covs2014/esp6_14.tif")
plot(cc_wtf)
cc_wtf@data 
	# Slot "isfactor":
	#[1] FALSE
cc_wtf@data <- as.factor(cc_wtf@data)
	#dat.GDM$cover_class, used in the lm, is factor with 12 levels (correct)
	#but esp6 tifs also have a 0 which gives them 13; i think that's the issue
	
# jesse says 0s are non-habitat. replacing with NAs
cc_wtf[cc_wtf==0] <- NA
# and save as new raster to pull for predict
writeRaster(cc_wtf, 
  "C:/Users/kristin.barker/Documents/GitHub/Vegetation/writtenrasters/covs2014/cover_class", 
  format = 'GTiff', 
  overwrite = TRUE)
# then restack and rerun
rast_14 <- list.files(path=paste(wd, "writtenrasters/covs2014", sep="/"), pattern="tif$", full.names=TRUE) 
s14 <- stack(rast_14) 
names(s14)
names(s14) <- c("cover_class", "cti", "elev", "hillshade", "ndvi_avg", "ndvi_ti", "precip", "slope")
m14 <- lm(log(GDM) ~ cover_class + cti + elev + ndvi_ti + slope + ndvi_avg, data=dat.GDM)
pred.14 <- predict(s14, m14, fun=predfun, index=1:2, progress="text")

plot(exp(pred.14[["layer.1"]]))

# ok sweet. steps for completion:
# make sure pulling ndvi_avg according to yr (pretty sure yes)
# separate covariate raster stacks per year
# predict for each year

#########################
## TREE COVER RASTER STUFF ####
##########################

#forest/riparian- 1, 2, 5, 6, 8, 9, 10, 11, 12
#nocov - 3, 4, 7

testcov14 <- raster(ifelse(esp6_14@data@values == 3 | # grass/shrub/open woodland
						   esp6_14@data@values == 4 | # dry ag
						   esp6_14@data@values == 7 | # irrigated ag
						   esp6_14@data@values == NA, 0, 1)) # na
#Error in (function (classes, fdef, mtable)  : 
#  unable to find an inherited method for function 'raster' for signature '"numeric"'	

testcov14 <- esp6_14
testcov14@data@values <- (ifelse(
  testcov14@data@values == 3 | # grass/shrub/open woodland
	testcov14@data@values == 4 | # dry ag
	testcov14@data@values == 7 | # irrigated ag
	testcov14@data@values == NA, 0, 1)) # na
plot(testcov14)
unique(testcov14@data@values)
# close but no 1s

testcov14 <- esp6_14
testcov14@data@values <- (ifelse(
  testcov14@data@values == 3 | # grass/shrub/open woodland
	testcov14@data@values == 4 | # dry ag
	testcov14@data@values == 7, # irrigated ag
	 0, 1)) # na
plot(testcov14)
unique(testcov14@data@values)
# bam

#########################
## GDM MODEL STUFF ####
##########################

# check correlations with landcover (factor)
# just using global model bc can't run this with zeros in response
testdat <- dat.GDM %>%
  mutate(GDMsum = sum(GDMforb, GDMgrass, GDMshrub))
m.all <- lm(log10(GDMsum) ~ cover_class + cc + cti + elev + gsri + ndvi_ti + sum_precip + slope, data=testdat)
vif(m.all)
# i think but am not sure that 9 isn't necessarily huge
# going forward as if there are no problems here...

#########################
## DELETED CODE ####
##########################


# herbaceous biomass and herbaceous forage biomass per plot - by species
plot.herb.species <- plot.herb %>%
  select(c(PlotVisit, Species, Biomass)) %>%
  rename(HerbBiomass = Biomass) %>%
  full_join(forage.herb, by = c("PlotVisit", "Species")) %>%
  rename(ForageHerbBiomass = Biomass) 
plot.herb.species[is.na(plot.herb.species)] <- 0

#pull genus from full species name 
plot.shrub$Genus <- sapply(strsplit(as.character(plot.shrub$NameScientific), " "), "[", 1)

#Scale up to plot level - all herbaceous biomass
biomass <- drywt
biomass <- summarise(group_by(biomass, PlotVisit, Species), g0.75m = sum(grams))
#biomass <- biomass[-1767,] #remove NA row caused by above line for some reason
biomass$g1m <- biomass$g0.75m*1.33333333333333
biomass <- left_join(biomass, spp, by = "Species")

# COVER - plus quadrat ID, quadrat-visit ID, plot-visit ID
cover <- sqlQuery(channel, paste("select * from Cover"))
colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "GrassCov", "ShrubCov",
                     "SubShrubCov", "ForbCov", "MossLichenCov", "NonVegCov", "SmTreeCov")
cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  select(c(GrassCov, ForbCov, QuadratVisit))

##look at stuff and ponder what i'm actually doing
##bc it's hard to get back to data analyses after playing with bears
  #344.2014-06-16.20
allcover[allcover$QuadratVisit %in% "344.2014-06-16.0" | allcover$QuadratVisit %in% "344.2014-06-16.10" | 
         allcover$QuadratVisit %in% "344.2014-06-16.20" | allcover$QuadratVisit %in% "344.2014-06-16.30" | 
         allcover$QuadratVisit %in% "344.2014-06-16.40",]
clip[clip$PlotVisit %in% "344.2014-06-16",] %>%
  arrange(PlotM)
classn[classn$PlotVisit %in% "344.2014-06-16",] %>%
  arrange(PlotM)
  #323.2014-06-30.20
allcover[allcover$QuadratVisit %in% "323.2014-06-30.0" | allcover$QuadratVisit %in% "323.2014-06-30.10" | 
         allcover$QuadratVisit %in% "323.2014-06-30.20" | allcover$QuadratVisit %in% "323.2014-06-30.30" | 
         allcover$QuadratVisit %in% "323.2014-06-30.40",]
clip[clip$PlotVisit %in% "323.2014-06-30",] %>%
  arrange(PlotM)
classn[classn$PlotVisit %in% "323.2014-06-30",] %>%
  arrange(PlotM)

####oops, put this in plots not quadrats
quadrat$ForbWt <- quadrat$ForbWt*1.333333 #convert from g/0.75m^2 to g/m^2
quadrat$GrassWt <- quadrat$GrassWt*1.333333 #convert from g/0.75m^2 to g/m^2
quadrat.spp$ClipGrams <- quadrat.spp$ClipGrams*1.3333333

#####################################
########## (PREVIOUS CALCULATIONS)
#biomass per plot - all herbaceous
biomass.plot <- quadrat.spp %>%
  select(PlotVisit, QuadratVisit, ForbCov, GrassCov) %>%
  inner_join(quadrat, by = "QuadratVisit") 
biomass.plot <- biomass.plot[!duplicated(biomass.plot),] #remove rows duplicated by join
biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333) #biomass to plot-level, g/m^2
biomass.plot$gHerb <- biomass.plot$gForbs+biomass.plot$gGrass
biomass.plot$PlotID <- substr(biomass.plot$PlotVisit, 1, 3)
biomass.plot$Date <- substr(biomass.plot$PlotVisit, 5, 14)


#biomass per plot - forage only
biomass.forage <- quadrat.spp 
biomass.forage$NameScientific <- as.character(biomass.forage$NameScientific) #add genus
biomass.forage$Genus <- sapply(strsplit(biomass.forage$NameScientific, " "), "[", 1)
biomass.forage <- semi_join(biomass.forage, forage, by = "Genus") #forage plants only
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)
