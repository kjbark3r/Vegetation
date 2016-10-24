library(dplyr)

setwd("C:\\Users\\kjbark3r\\Documents\\GitHub\\Vegetation\\zFireManu")

sci <- read.csv("NSERP_SP_list.csv") %>%
  select(c(PlantCode, NameScientific)) 
#colnames(sci) <- c("Code", "NameScientific")


codes <- read.csv("shrubcodes.csv") %>%
  #rename(Code = PlantCode) %>%
  left_join(sci)
write.csv(codes, file="shrubnames.csv", row.names=FALSE)
