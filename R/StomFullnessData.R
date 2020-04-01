#Calculates stomach fullness index

library(dplyr)
library(readr)
library(data.table)
library(rgdal)
library(Survdat)

out.dir = "output"
data.dir <- "data"

#Stomach weights in union_fscs_svbio are unaudited and only start in 2001 (don't use stom.epu)
#Use updated food habits data: allfh.Rdata from \\net\bsmith\DataRequests\R
load(file.path(data.dir, 'allfh.RData'))

#View poststrat function to determine variables required
poststrat

allfh$LAT <- allfh$declat 
allfh$LON <- allfh$declon *-1
allfh$CRUISE6 <- allfh$cruise6
allfh$STRATUM <- allfh$stratum
allfh$STATION <- allfh$station

head(allfh)

x <- as.data.table(allfh)

#Find missing values
which(is.na(x$LAT))
which(is.na(x$LON))
which(is.na(x$CRUISE6))
which(is.na(x$STRATUM))
which(is.na(x$STATION))

#Remove rows with missing LAT or LON
x <- x[!is.na(x$LAT),]
x <- x[!is.na(x$LON),]

head(x)

#Grab strata
EPUstrata <- readOGR(gis.dir, 'EPU')

head(fh.epu)

#Output results either to a flat .csv file or .RData set
#write.csv(fh.epu, file = file.path(out.dir, 'FoodHabits_EPU.csv'), row.names = F)
#save(     fh.epu, file = file.path(out.dir, 'FoodHabits_EPU.RData'))

#load(file.path(data.dir, 'FoodHabits_EPU.RData'))

#Predator names
fh.epu$Species[fh.epu$svspp==13] <- 'Smooth Dogfish'
fh.epu$Species[fh.epu$svspp==15] <- 'Spiny Dogfish'
fh.epu$Species[fh.epu$svspp==23] <- 'Winter Skate'
fh.epu$Species[fh.epu$svspp==26] <- 'Little Skate'
fh.epu$Species[fh.epu$svspp==28] <- 'Thorny Skate'
fh.epu$Species[fh.epu$svspp==32] <- 'Atl Herring'
fh.epu$Species[fh.epu$svspp==72] <- 'Silver Hake'
fh.epu$Species[fh.epu$svspp==73] <- 'Atl Cod'
fh.epu$Species[fh.epu$svspp==74] <- 'Haddock'
fh.epu$Species[fh.epu$svspp==75] <- 'Pollock'
fh.epu$Species[fh.epu$svspp==76] <- 'White Hake'
fh.epu$Species[fh.epu$svspp==77] <- 'Red Hake'
fh.epu$Species[fh.epu$svspp==78] <- 'Spotted Hake'
fh.epu$Species[fh.epu$svspp==102] <- 'American Plaice'
fh.epu$Species[fh.epu$svspp==103] <- 'Summer Flounder'
fh.epu$Species[fh.epu$svspp==104] <- 'Fourspot'
fh.epu$Species[fh.epu$svspp==105] <- 'Yellowtail'
fh.epu$Species[fh.epu$svspp==106] <- 'Winter Flounder'
fh.epu$Species[fh.epu$svspp==107] <- 'Witch Flounder'
fh.epu$Species[fh.epu$svspp==108] <- 'Windowpane Flounder'
fh.epu$Species[fh.epu$svspp==121] <- 'Mackerel'
fh.epu$Species[fh.epu$svspp==131] <- 'Butterfish'
fh.epu$Species[fh.epu$svspp==135] <- 'Bluefish'
fh.epu$Species[fh.epu$svspp==141] <- 'Black Sea Bass'
fh.epu$Species[fh.epu$svspp==143] <- 'Scup'
fh.epu$Species[fh.epu$svspp==145] <- 'Weakfish'
fh.epu$Species[fh.epu$svspp==155] <- 'Acadian Redfish'
fh.epu$Species[fh.epu$svspp==164] <- 'Sea Raven'
fh.epu$Species[fh.epu$svspp==193] <- 'Ocean Pout'
fh.epu$Species[fh.epu$svspp==197] <- 'Goosefish'

#pdwgt is indwt (individual fish weight in grams) pulled from union_fscs_svbio
#pdsex is sex of predator fish, pdgutw and pdgutv are full data sets of stomach weights and volumes
#calculate stomach fullness and remove duplicates due to each row being a prey item:
uniqstom <- fh.epu %>%
  dplyr::filter(!is.na(Species), year >1991, pdwgt >= 1, !is.na(pdgutw)) %>%
  mutate(stom_full = pdgutw/pdwgt) %>%
  select(CRUISE6, STRATUM, STATION, year, season, EPU, Species, pdid, pdsex, pdwgt, pdgutw, stom_full)

stom <- dplyr::distinct(uniqstom)

stom <-  dplyr::filter(stom, stom_full <1, stom_full >0)

#Creating Average Stomach Fullness for females by year, EPU, species
AvgStomFullEPU <- stom %>% dplyr::filter(pdsex == 2, season == "FALL") %>%
  group_by(year, EPU, Species) %>% 
  mutate(AvgStomFullEPU=(mean(stom_full)))

readr::write_csv(AvgStomFullEPU, here::here(out.dir,"AnnualStomFullnessEPU_2019.csv"))
