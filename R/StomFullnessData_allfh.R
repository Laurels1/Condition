#Calculates stomach fullness index

library(dplyr)
library(readr)
library(data.table)
library(rgdal)
library(devtools)
#devtools::install_github('slucey/RSurvey/Survdat', )
library(survdat)

# out.dir = "output"
# data.dir <- "data"
# gis.dir <- "gis"

#StomFullnessData_llfh <- function(out.dir,data.dir,gis.dir){

#Stomach weights in union_fscs_svbio are unaudited and only start in 2001 (don't use stom.epu)
#Use updated food habits data: allfh.Rdata from \\net.nefsc.noaa.gov\home10\bsmith\DataRequests\R
load(file.path(data.dir, 'allfh.RData'))

#View poststrat function to determine variables required
#poststrat

allfh$LAT <- allfh$declat 
allfh$LON <- allfh$declon *-1
allfh$CRUISE6 <- allfh$cruise6
allfh$STATION <- allfh$station
#STRATUM in CondCal for gam_calcs.R is a factor. 
#stratum in allfh is an interger and removes the leading zero.
#Create a new variable STRATUM that is a factor with a leading zero for merge.
#allfh$STRATUM <- as.factor(paste("0",allfh$stratum,sep=''))

#STRATUM is numeric in survdat data:
all_fh <- allfh %>% dplyr::mutate(STRATUM = stratum)


#head(allfh)

x <- as.data.table(all_fh)

#Find missing values
which(is.na(x$LAT))
which(is.na(x$LON))
which(is.na(x$CRUISE6))
which(is.na(x$STRATUM))
which(is.na(x$STATION))

#Remove rows with missing LAT or LON
x <- x[!is.na(x$LAT),]
x <- x[!is.na(x$LON),]

#head(x)

#Grab EPU
#EPUstrata <- readOGR(gis.dir, 'EPU')
#EPU using survdat package:
strata <- sf::st_read(dsn = system.file("extdata", "epu.shp", package = "survdat"),
                      quiet = T)

#Post stratify data if necessary
fh.epu <- survdat::post_strat(x, strata, areaDescription = 'EPU', na.keep = TRUE)
#setnames(fh.epu, 'newstrata', 'EPU')

#head(fh.epu)

#Predator names
fh.epu$Species[fh.epu$svspp==13] <- 'Smooth dogfish'
fh.epu$Species[fh.epu$svspp==15] <- 'Spiny dogfish'
fh.epu$Species[fh.epu$svspp==23] <- 'Winter skate'
fh.epu$Species[fh.epu$svspp==26] <- 'Little skate'
fh.epu$Species[fh.epu$svspp==28] <- 'Thorny skate'
fh.epu$Species[fh.epu$svspp==32] <- 'Atl herring'
fh.epu$Species[fh.epu$svspp==72] <- 'Silver hake'
fh.epu$Species[fh.epu$svspp==73] <- 'Atl cod'
fh.epu$Species[fh.epu$svspp==74] <- 'Haddock'
fh.epu$Species[fh.epu$svspp==75] <- 'Pollock'
fh.epu$Species[fh.epu$svspp==76] <- 'White hake'
fh.epu$Species[fh.epu$svspp==77] <- 'Red hake'
fh.epu$Species[fh.epu$svspp==78] <- 'Spotted hake'
fh.epu$Species[fh.epu$svspp==102] <- 'American plaice'
fh.epu$Species[fh.epu$svspp==103] <- 'Summer flounder'
fh.epu$Species[fh.epu$svspp==104] <- 'Fourspot'
fh.epu$Species[fh.epu$svspp==105] <- 'Yellowtail'
fh.epu$Species[fh.epu$svspp==106] <- 'Winter flounder'
fh.epu$Species[fh.epu$svspp==107] <- 'Witch flounder'
fh.epu$Species[fh.epu$svspp==108] <- 'Windowpane flounder'
fh.epu$Species[fh.epu$svspp==121] <- 'Mackerel'
fh.epu$Species[fh.epu$svspp==131] <- 'Butterfish'
fh.epu$Species[fh.epu$svspp==135] <- 'Bluefish'
fh.epu$Species[fh.epu$svspp==141] <- 'Black sea bass'
fh.epu$Species[fh.epu$svspp==143] <- 'Scup'
fh.epu$Species[fh.epu$svspp==145] <- 'Weakfish'
fh.epu$Species[fh.epu$svspp==155] <- 'Acadian redfish'
fh.epu$Species[fh.epu$svspp==164] <- 'Sea raven'
fh.epu$Species[fh.epu$svspp==193] <- 'Ocean pout'
fh.epu$Species[fh.epu$svspp==197] <- 'Goosefish'
fh.epu$Species[fh.epu$SVSPP=='84'] <- 'Cusk'
fh.epu$Species[fh.epu$SVSPP=='69'] <- 'Offshore hake'
fh.epu$Species[fh.epu$SVSPP=='4'] <- 'Roughtail stingray'
fh.epu$Species[fh.epu$SVSPP=='375'] <- 'Spiny butterfly ray'
fh.epu$Species[fh.epu$SVSPP=='27'] <- 'Smooth skate'
fh.epu$Species[fh.epu$SVSPP=='25'] <- 'Rosette skate'
fh.epu$Species[fh.epu$SVSPP=='24'] <- 'Clearnose skate'
fh.epu$Species[fh.epu$SVSPP=='22'] <- 'Barndoor skate'
fh.epu$Species[fh.epu$SVSPP=='19'] <- 'Bullnose ray'
fh.epu$Species[fh.epu$SVSPP=='18'] <- 'Bluntnose stingray'
fh.epu$Species[fh.epu$SVSPP=='163'] <- 'Longhorn sculpin'
fh.epu$Species[fh.epu$SVSPP=='156'] <- 'Blackbelly rosefish'
fh.epu$Species[fh.epu$SVSPP=='136'] <- 'Atlantic croaker'

#Feeding guilds by planktivore/benthivore/piscivore? Varies by size of predator.

#pdwgt is indwt (individual fish weight in grams) pulled from union_fscs_svbio
#pdsex is sex of predator fish, pdgutw and pdgutv are full data sets of stomach weights and volumes

#prey volume by prey group and EPU for 2020 SOE (need to use allwt_stratfin_byvars.R to get diet comps by prey weight):
prey <- fh.epu %>%
  dplyr::filter(!is.na(Species), year >1990, pdwgt >=1, !is.na(pdgutw)) %>%
  dplyr::select(CRUISE6, STRATUM, STATION, year, season, EPU, Species, pdid, pdsex, pdwgt, pdgutw, analcat)

#calculate stomach fullness and remove duplicates due to each row being a prey item:
uniqstom <- fh.epu %>%
  dplyr::filter(!is.na(Species), year >1990, pdwgt >= 1, !is.na(pdgutw)) %>%
  mutate(stom_full = pdgutw/pdwgt) %>%
  dplyr::select(CRUISE6, STRATUM, STATION, year, season, EPU, Species, pdid, pdsex, pdwgt, pdgutw, stom_full)

stom <- dplyr::distinct(uniqstom)

#stom is used to import stomach data into gam_calcs.R:
stom <-  dplyr::filter(stom, stom_full <1, stom_full >0)

#Creating Average Fall Stomach Fullness for females by year, EPU, species
#AvgStomFullEPU <- stom %>% dplyr::filter(pdsex == 2, season == "FALL") %>%
#  group_by(year, EPU, Species) %>% 
#  mutate(AvgStomFullEPU=(mean(stom_full)))

#Creating Average Fall Stomach Fullness by year, EPU, species, sex
AvgStomFullEPU <- stom %>% dplyr::filter(season == "FALL") %>%
  group_by(year, EPU, Species, pdsex) %>% 
  mutate(AvgStomFullEPU=(mean(stom_full)))

#Creating Average Fall Stomach Fullness by year, STRATUM, species, sex
AvgStomFullStrata <- stom %>% dplyr::filter(season == "FALL") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullStrata=(mean(stom_full)))

#Creating Average Spring Stomach Fullness by year, STRATUM, species, sex
AvgStomFullSpringStrata <- stom %>% dplyr::filter(season == "SPRING") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullSpringStrata=(mean(stom_full)))

#Data for 2019 SOE stomach fullness index:
readr::write_csv(AvgStomFullEPU, here::here(out.dir,"AnnualStomFullnessEPU_2020.csv"))
#return(stom)

#}