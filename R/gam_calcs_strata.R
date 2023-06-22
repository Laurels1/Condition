#' gam stuff
#'
#'@param annualcond tibble. Output from RelConditionEPU.R
#'@param out.dir character string. name of directory in which plots and data files will be saved
#'

#install.packages("corrplot")
#install.packages("car")
#install.packages("mgcv.helper")
#devtools::install_github("samclifford/mgcv.helper")

library(mgcv)
library(gam)
library(dplyr)
library(readr)
library(corrplot)
library(data.table)
#library(car)
#install.packages("remotes")
#remotes::install_github("samclifford/mgcv.helper")
#library(mgcv.helper)

#in Nov. 2020 changed data pull to only include representative tows
#turn off function while changing code
#gam_calcs <- function(annualcond,out.dir="output") {
#gam_calcs_strata <- function(cond.epu=cond.epu,stom=stom,out.dir,data.dir,gis.dir) {
#turn on output dir when not using funtion
# out.dir = "output"
# data.dir <- "data"
# 
# gis.dir  <- "gis"

#--------------------------------------------------------------
#Set up data structure of condition data for GAMs:

#Spatial aggregation for input data:
#by strata:

#by EPU:

#Filter data to include StockSmart stock biomass and abundance data:


#Spatial aggregation for GAM runs:
#by shelf:

#by stock (fall BTS stock designations from StockStrataFall.csv):
StockStrata <- readr::read_csv(here::here(data.dir, "StockStrataFall.csv"))

#StockStrata <- readr::read_csv(here::here(data.dir, "StockStrataSpring.csv"))

StockData <- StockStrata %>% tidyr::separate_rows(Strata) %>% dplyr::mutate(STRATUM = as.numeric(Strata))

#Limit survey strata to north of Hatteras:
cond.strata <- cond.epu %>% filter(STRATUM <= 7000)

#For mature mackerel >23 cm:
#cond.strata <- cond.strata %>% dplyr::filter(Species == 'Atlantic mackerel', LENGTH > 23, YEAR >= 1992)

#For immature mackerel <=23:
cond.strata <- cond.strata %>% dplyr::filter(Species == 'Atlantic mackerel', LENGTH >= 23, YEAR >= 1992)

#Drop StockName column for merge with StockSMART:
CondStockjoin <- dplyr::left_join(cond.strata, dplyr::select(StockData, c(SVSPP,STRATUM,Stock) ), by = c("SVSPP", "STRATUM"))

#Assign stock to Unit if sample is outside of stock strata or no stock strata are definited for species:
CondStockUnit <- CondStockjoin %>% dplyr::mutate(StockUnit =ifelse(is.na(Stock), "Unit", Stock))

#Samples without stock area designations (strata where species were sampled but aren't included in stock area definition):
CondStockMissing <- CondStockjoin %>% filter(is.na(Stock))

#GAM runs by sex:



#-------------------------------------------------------------
#Not working currently to source, but reminder to run these files first:
#source("R/StomFullnessData_allfh.R")
#source("R/RelConditionEPU.R")

#Explore different ways of aggregating Relative Condition:
#Creating Average Relative Condition and Average Stomach Fullness by tow, species, sex
#For use in multi-model dataset:
#AvgTowCond <- cond.epu %>% group_by(CRUISE6, STRATUM, STATION, TOW, Species, sex) %>% 
#  mutate(AvgTowRelCond=(mean(RelCond)), AvgTowRelCondSD=(sd(RelCond)))

#For direct data pull (used in 2019 SOE and 2019 AFS GAMs):
#Creating Average Relative Condition by strata, species, sex
# AvgStrataCond <- cond.epu %>% group_by(CRUISE6, STRATUM, Species, sex) %>% 
#   mutate(AvgRelCondStrata=(mean(RelCond)), AvgRelCondStrataSD = (sd(RelCond)), AvgExpcatchwtStrata = (mean(EXPCATCHWT)),
#          AvgExpcatchnumStrata= (mean(EXPCATCHNUM)), AvgLatStrata = (mean(LAT)), 
#         AvgLonStrata = (mean(LON)), AvgBottomTempStrata = (mean(BOTTEMP))) %>%
# distinct(AvgRelCondStrata, .keep_all = T)

#Using Survdat:
#Creating Average Relative Condition by strata, species, sex (for 2022 ICES Regime shifts and 2021 GAMs)
AvgStrataCond <- CondStockUnit %>% group_by(CRUISE6, STRATUM, Species, sex) %>%
  mutate(AvgRelCondStrata=(mean(RelCond)), AvgRelCondStrataSD = (sd(RelCond)), AvgExpcatchwtStrata = (mean(BIOMASS)),
         AvgExpcatchnumStrata= (mean(ABUNDANCE)), AvgLatStrata = (mean(LAT)),
         AvgLonStrata = (mean(LON)), AvgBottomTempStrata = (mean(BOTTEMP)), AvgSurfaceTempStrata = (mean(SURFTEMP))) %>%
  distinct(AvgRelCondStrata, .keep_all = T)

#Creating Average Relative Condition by Year, species for shelf-wide regime shift work (Scott Large Dynamic Factor Analysis)
AvgYearCond <- CondStockUnit %>% group_by(YEAR, Species) %>%
  mutate(AvgRelCondYear=(mean(!is.na(RelCond))), AvgRelCondYearSD = (sd(!is.na(RelCond))), AvgExpcatchwtYear = (mean(!is.na(BIOMASS))),
         AvgExpcatchnumYear= (mean(!is.na(ABUNDANCE))), AvgLatYear = (mean(!is.na(LAT))),
         AvgLonYear = (mean(!is.na(LON))), AvgBottomTempYear = (mean(!is.na(BOTTEMP))), AvgSurfaceTempYear = (mean(!is.na(SURFTEMP)))) %>%
  distinct(AvgRelCondYear, .keep_all = T)

#Creating Average Relative Condition by Year and EPu, species for shelf-wide regime shift work (Scott Large Dynamic Factor Analysis)
AvgEPUCond <- CondStockUnit %>% group_by(YEAR, EPU, Species) %>% 
  mutate(AvgRelCondEPU=(mean(!is.na(RelCond))), AvgRelCondEPUSD = (sd(!is.na(RelCond))), AvgExpcatchwtEPU = (mean(!is.na(BIOMASS))),
         AvgExpcatchnumEPU= (mean(!is.na(ABUNDANCE))), AvgLatEPU = (mean(!is.na(LAT))), 
         AvgLonEPU = (mean(!is.na(LON))), AvgBottomTempEPU = (mean(!is.na(BOTTEMP))), AvgSurfaceTempEPU = (mean(!is.na(SURFTEMP)))) %>%
  distinct(AvgRelCondEPU, .keep_all = T)

#Creating Average Relative Condition and Average Stomach Fullness by EPU, species, sex
#Couldn't run mechanisms model because data too sparse:
#AvgEPUCond <- stom.epu %>% group_by(CRUISE6, EPU, Species, sex) %>% 
#  mutate(AvgRelCondEPU=(mean(RelCond)), AvgStomFullEPU=(mean(stom_full))) %>%
#  distinct(AvgRelCondEPU, .keep_all = T)
#---------------------------------------------------------------

#Bringing in average temperature data from Chris Melrose (e.g. Data For Laurel- Sep 1 2022.xlsx saved as files below)
#***Before reading EcoMon data, have to change all NaN values to NAs
AvgTempSpringData <- readr::read_csv(here::here(data.dir, "AverageTempSpring2021.csv"))
AvgTempSummerData <- readr::read_csv(here::here(data.dir, "AverageTempSummer2021.csv"))
AvgTempFallData <- readr::read_csv(here::here(data.dir, "AverageTempFall2021.csv"))
AvgTempWinterData <- readr::read_csv(here::here(data.dir, "AverageTempWinter2021.csv"))

AvgTempSpringFormat <- AvgTempSpringData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempSpring, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempSummerFormat <- AvgTempSummerData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempSummer, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempFallFormat <- AvgTempFallData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempFall, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempWinterFormat <- AvgTempWinterData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempWinter, c(GB, GOM,SS, MAB), na.rm=F)

AvgTemp <- Reduce(dplyr::full_join, list(AvgTempWinterFormat, AvgTempSpringFormat, AvgTempSummerFormat, AvgTempFallFormat))

AvgTemp <- AvgTemp %>% dplyr::mutate_all(~(replace(., . == NaN, NA))) %>%
  dplyr::mutate_at(c("AvgTempWinter", "AvgTempSpring", "AvgTempSummer", "AvgTempFall"), as.numeric)

CondAvgTemp <- dplyr::left_join(AvgStrataCond, AvgTemp, by=c("YEAR", "EPU"))

#Test for regime shifts in summer temp (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# SummerTemp <- AvgTempSummerFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, AvgTempSummer)
# SummerRegime <- rpart::rpart(AvgTempSummer~YEAR, data=SummerTemp)
# #Choose simplest tree within one standard error of best tree: xerror +xstd > xerror of next row 
# printcp(SummerRegime) 
# #SummerTempRegimePlot <- rpart.plot::rpart.plot(SummerRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# SummerRegimeResults <- as.data.frame(SummerRegime[["splits"]])
# SummerSplit1 <- SummerRegimeResults$index[1]
# SummerSplit2 <- SummerRegimeResults$index[2]
# SummerSplit3 <- SummerRegimeResults$index[3]
# SummerSplit4 <- SummerRegimeResults$index[4]
# SummerSplit5 <- SummerRegimeResults$index[5]
# 
# #Test for regime shifts in spring temp (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
SpringTemp <- AvgTempSpringFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, AvgTempSpring)
SpringRegime <- rpart::rpart(AvgTempSpring~YEAR, data=SpringTemp)
#Choose simplest tree within one standard error of best tree: xerror +xstd < xerror of next row
printcp(SpringRegime)
#error margins too large:
#SpringTempRegimePlot <- rpart.plot::rpart.plot(SpringRegime)
#b <- SpringRegime$cptable[which.min(SpringRegime$cptable[, "xerror"]), "CP"]
#SpringRegimePrune <- prune(SpringRegime, cp = b)
#SpringTempPlot2 <- rpart.plot::rpart.plot(SpringRegimePrune)

#Pull regime shift years into new data frame to add to plot:
SpringRegimeResults <- as.data.frame(SpringRegime[["splits"]])
SpringSplit1 <- SpringRegimeResults$index[1]
SpringSplit2 <- SpringRegimeResults$index[2]
SpringSplit3 <- SpringRegimeResults$index[3]
SpringSplit4 <- SpringRegimeResults$index[4]
# 
# #Test for regime shifts in Fall temp (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# FallTemp <- AvgTempFallFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, AvgTempFall)
# FallRegime <- rpart::rpart(AvgTempFall~YEAR, data=FallTemp)
# #FallTempRegimePlot <- rpart.plot::rpart.plot(FallRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# FallRegimeResults <- as.data.frame(FallRegime[["splits"]])
# FallSplit1 <- FallRegimeResults$index[1]
# FallSplit2 <- FallRegimeResults$index[2]
# FallSplit3 <- FallRegimeResults$index[3]
# 
# #Test for regime shifts in Winter temp (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# WinterTemp <- AvgTempWinterFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, AvgTempWinter)
# WinterRegime <- rpart::rpart(AvgTempWinter~YEAR, data=WinterTemp)
# #WinterTempRegimePlot <- rpart.plot::rpart.plot(WinterRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# WinterRegimeResults <- as.data.frame(WinterRegime[["splits"]])
# WinterSplit1 <- WinterRegimeResults$index[1]
# WinterSplit2 <- WinterRegimeResults$index[2]
# WinterSplit3 <- WinterRegimeResults$index[3]

#----------------------------------------------------------------------------------
#Bring in GLORYS bottom temperature data by NEFSC survey strata (mismatch of some strata currently)
# GLORYSdata <- readr::read_csv(here::here(data.dir, "GLORYS_bottom_temp_STRATA_1993_2018.csv"))
# 
# GLORYSformat <- GLORYSdata %>% 
#   separate(date, c('Year2', 'MONTH', 'DAY'), sep='-')
# 
# GLORYSseason <- GLORYSformat %>% group_by(Year2, STRATA) %>% 
#   dplyr::mutate(YEAR=as.numeric(Year2),
#                 STRATUM = STRATA, 
#                 #STRATUM is numeric from survdat pull:
#                 #STRATUM = as.character(paste0('0',STRATA)), 
#                 GLORYSwinter=ifelse(season==1,(weighted.mean), NA), GLORYSspring=ifelse(season==2,(weighted.mean), NA),
#                 GLORYSsummer=ifelse(season==3,(weighted.mean), NA), GLORYSfall=ifelse(season==4,(weighted.mean), NA))
# 
# GLORYS2 <- GLORYSseason %>% group_by(YEAR, STRATUM) %>% 
#   summarize(GLORYSwinter=mean(GLORYSwinter, na.rm=T), GLORYSspring=mean(GLORYSspring, na.rm=T),
#             GLORYSsummer=mean(GLORYSsummer, na.rm=T),GLORYSfall=mean(GLORYSfall, na.rm=T))
# 
# CondGLORYS <- dplyr::left_join(CondAvgTemp, GLORYS2, by=c('YEAR', 'STRATUM'))
# 
# #See NAs for GLORYS merge:
# #NAglorys <- CondGLORYS %>% filter(is.na(GLORYSwinter))
# 
# #Trying to determine why 27% of Condition data doesn't have corresponding GLORYS data:
# #Occurs across all years and strata
# GLORYSna <- CondGLORYS %>% filter(is.na(GLORYSwinter)) 
# GLORYSna_not <- CondGLORYS %>% filter(!is.na(GLORYSwinter)) 
# GLORYSnaStratum <- GLORYSna_not[!duplicated(GLORYSna_not[,'STRATUM']),]
# GLORYSnaStratOrder <- GLORYSnaStratum %>% arrange(STRATUM)

#--------------------------------------------------------------------------------
#Bringing in small-large copepods, total copepods and zooplankton abundance anomaly by strata from Harvey Walsh:
#Change NaN's to NA's before importing
ZooplStrata <- readr::read_csv(here::here(data.dir,"EcoMon_ZooplanktonData2021_BTSMeanAbundance.csv"), col_names = T)

#****have to separate out into 4 seasons to merge with condition data
ZoopStr <- ZooplStrata %>% dplyr::mutate(YEAR=Year, STRATUM = BTS, Seasons = as.character(Season)) %>%
  dplyr::mutate(TotalCopepodStrata = (SmCalanoida+LgCalanoida+Cyclopoida)/1000) %>%
  dplyr::mutate(ZooplAbundStrata= (SmCalanoida+LgCalanoida+Bryozoa+Chaetognatha+
                                     Cirripedia+Cnidaria+Cyclopoida+Decapoda+
                                     Polychaeta+Diplostraca+Echinodermata+Euphausiacea+
                                     Gammaridea+Hyperiidea+Mollusca+Mysidacea+Ostracoda+
                                     Protozoa+Thecosomata+Tunicata)/1000) %>%
  dplyr::filter(LgCalanoida != 0) %>%
  dplyr::mutate(CopepodSmallLargeStrata = SmCalanoida/LgCalanoida) %>%
  dplyr::select(YEAR, STRATUM, Seasons, CopepodSmallLargeStrata, TotalCopepodStrata, ZooplAbundStrata) %>%
  dplyr::mutate_at(c('CopepodSmallLargeStrata', 'TotalCopepodStrata', 'ZooplAbundStrata'), as.numeric)

 ZooSeason <- ZoopStr %>% dplyr::mutate(season1 = ifelse(Seasons == '1', 'Winter', 
                                                         ifelse(Seasons =='2', 'Spring', ifelse(Seasons == '3', 'Summer', ifelse(Seasons=='4', 'Fall', NA)))))

#ZooSeason <- ZoopStr %>% dplyr::mutate(SEASON = ifelse(Seasons == '1', 'WINTER', 
#                                                        ifelse(Seasons =='2', 'SPRING', ifelse(Seasons == '3', 'SUMMER', ifelse(Seasons=='4', 'FALL', NA)))))

SmLgCop <- ZooSeason %>% dplyr::select(YEAR, STRATUM, season1, CopepodSmallLargeStrata) %>%
  dplyr::mutate(season1=paste('CopepodSmallLargeStrata', season1, sep="")) %>%
  tidyr::spread(season1, CopepodSmallLargeStrata)

TotCop <- ZooSeason %>% dplyr::select(YEAR, STRATUM, season1, TotalCopepodStrata) %>%
  dplyr::mutate(season1=paste('TotalCopepodStrata', season1, sep="")) %>%
  tidyr::spread(season1, TotalCopepodStrata)

ZoopAbund <- ZooSeason %>% dplyr::select(YEAR, STRATUM, season1, ZooplAbundStrata) %>%
  dplyr::mutate(season1=paste('ZooplAbundStrata', season1, sep="")) %>%
  tidyr::spread(season1, ZooplAbundStrata)

ZoopIndexStrata <- Reduce(dplyr::full_join, list(SmLgCop, TotCop, ZoopAbund))

ZoopData <- dplyr::left_join(CondAvgTemp, ZoopIndexStrata, by=c('YEAR', 'STRATUM'))

#Zooplankton data by EPU, YEAR for Scott Large Dynamic Factor Analysis:
ZoopDataEPU <- ZoopData %>% group_by(YEAR, EPU, SEASON) %>% 
  dplyr:: mutate(CopepodSmLgSpringEPU=(mean(CopepodSmallLargeStrataSpring, na.rm=TRUE)),
                 CopepodSmLgSummmerEPU=(mean(CopepodSmallLargeStrataSummer, na.rm=TRUE)),
                 CopepodSmLgFallEPU=(mean(CopepodSmallLargeStrataFall, na.rm=TRUE)),
                 CopepodSmLgWinterEPU=(mean(CopepodSmallLargeStrataWinter, na.rm=TRUE)),
                 TotCopSpringEPU=(sum(TotalCopepodStrataSpring, na.rm=TRUE)),
                 TotCopSummerEPU=(sum(TotalCopepodStrataSummer, na.rm=TRUE)),
                 TotCopFallEPU=(sum(TotalCopepodStrataFall, na.rm=TRUE)),
                 TotCopWinterEPU=(sum(TotalCopepodStrataWinter, na.rm=TRUE)),
                 ZoopAbundSpringEPU=(sum(ZooplAbundStrataSpring, na.rm=TRUE)), 
                 ZoopAbundSummerEPU=(sum(ZooplAbundStrataSummer, na.rm=TRUE)), 
                 ZoopAbundFallEPU=(sum(ZooplAbundStrataFall, na.rm=TRUE)), 
                 ZoopAbundWinterEPU=(sum(ZooplAbundStrataWinter, na.rm=TRUE)), 
              )

#readr::write_csv(ZooSeason, here::here(out.dir,"Zooplankton1977-2021.csv")) 

#Zooplankton data separately for regime shift:
ZooSeason <- ZoopStr %>% dplyr::mutate(SEASONS = ifelse(Seasons == '1', 'WINTER',
                                                       ifelse(Seasons =='2', 'SPRING', ifelse(Seasons == '3', 'SUMMER', ifelse(Seasons=='4', 'FALL', NA)))))

ZoopEPU <- dplyr::left_join(CondAvgTemp, ZooSeason, by=c('YEAR', 'STRATUM'))

ZoopDataSeasonEPU <- ZoopEPU %>% group_by(YEAR, EPU, SEASON) %>%
  dplyr:: mutate(CopepodSmLgEPU=(mean(CopepodSmallLargeStrata, na.rm=TRUE)),
                 TotCopEPU=(sum(TotalCopepodStrata, na.rm=TRUE)),
                 ZoopAbundEPU=(sum(ZooplAbundStrata, na.rm=TRUE)),
  )
# 
#Bringing in difference of small to large copepod anomalies (by EPU from Ryan Morse):
load(here::here("data","1977_2021_SLI.rdata"))
Calfin <- test
#load(here::here("data","1977_2019_SLI_Calfin_Pseudocal_Ctyp_anomaly.rdata"))
#load(here::here("data","1977_2017_SLI_Calfin_Pseudo_Ctyp.rdata"))
#Calfin <- Zooplankton_Primary_Prod
#head(Calfin)
CalfinFormat <- Calfin %>% dplyr::rename(YEAR = year) %>%
  dplyr::select(YEAR, SLIAnom.gbk, SLIAnom.gom, SLIAnom.mab, SLIAnom.scs) %>%
  tidyr::gather(CalEPU, CopepodSmallLarge, c(SLIAnom.gbk, SLIAnom.gom, SLIAnom.mab, SLIAnom.scs)) %>%
  dplyr::mutate(EPU = if_else(CalEPU=='SLIAnom.gbk', 'GB',
                              if_else(CalEPU=='SLIAnom.gom', 'GOM',
                                      if_else(CalEPU=='SLIAnom.mab', 'MAB',
                                              if_else(CalEPU=='SLIAnom.scs', 'SS', 'NA')))))

CondCal <- dplyr::left_join(ZoopDataEPU, CalfinFormat, by=c("YEAR", "EPU"))

#Small-large copepod index for Rob Gamble EDM:
saveRDS(CalfinFormat,file = here::here("other",paste0("SmallLargeCopepods_EDM2021.rds")))

#Test for regime shifts in Copepod small/large ratio (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
CopepodEPU <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, CopepodSmallLarge)
CopepodEPURegime <- rpart::rpart(CopepodSmallLarge~YEAR, data=CopepodEPU)
#CopepodEPURegimePlot <- rpart.plot::rpart.plot(CopepodEPURegime)
CopepodEPURegime$cptable

#Pull regime shift years into new data frame to add to plot:
CopepodEPURegimeResults <- as.data.frame(CopepodEPURegime[["splits"]])
CopepodEPUSplit1 <- CopepodEPURegimeResults$index[1]
CopepodEPUSplit2 <- CopepodEPURegimeResults$index[2]
# CopepodEPUSplit3 <- CopepodEPURegimeResults$index[3]
# CopepodEPUSplit4 <- CopepodEPURegimeResults$index[4]
# CopepodEPUSplit5 <- CopepodEPURegimeResults$index[5]
# CopepodEPUSplit6 <- CopepodEPURegimeResults$index[6]

#Bringing in difference of small to large copepod anomalies (shelf-wide from Ryan Morse):
load(here::here("data","1977_2021_NES_SLI.rdata"))
Calfin <- SLI.nes
#head(Calfin)
CalfinFormat <- Calfin %>% dplyr::rename(YEAR = year) %>%
   dplyr::select(YEAR, SLIAnom.nes) %>%
  dplyr::rename(CopepodSmallLarge = SLIAnom.nes)

#Shelf-wide Small-large copepod index for Rob Gamble EDM:
#saveRDS(CalfinFormat,file = here::here("other",paste0("SmallLargeCopepods_Shelf_EDM2021.rds")))


#Test for regime shifts in Shelf-wide Copepod small/large ratio (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
CopepodShelf <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, CopepodSmallLarge)
CopepodShelfRegime <- rpart::rpart(CopepodSmallLarge~YEAR, data=CopepodShelf)
CopepodShelfRegime$cptable

#Pull regime shift years into new data frame to add to plot:
CopepodShelfRegimeResults <- as.data.frame(CopepodShelfRegime[["splits"]])
CopepodShelfSplit1 <- CopepodShelfRegimeResults$index[1]
CopepodShelfSplit2 <- CopepodShelfRegimeResults$index[2]
# CopepodShelfSplit3 <- CopepodShelfRegimeResults$index[3]
# CopepodShelfSplit4 <- CopepodShelfRegimeResults$index[4]
# CopepodShelfSplit5 <- CopepodShelfRegimeResults$index[5]
# CopepodShelfSplit6 <- CopepodShelfRegimeResults$index[6]

# #Bring in ratio of small to large copepods (by strata from Ryan Morse):
# load(here::here("data","TS_spring_zoop.rda"))
# ZoopSpring <- zoo.spr
# 
# ZooSprStrata <- ZoopSpring %>% dplyr::rename(YEAR=year, STRATUM = epu) %>%
#   dplyr::filter(calfin_100m3 > 0) %>%
#   dplyr::mutate(CopepodSmallLargeSprStrata = ((pseudo_100m3 + tlong_100m3 + cham_100m3 +ctyp_100m3)/calfin_100m3)) %>%
#   dplyr::select(YEAR, STRATUM, CopepodSmallLargeSprStrata)
# 
# load(here::here("data","TS_fall_zoop.rda"))
# ZoopFall <- zoo.fall
# 
# ZooFallStrata <- ZoopFall %>% dplyr::rename(YEAR=year, STRATUM = epu) %>%
#   dplyr::filter(calfin_100m3 > 0) %>%
#   dplyr::mutate(CopepodSmallLargeFallStrata = ((pseudo_100m3 + tlong_100m3 + cham_100m3 +ctyp_100m3)/calfin_100m3)) %>%
#   dplyr::select(YEAR, STRATUM, CopepodSmallLargeFallStrata)
# 
# load(here::here("data","TS_yearly_zoop.rda"))
# ZoopAnnual <- zoo.yr
# 
# ZooAnnualStrata <- ZoopAnnual %>% dplyr::rename(YEAR=year, STRATUM = epu) %>%
#   dplyr::filter(calfin_100m3 > 0) %>%
#   dplyr::mutate(CopepodSmallLargeAnnualStrata = ((pseudo_100m3 + tlong_100m3 + cham_100m3 +ctyp_100m3)/calfin_100m3)) %>%
#   dplyr::select(YEAR, STRATUM, CopepodSmallLargeAnnualStrata)
# 
# 
# CondSLIspr <- dplyr::left_join(CondCal, ZooSprStrata, by=c("YEAR", "STRATUM"))
# CondSLIfall <- dplyr::left_join(CondSLIspr, ZooFallStrata, by=c("YEAR", "STRATUM"))
# CondSLIstrata <- dplyr::left_join(CondSLIfall, ZooAnnualStrata, by=c("YEAR", "STRATUM"))
# 
# dplyr::ungroup(CondSLIstrata)
# dplyr::count(CondSLIstrata, is.na(CopepodSmallLargeSprStrata))
# 
# #Bring in zooplankton anomalies: 
ZoopBio <- readr::read_csv(here::here("data","EPUCopepodBiomassAnomalies.csv"))

Zoop <- ZoopBio %>% dplyr::rename(YEAR=Year)

CondZoo <- dplyr::left_join(CondCal, Zoop, by = c("YEAR", "EPU"))

#Bring in total copepods (as millions of individuals) from NEFSCZooplankton_v3_6b_v2018.xls:
TotalCopepods <- readr::read_csv(here::here("data","TotalCopepods2020.csv"))

TotCop <- dplyr::left_join(CondZoo, TotalCopepods, by = c("YEAR", "EPU"))

#Test for regime shifts in Total Copepods (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# TotCopepods <- TotalCopepods %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, EPU, TotalCopepodsMillions)
# TotCopepodsRegime <- rpart::rpart(TotalCopepodsMillions~YEAR, data=TotCopepods)
# #TotCopepodsRegimePlot <- rpart.plot::rpart.plot(TotCopepodsRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# TotCopepodsRegimeResults <- as.data.frame(TotCopepodsRegime[["splits"]])
# TotCopepodsSplit1 <- TotCopepodsRegimeResults$index[1]
# TotCopepodsSplit2 <- TotCopepodsRegimeResults$index[2]

#--------------------------------------------------------------------------------
#Bloom time and magnitude data
Bloom <- readr::read_csv(here::here("data","FallBloom_Chlorophyll.csv"))

#Use Range Magnitude and Range Duration
#YEAR gives fall bloom from the year before:
Fallbloom <- Bloom %>% dplyr::mutate(YEAR = RecruitmentYear)

#merge with condition data:
FallBloomCond <- dplyr::left_join(TotCop, Fallbloom, by = "YEAR")

#Test for regime shifts in fall bloom magnitude (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# FallBloomMag <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, RangeMagnitude)
# FallBloomMagRegime <- rpart::rpart(RangeMagnitude~YEAR, data=FallBloomMag)
# #FallBloomMagRegimePlot <- rpart.plot::rpart.plot(FallBloomMagRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# FallBloomMagRegimeResults <- as.data.frame(FallBloomMagRegime[["splits"]])
# FallBloomMagSplit1 <- FallBloomMagRegimeResults$index[1]
# FallBloomMagSplit2 <- FallBloomMagRegimeResults$index[2]
# 
# #Test for regime shifts in fall bloom duration (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# FallBloomDur <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, RangeDuration)
# FallBloomDurRegime <- rpart::rpart(RangeDuration~YEAR, data=FallBloomDur)
# #FallBloomDurRegimePlot <- rpart.plot::rpart.plot(FallBloomDurRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# FallBloomDurRegimeResults <- as.data.frame(FallBloomDurRegime[["splits"]])
# FallBloomDurSplit1 <- FallBloomDurRegimeResults$index[1]
# FallBloomDurSplit2 <- FallBloomDurRegimeResults$index[2]

#-------------------------------------------------------------------------------- 
#Average stomach fullness by Species, YEAR, EPU and sex for the year before
#This brings in stomach fullness data from allfh database (from StomFullnessData_allfh.R):
#average stomach fullness by EPU:
stomdata <- stom

#Creating Average Fall Stomach Fullness by year, STRATUM, species, sex
#Currently used for Condition GAM analyses and multi-model dataset:
AvgStomStrata <- stomdata %>% dplyr::filter(season == "FALL") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullStrata=(mean(stom_full)))

#Creating Average Fall Stomach Fullness by year, EPU, species, sex
# AvgStomFullEPU <- stomdata %>% dplyr::filter(season == "FALL") %>%
#   group_by(year, EPU, Species, pdsex) %>% 
#   mutate(AvgStomFullEPU=(mean(stom_full)))
# 
# #Test for GAMs analysis to see if spring stomach fullness predicts fall condition:
#Creating Average Spring Stomach Fullness by year, STRATUM, species, sex
AvgStomFullSpringStrata <- stom %>% dplyr::filter(season == "SPRING") %>%
  group_by(year, STRATUM, Species, pdsex) %>%
  mutate(AvgStomFullSpringStrata=(mean(stom_full)))

# change stomach data variables to merge with condition data:
# stom.data.EPU <- AvgStomFullEPU %>% dplyr::mutate(YEAR = year, SEASON = season, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
#   dplyr::distinct(YEAR, EPU, Species, SEX, .keep_all = TRUE) %>% dplyr::select(YEAR, EPU, Species, SEASON, SEX, AvgStomFullEPU)

#Fall stomach data by strata:
stom.data.strata <- AvgStomStrata %>% dplyr::mutate(YEAR = year, SEASON = season, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  dplyr::distinct(YEAR, STRATUM, Species, SEX, .keep_all = TRUE) %>% dplyr::select(YEAR, STRATUM, EPU, Species, SEASON, SEX, AvgStomFullStrata)

#Spring stomach data by strata as test for lagging in GAM:
stom.spring.strata <- AvgStomFullSpringStrata %>% dplyr::mutate(YEAR = year, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  dplyr::distinct(YEAR, STRATUM, Species, SEX, .keep_all = TRUE) %>% dplyr::select(YEAR, STRATUM, EPU, Species, SEX, AvgStomFullSpringStrata)


stom.data.strata$SEX <- as.factor(stom.data.strata$SEX)
stom.spring.strata$SEX <- as.factor(stom.spring.strata$SEX)

#merge stomach fullness into condition data:
#make sure allfh data includes STRATUM as factor with leading zero for merge
#merge by strata for Condition GAM and multi-model dataset:
#*******
AvgStom <- dplyr::left_join(FallBloomCond, stom.data.strata, by = c('YEAR', 'SEASON', 'STRATUM', 'EPU', 'Species', 'SEX'))

#spring stomach fullness merge by strata for Condition GAM lag:
# AvgStomSpr <- dplyr::left_join(FallBloomCond, stom.spring.strata, by = c('YEAR', 'STRATUM', 'EPU', 'Species', 'SEX')) %>%
#   select('YEAR', 'STRATUM', 'EPU', 'Species', 'sex','StockName', 'Stock', 'Survey',
#          'AvgRelCondStrata', 'AvgRelCondStrataSD', 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
#          'AvgLatStrata', 'AvgLonStrata', 'AvgBottomTempStrata',
#          'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall','CopepodSmallLarge',
#          'ZooplBiomassAnomaly', 'TotalCopepodsMillions', 'AvgStomFullSpringStrata') %>%
#   distinct()

#Old code for stomach data not from allfh:
#AvgStom <- CondCal %>% dplyr::group_by(Species, YEAR, EPU, sex) %>% dplyr::mutate(AvgStomFull=mean(AvgStomFullStrata, na.rm=TRUE))
##Can't get lag and mutate to work
#AvgStomLag <- AvgStom %>% dplyr::mutate(AvgStomLag1=(AvgStomFull %in% YEAR-1))
#AvgStomLag <- AvgStom %>% dplyr::lag(AvgStomLag1=(AvgStomFull, n=1)
#Clunky way of lagging but it works:
#Lagged stomach index by strata for Condition GAM:
A <- AvgStom %>% dplyr::select(YEAR, Species, STRATUM, EPU, SEX, AvgStomFullStrata)
B <- unique(A)
C <- B %>% dplyr::mutate(YEARstom= YEAR)
D <- C %>% dplyr::ungroup()
E <- D %>% dplyr::select(Species, YEARstom, STRATUM, EPU, SEX, AvgStomFullStratalag=AvgStomFullStrata)
Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
AvgStom2 <- AvgStom %>% dplyr::select(-c(AvgStomFullStrata))
AvgStomStrataLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("Species", "YEAR","STRATUM", "EPU", "SEX")) %>%
  dplyr::select('YEAR', 'CRUISE6', 'STRATUM', 'EPU', 'SEASON','Species', 'SVSPP','SEX',
                #'StockName', 'Survey',
                'StockUnit',
                # Use for condition data by strata:
                 'AvgRelCondStrata', 'AvgRelCondStrataSD', 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
                'AvgLatStrata', 'AvgLonStrata', 'AvgBottomTempStrata', 'AvgSurfaceTempStrata',
               # 'AvgRelCondYear', 'AvgRelCondYearSD', 'AvgExpcatchwtYear', 'AvgExpcatchnumYear',
                #'AvgLatYear', 'AvgLonYear', 'AvgBottomTempYear', 'AvgSurfaceTempYear',
                'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
                #'CalEPU',
                'CopepodSmallLarge',
                #      'CopepodSmallLargeStrataWinter', 'CopepodSmallLargeStrataSpring', 'CopepodSmallLargeStrataSummer', 'CopepodSmallLargeStrataFall',
                #        'CopepodSmallLargeSprStrata','CopepodSmallLargeFallStrata','CopepodSmallLargeAnnualStrata',
                #      'TotalCopepodStrataWinter', 'TotalCopepodStrataSpring', 'TotalCopepodStrataSummer', 'TotalCopepodStrataFall',
                #     'ZooplAbundStrataWinter', 'ZooplAbundStrataSpring','ZooplAbundStrataSummer', 'ZooplAbundStrataFall',
                'ZooplBiomassAnomaly', 'TotalCopepodsMillions',
                'RangeMagnitude', 'RangeDuration', 'AvgStomFullStratalag') %>%
  distinct()


#Lagged stomach index by tow for multi-model dataset:
 # A <- stom.data.strata %>% select(YEAR, Species, STRATUM, EPU, SEASON, SEX, AvgStomFullStrata)
 # B <- unique(A)
 # C <- B %>% dplyr::mutate(YEARstom= YEAR)
 # D <- C %>% dplyr::ungroup()
 # E <- D %>% dplyr::select(Species, YEARstom, STRATUM, EPU, SEASON, SEX, AvgStomFullStratalag=AvgStomFullStrata)
 # Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
 # AvgStom2 <- stom.data.strata %>% dplyr::select(-c(AvgStomFullStrata))

 # AvgStomTowLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("YEAR", "SEASON", "Species", "STRATUM", "EPU", "SEX")) %>%
 #   select('YEAR', 'SEASON','CRUISE6', 'STRATUM', 'STATION', 'TOW', 'BOTTEMP', 'LAT', 'LON', 'EPU', 'Species', 'SEX',
 #          'EXPCATCHWT', 'EXPCATCHNUM',
 #          'AvgTowRelCond', 'AvgTowRelCondSD', 'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
 #         'CalEPU', 'CopepodSmallLarge', 'ZooplBiomassAnomaly', 'AvgStomFullStratalag')
 # AvgStomTowLag <- dplyr::distinct(AvgStomTowLag)

#check merge rows by counting distinct rows:
DistRowsAvgStom2 <- nrow(dplyr::distinct(AvgStom2, YEAR, Species, STRATUM, EPU, SEASON, SEX))
#DistRowsStomlag <- nrow(dplyr::distinct(Stomlag, YEAR, Species, STRATUM, EPU, SEASON, SEX))

#Multi-model dataset:
#readr::write_csv(AvgStomTowLag, here::here(out.dir,"RelCondition_tow_EnvirIndices.csv"))

#------------------------------------------------------------------------------------ 
#Add stock assessment data from Stock SMART: https://www.fisheries.noaa.gov/resource/tool-app/stock-smart
#load(here::here("data","stockAssessmentData.Rdata"))
#load(here::here("data","stockAssessmentData.rda"))
#Updated pull May 28, 2021:
load(here::here("data","stockAssessmentData_05-28-2021.rda"))

#View(stockAssessmentData)
#missingArea <- distinct(stockAssessmentData,StockName, is.na(StockArea))

#2019 goosefish assessment has too few years to use, also 2019 assessment of GOM cod, 2018 assessment of herring,
#Also 2019 assessment of GB haddock, 2017 assessment of GOM winter flounder, 2019 assessment of GB YT
#cusk and blackbelly rosefish don't have n>=3 and years > 20 if only using condition within 1 standard deviation of mean:
#5/28/2021 pull from StockSMART is missing some StockAreas. Have to use StockName instead:
StockAssDat <- stockAssessmentData %>%
  dplyr::filter(StockName %in% c('Spiny dogfish - Atlantic Coast',
                                 'Winter skate - Georges Bank / Southern New England',
                                 'Little skate - Georges Bank / Southern New England',
                                 'Thorny skate - Gulf of Maine',
                                 'Smooth skate - Gulf of Maine',
                                 'Barndoor skate - Georges Bank / Southern New England',
                                 'Clearnose skate - Southern New England / Mid-Atlantic',
                                 'Rosette skate - Southern New England / Mid-Atlantic',
                                 'Atlantic herring - Northwestern Atlantic Coast',
                                 'Silver hake - Gulf of Maine / Northern Georges Bank',
                                 'Silver hake - Southern Georges Bank / Mid-Atlantic',
                                 'Atlantic cod - Gulf of Maine',
                                 'Atlantic cod - Georges Bank',
                                 'Haddock - Gulf of Maine',
                                 'Haddock - Georges Bank',
                                 'Pollock - Gulf of Maine / Georges Bank',
                                 'White hake - Gulf of Maine / Georges Bank',
                                 'Red hake - Gulf of Maine / Northern Georges Bank',
                                 'Red hake - Southern Georges Bank / Mid-Atlantic',
                                 'American plaice - Gulf of Maine / Georges Bank',
                                 'Summer flounder - Mid-Atlantic Coast',
                                 'Yellowtail flounder - Cape Cod / Gulf of Maine',
                                 'Yellowtail flounder - Georges Bank',
                                 'Yellowtail flounder - Southern New England / Mid-Atlantic',
                                 'Winter flounder - Gulf of Maine',
                                 'Winter flounder - Georges Bank',
                                 'Winter flounder - Southern New England / Mid-Atlantic',
                                 'Witch flounder - Northwestern Atlantic Coast',
                                 'Windowpane - Gulf of Maine / Georges Bank',
                                 'Windowpane - Southern New England / Mid-Atlantic',
                                 'Atlantic mackerel - Gulf of Maine / Cape Hatteras',
                                 'Butterfish - Gulf of Maine / Cape Hatteras',
                                 'Bluefish - Atlantic Coast',
                                 'Black sea bass - Mid-Atlantic Coast',
                                 'Scup - Atlantic Coast',
                                 'Acadian redfish - Gulf of Maine / Georges Bank',
                                 'Ocean pout - Northwestern Atlantic Coast',
                                 'Goosefish - Gulf of Maine / Northern Georges Bank',
                                 'Goosefish - Southern Georges Bank / Mid-Atlantic'
  ))

#stockSpp <- distinct(StockAssDat,StockName)

#   filter(CommonName %in% c('Spiny dogfish', 'Winter skate', 'Little skate',
#'                         #'Smooth dogfish' (not enough data when outliers removed)
#'                         'Thorny skate',
#'                         'Atlantic herring',
#'                         'Silver hake',
#'                         'Atlantic cod',
#'                         'Haddock',
#'                         'Pollock',
#'                         'White hake',
#'                         'Red hake',
#'                         'Spotted hake',
#'                         'American plaice',
#'                         'Summer flounder',
#'                         'Fourspot',
#'                         'Yellowtail flounder',
#'                         'Winter flounder',
#'                         'Witch flounder',
#'                         'Windowpane',
#'                         'Atlantic mackerel',
#'                         'Butterfish',
#'                         'Bluefish',
#'                         'Black sea bass',
#'                         'Scup',
#'                         'Weakfish',
#'                         'Acadian redfish',
#'                         'Sea raven',
#'                         'Ocean pout',
#'                         'Goosefish',
#' #                        'Cusk',
#'                         'Offshore hake',
#'                         'Roughtail stingray',
#'                         'Spiny butterfly ray',
#'                         'Smooth skate',
#'                         'Rosette skate',
#'                         'Clearnose skate',
#'                         'Barndoor skate',
#'                         'Bullnose ray',
#'                         'Bluntnose stingray',
#'                         'Longhorn sculpin',
#' #                        'Blackbelly rosefish',
#'                         'Atlantic croaker'))

#                       )) %>%
# filter(StockArea %in% c('Atlantic',
#                      'Gulf of Maine / Georges Bank',
#                      'Northwestern Atlantic Coast',
#                      'Gulf of Maine',
#                      'Gulf of Maine / Cape Hatteras',
#                      'Mid',
#                      'Atlantic Coast',
#                      'Gulf of Maine / Northern Georges Bank',
#                      'Southern Georges Bank / Mid',
#                      'Southern Georges Bank / Mid-Atlantic',
#                      'Georges Bank',
#                      'Georges Bank / Southern New England',
#                      'Southern New England / Mid',
#                      'Cape Cod / Gulf of Maine'))



#Find unique stocks to merge with condition data:
#stocks <- StockAssDat %>% select(Species, Region)
#stocksunique <- unique(stocks)
#View(stocksunique)

#by StockName after 5/28/20201 StockSMART pull:
StockAssDat$StockUnit[StockAssDat$StockName== 'Spiny dogfish - Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Winter skate - Georges Bank / Southern New England'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Little skate - Georges Bank / Southern New England'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Thorny skate - Gulf of Maine'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Smooth skate - Gulf of Maine'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Barndoor skate - Georges Bank / Southern New England'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Clearnose skate - Southern New England / Mid-Atlantic'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Rosette skate - Southern New England / Mid-Atlantic'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Atlantic herring - Northwestern Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Silver hake - Gulf of Maine / Northern Georges Bank'] <- 'N'
StockAssDat$StockUnit[StockAssDat$StockName=='Silver hake - Southern Georges Bank / Mid-Atlantic'] <- 'S'
StockAssDat$StockUnit[StockAssDat$StockName=='Atlantic cod - Gulf of Maine'] <- 'GOM'
StockAssDat$StockUnit[StockAssDat$StockName=='Atlantic cod - Georges Bank'] <- 'GB'
StockAssDat$StockUnit[StockAssDat$StockName== 'Haddock - Gulf of Maine'] <- 'GOM'
StockAssDat$StockUnit[StockAssDat$StockName== 'Haddock - Georges Bank'] <- 'GB'
StockAssDat$StockUnit[StockAssDat$StockName== 'Pollock - Gulf of Maine / Georges Bank'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName== 'White hake - Gulf of Maine / Georges Bank'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Red hake - Gulf of Maine / Northern Georges Bank'] <- 'N'
StockAssDat$StockUnit[StockAssDat$StockName=='Red hake - Southern Georges Bank / Mid-Atlantic'] <- 'S'
StockAssDat$StockUnit[StockAssDat$StockName=='American plaice - Gulf of Maine / Georges Bank'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Summer flounder - Mid-Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Yellowtail flounder - Cape Cod / Gulf of Maine'] <- 'CCGOM'
StockAssDat$StockUnit[StockAssDat$StockName=='Yellowtail flounder - Georges Bank'] <- 'GB'
StockAssDat$StockUnit[StockAssDat$StockName=='Yellowtail flounder - Southern New England / Mid-Atlantic'] <- 'SNEMA'
StockAssDat$StockUnit[StockAssDat$StockName=='Winter flounder - Gulf of Maine'] <- 'GOM'
StockAssDat$StockUnit[StockAssDat$StockName=='Winter flounder - Georges Bank'] <- 'GB'
StockAssDat$StockUnit[StockAssDat$StockName=='Winter flounder - Southern New England / Mid-Atlantic'] <- 'SNEMA'
StockAssDat$StockUnit[StockAssDat$StockName=='Witch flounder - Northwestern Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Windowpane - Gulf of Maine / Georges Bank'] <- 'N'
StockAssDat$StockUnit[StockAssDat$StockName=='Windowpane - Southern New England / Mid-Atlantic'] <- 'S'
StockAssDat$StockUnit[StockAssDat$StockName=='Atlantic mackerel - Gulf of Maine / Cape Hatteras'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Butterfish - Gulf of Maine / Cape Hatteras'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Bluefish - Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Black sea bass - Mid-Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Scup - Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Acadian redfish - Gulf of Maine / Georges Bank'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Ocean pout - Northwestern Atlantic Coast'] <- 'Unit'
StockAssDat$StockUnit[StockAssDat$StockName=='Goosefish - Gulf of Maine / Northern Georges Bank'] <- 'N'
StockAssDat$StockUnit[StockAssDat$StockName=='Goosefish - Southern Georges Bank / Mid-Atlantic'] <- 'S'

#Reformat StockSmart Regions to merge with StockUnit from stockStrataFall.csv:
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Atlantic'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine / Georges Bank' & StockAssDat$CommonName %in% c(
#    'Acadian redfish', 'American plaice', 'Pollock', 'White hake')] <- 'Unit' 
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine / Georges Bank' & StockAssDat$CommonName == 'Windowpane'] <- 'N'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Northwestern Atlantic Coast'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine' & StockAssDat$CommonName %in% c('Atlantic cod', 'Haddock', 'Winter flounder')] <- 'GOM'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine' & StockAssDat$CommonName %in% c('Thorny skate', 'Smooth skate')] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine / Cape Hatteras'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Mid'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Atlantic Coast'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Gulf of Maine / Northern Georges Bank'] <- 'N'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Southern Georges Bank / Mid'] <- 'S'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Georges Bank' & StockAssDat$CommonName %in% c(
#    'Atlantic cod', 'Haddock', 'Yellowtail flounder', 'Winter flounder')] <- 'GB'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Georges Bank / Southern New England'] <- 'Unit'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Southern New England / Mid' & StockAssDat$CommonName %in% c(
#    'Yellowtail flounder', 'Winter flounder')] <- 'SNEMA'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Southern New England / Mid' & StockAssDat$CommonName == 'Windowpane'] <- 'S'
#  StockAssDat$StockUnit[StockAssDat$StockArea=='Southern New England / Mid' & StockAssDat$CommonName %in% c('Clearnose skate', 'Rosette skate')] <- 'Unit'
# StockAssDat$StockUnit[StockAssDat$StockArea=='Cape Cod / Gulf of Maine'] <- 'CCGOM'

#From stockAssessmentData.rda pull with multiple assessment years, select most recent assessment for each year and metric:    
# StockAssYear <- StockAssDat %>%
#   group_by(Species, Region, Stock, Year, Metric) %>%
#   arrange(-AssessmentYear) %>%
#   slice(1) %>%
#   ungroup()

#2019 Opperational assessment for GB haddock only includes 2011-2018, so need to change to 2017 assessment year (not working yet):
if(StockAssDat$CommonName== 'Haddock' && StockAssDat$StockUnit =="GB" && StockAssDat$AssessmentYear == 2017) {maxAssyr$keep == 'y'}
#From stockAssessmentData.rda pull with multiple assessment years, select most recent assessment regardless of missing data for metrics:    
# maxAssyr=aggregate(StockAssDat$AssessmentYear, by=list('CommonName'=StockAssDat$CommonName, 'StockArea'=StockAssDat$StockArea),max)
# names(maxAssyr)[ncol(maxAssyr)]='AssessmentYear' 
# maxAssyr$keep='y'      
# 
# say=merge(StockAssDat, maxAssyr, by=c('CommonName', 'StockArea', 'AssessmentYear'), all.x=T)
# StockAssYear= subset(say, say$keep=='y')       
# 
# AssDat <- StockAssYear %>%
#   select(CommonName, StockArea, StockUnit, Year, AssessmentYear, Value, Metric) %>%
#   spread(Metric, Value) %>%
#   dplyr::mutate(YEAR = Year) %>%

#for 5/28/2021 data pull, CommonName and StockUnit are missing for many stocks, use StockName:
maxAssyr=aggregate(StockAssDat$AssessmentYear, by=list('StockName'=StockAssDat$StockName),max)
names(maxAssyr)[ncol(maxAssyr)]='AssessmentYear' 
maxAssyr$keep='y'      

say=merge(StockAssDat, maxAssyr, by=c('StockName', 'AssessmentYear'), all.x=T)
StockAssYear= subset(say, say$keep=='y')       

AssDat <- StockAssYear %>%
  dplyr::select(StockName, StockUnit, Year, AssessmentYear, Value, Metric) %>%
  tidyr::spread(Metric, Value) %>%
  dplyr::mutate(YEAR = Year) %>%
  #Sum total biomass (Abundance) across stocks if running GAMs by unit instead of StockUnit:
  #  group_by(Species, YEAR) %>%
  #  dplyr::mutate(TotalBiomass = sum(Abundance, na.rm=TRUE))
  #If by StockUnit:
  dplyr::mutate(TotalBiomass = Abundance) %>%
  #In 5-28-2021 StockSmart pull, have to rename StockName to Species:
  #select StockName up to '-' to get Species:
  dplyr::mutate(Species = sub("\\-.*", "", StockName))


#Catch/biomass as index of Fmort for Goosefish for GAMs:
AssDat$FproxyCatch <- (AssDat$Catch/AssDat$Abundance)
AssDat$Fproxy <- ifelse(is.na(AssDat$Fmort),AssDat$FproxyCatch,AssDat$Fmort) 

#Output Stock Assessment data as csv (examined missing data as .xls:
#readr::write_csv(AssDat, here::here(out.dir,"StockAssessmentData.csv"))
AssDat2 <- tibble::as_tibble(AssDat) %>%
  dplyr::mutate(Species = trimws(Species))
#Using Average stomach fullness lagged 1 year: 
#***********
CondStockAss <- dplyr::left_join(AvgStomStrataLag, AssDat2, by=c('Species', 'StockUnit', 'YEAR'))

#CondStockAss <- dplyr::left_join(FallBloomCond, AssDat2, by=c('Species', 'StockUnit', 'YEAR'))
#Using spring stomach fullness: 
#CondStockAss <- dplyr::left_join(AvgStomSpr, AssDat, by=c('Species', 'StockUnit', 'YEAR'))

#------------------------------------------------------------------------------------
#Weight at age coefficients from Kevin's GLM output: pt tab of glm_out_GrowthCovariates_species.xls
WAA <- readr::read_csv(here::here("data","GrowthCovariates_15species.csv"))
#Don't join on sex since F is changed to FALSE when importing csv into R:
CondWAAcoeff <- dplyr::left_join(CondStockAss, WAA, by=c('Species', 'YEAR' ,'SEASON'))
#Until StockSMART data is finalized:
#CondWAAcoeff <- dplyr::left_join(AvgStomStrataLag, WAA, by=c('Species', 'YEAR' ,'SEASON'))


#AvgStomSpring doesn't include SEASON so don't join to WAA


#---------------------------------------------------------------------------------
#Cold Pool indices summarized by Joe Caracappa from Zhuomin:
ColdPool <- readr::read_csv(here::here("data","V_max_Strata_ColdPool.csv"))

ColdP <- ColdPool %>% dplyr::mutate(YEAR=year, STRATUM = area, PropColumnColdPool = var.wgt.mean)

CondColdPool <- dplyr::left_join(CondWAAcoeff, ColdP, by=c('YEAR', 'STRATUM'))


#---------------------------------------------------------------------------------
#allfh data includes a better audit of food habits data and eliminates the need for these removals:


#Remove Bluefish since not enough spring stomach samples
#  CondClean <- AvgStomSpr%>%
#   dplyr::filter(!(Species == "Bluefish"))

###Remove outliers for GAM runs based on individual fish samples:
#Removed outlier where American Plaice stom_full >0.3, 
#Removed 4 outliers where Butterfish STOM_VOLUME >10,
#Removed 1 outlier where spotted hake EXPCATCHNUM >5000 
#%>% dplyr::filter(
#(is.na(AvgStomFullStrata) | !(Species == "American Plaice" & AvgStomFullStrata >0.3)),
#                                     (is.na(STOM_VOLUME) | !(Species == "Butterfish" & STOM_VOLUME >10)),
#                                       (is.na(EXPCATCHNUM) | !(Species == "Spotted Hake" & EXPCATCHNUM >5000)))


#--------------------------------------------------------------------------------
####GAM analyses relating condition to environmental parameters, by species:
#remove Sea Raven for LON/LAT runs since not enough data:
#CondClean <- CondClean %>% dplyr::filter(Species!="Sea Raven")

#For condition data with stomach fullness lagged 1 year data: 
#CondClean <- CondWAAcoeff

####If using total biomass (Abundance) or Fmort from StockSMART in GAMs, remove species lacking data: 
CondClean <- CondColdPool %>%
  filter(Species %in% c(
    'Smooth dogfish', #(not enough data once outliers removed)
    'Spiny dogfish',
    'Winter skate',
    'Little skate',
    'Thorny skate',
    'Atlantic herring',
    'Silver hake',
    'Atlantic cod',
    'Haddock',
    'Pollock',
    'White hake',
    'Red hake',
    'Spotted hake',
    'American plaice',
    'Summer flounder',
    'Fourspot',
    'Yellowtail flounder',
    'Winter flounder',
    'Witch flounder',
    'Windowpane',
    'Atlantic mackerel',
    'Butterfish',
    'Bluefish',
    'Black sea bass',
    'Scup',
    'Weakfish',
    'Acadian redfish',
    'Sea raven',
    'Ocean pout',
    'Offshore hake',
    'Smooth skate',
    'Rosette skate',
    'Clearnose skate',
    'Barndoor skate',
    'Goosefish',
    'Atlantic croaker',
    'Bluntnose stingray',
    'Bullnose ray',
    'Longhorn sculpin',
    'Roughtail stingray',
    'Spiny butterfly ray'))

# #Have to remove Atlantic herring, Atlantic cod, YT, Windowpane and mackerel to include AvgStomStrataLag or AvgStomSpringStrata (also removed bluefish) in condition mechanism run:
#'  CondClean <- CondWAAcoeff %>%      filter(Species %in% c('Smooth dogfish', 'Spiny dogfish', 'Winter skate', 'Little skate',
#'   'Thorny skate',
#' #  'Atlantic herring',
#' 'Silver hake',
#' #'Atlantic cod',
#' 'Haddock',
#' 'Pollock',
#' 'White hake',
#' 'Red hake',
#' 'Spotted hake',
#' 'American plaice',
#' 'Summer flounder',
#' 'Fourspot',
#' # 'Yellowtail flounder',
#' 'Winter flounder',
#' 'Witch flounder',
#' # 'Windowpane',
#' # 'Atlantic mackerel',
#' 'Butterfish',
#' #'Bluefish',
#' 'Black sea bass',
#' 'Weakfish',
#' 'Acadian redfish',
#' 'Sea raven',
#' 'Ocean pout',
#' 'Goosefish'))

#only select species with age data if using weight at age coefficients:
#If not using na.gam.replace (can't use with stomach data), remove species with missing coefficient data: 
#(Atlantic herring, cod, silver hake, white hake, mackerel, butterfish, black sea bass, scup, YT)
#Remove 1992 fall since no coefficient data:
#  CondClean <- CondWAAcoeff %>%
#    filter(Species %in% c(#'Atlantic herring',
# #                        'Silver hake',
# #                         'Atlantic cod',
#                          'Haddock',
#                          'Pollock',
# #                     'White hake',
#                          'American plaice',
#                          'Summer flounder',
# #                        'Atlantic mackerel',
# #                       'Butterfish',
# #                     'Black sea bass',
# #                      'Scup',
# #                         'Yellowtail flounder',
#                          'Winter flounder',
#                          'Witch flounder'),
#   YEAR > 1992)

###Filter out outliers for GAMs from strata averaged data:
#remove TotalCopepodsMillions >10000
#remove spiny dogfish with AvgExpcatchwtStrata >1500
#remove yellowtail with AvgExpcatchnumStrata > 600
# CondCleanTotCop <- CondClean %>%
#   dplyr::filter((is.na(TotalCopepodsMillions) | TotalCopepodsMillions < 10000))
#***********
# CondCleanSpDogWt <- CondClean %>%
#   dplyr::filter(is.na(AvgExpcatchwtStrata) | (!(Species == "Spiny dogfish" & AvgExpcatchwtStrata >1500)))
# 
# CondClean <- CondCleanSpDogWt %>%
#   dplyr::filter(is.na(AvgExpcatchnumStrata) | (!(Species == "Windowpane" & AvgExpcatchnumStrata >250)))


CondCleanSpDogWt <- CondClean %>%
#  dplyr::filter(is.na(BIOMASS) | (!(Species == "Spiny dogfish" & BIOMASS >1500)))
  dplyr::filter(is.na(AvgExpcatchwtStrata) | (!(Species == "Spiny dogfish" & AvgExpcatchwtStrata >1500)))

CondClean <- CondCleanSpDogWt %>%
#  dplyr::filter(is.na(ABUNDANCE) | (!(Species == "Windowpane" & ABUNDANCE >250)))
  dplyr::filter(is.na(AvgExpcatchnumStrata) | (!(Species == "Windowpane" & AvgExpcatchnumStrata >250)))

#####For GOM Haddock analyses comparing condition to commercial catch whole fish conversions:
# GOMhadd <- CondClean %>% 
#   dplyr::arrange(StockUnit) %>%
#   dplyr::filter(Species == 'Haddock') %>%
#   dplyr::select('YEAR', 'CRUISE6', 'STRATUM', 'EPU', 'SEASON', 'Species', 'sex', 'StockUnit', 'AvgRelCondStrata', 'AvgRelCondStrataSD')

#Output haddock condition:
#readr::write_csv(GOMhadd, here::here(out.dir,"FallStrata_RelCond_haddock.csv"))

#Test for colinearity of environmental data:
# select variables:
# EnvirVariables <- CondClean %>%
#   ungroup() %>%
#   dplyr::select('AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
#                 'AvgBottomTempStrata', 'AvgSurfaceTempStrata',
#                 'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
#                 'CopepodSmallLarge','ZooplBiomassAnomaly', 'TotalCopepodsMillions', 
#                 #            'CopepodSmallLargeSprStrata', 'CopepodSmallLargeFallStrata', 'CopepodSmallLargeAnnualStrata',
#                 # 'CopepodSmallLargeStrataWinter', 'CopepodSmallLargeStrataSpring', 'CopepodSmallLargeStrataSummer', 'CopepodSmallLargeStrataFall',
#                 # 'TotalCopepodStrataWinter', 'TotalCopepodStrataSpring', 'TotalCopepodStrataSummer', 'TotalCopepodStrataFall',
#                 # 'ZooplAbundStrataWinter', 'ZooplAbundStrataSpring','ZooplAbundStrataSummer', 'ZooplAbundStrataFall',
#                 'AvgStomFullStratalag', 
#                 #             'Fproxy', 'TotalBiomass', 
#                 'RangeMagnitude','RangeDuration',
#                 'PropColumnColdPool', 'AvgLatStrata', 'AvgLonStrata', 'YEAR') %>%
#   dplyr::rename('Local Biomass'='AvgExpcatchwtStrata', 'Local Abundance'= 'AvgExpcatchnumStrata',
#                 'Local Bottom Temp'= 'AvgBottomTempStrata', 'Local Surface Temp'= 'AvgSurfaceTempStrata',
#                 'Winter Temp'= 'AvgTempWinter',
#                 'Spring Temp'= 'AvgTempSpring', 'Summer Temp'= 'AvgTempSummer',
#                 'Fall Temp'= 'AvgTempFall', 
#                 #         'CopepodSmall_Large'= 'CopepodSmallLarge',
#                 'Zooplankton Biomass'= 'ZooplBiomassAnomaly', 'Total Copepods'= 'TotalCopepodsMillions', 
#                 #        'CopepodSmLg_SprStrata'= 'CopepodSmallLargeSprStrata', 
#                 #       'CopepodSmLg_FallStrata'= 'CopepodSmallLargeFallStrata', 
#                 #      'CopepodSmLg_AnnualStrata'= 'CopepodSmallLargeAnnualStrata',
#                 'Stomach Fullness'= 'AvgStomFullStratalag',  
#                 #    'Stock Biomass'= 'TotalBiomass',
#                 'Fall Bloom Magnitude'= 'RangeMagnitude', 'Fall Bloom Duration'= 'RangeDuration',
#                 'Prop Column Cold Pool'= 'PropColumnColdPool', 'Average Lat by Strata' = 'AvgLatStrata',
#                 'Average Lon by Strata' = 'AvgLonStrata', 'Year' = 'YEAR')
# 
#Correlation matrix:
#EnVarCor <- cor(EnvirVariables, use = "complete.obs")
#::write_csv(as.data.frame(EnVarCor), here::here(out.dir,"Corr_Env.csv"))

#Environmental covariates for NRHA:
#
# #Environmental covariates for NRHA:
# HabitatAssess <- CondClean %>%
#   ungroup() %>%
#   dplyr::select('YEAR', 'CRUISE6', 'STRATUM', 'EPU', 'SEASON', 'Species', 'SVSPP', 'sex', 'StockName', 'StockUnit',
#                 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
#                 'AvgBottomTempStrata','AvgSurfaceTempStrata','AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
#                 'CopepodSmallLarge','ZooplBiomassAnomaly', 'TotalCopepodsMillions', 
#                 'AvgStomFullStratalag', 'Fproxy', 'TotalBiomass', 'RangeMagnitude','RangeDuration',
#                 'PropColumnColdPool', 'AvgLatStrata', 'AvgLonStrata') %>%
#   dplyr::rename('Local Biomass'='AvgExpcatchwtStrata', 'Local Abundance'= 'AvgExpcatchnumStrata',
#                 'Local Bottom Temp'= 'AvgBottomTempStrata', 'Local Surface Temp' = 'AvgSurfaceTempStrata', 
#                 'Winter Temp'= 'AvgTempWinter',
#                 'Spring Temp'= 'AvgTempSpring', 'Summer Temp'= 'AvgTempSummer',
#                 'Fall Temp'= 'AvgTempFall', 'Copepod Small/Large'= 'CopepodSmallLarge',
#                 'Zooplankton Biomass'= 'ZooplBiomassAnomaly', 'Total Copepods'= 'TotalCopepodsMillions', 
#                 'Stomach Fullness'= 'AvgStomFullStratalag',  'Stock Biomass'= 'TotalBiomass',
#                 'Fall Bloom Magnitude'= 'RangeMagnitude', 'Fall Bloom Duration'= 'RangeDuration',
#                 'Prop Column Cold Pool'= 'PropColumnColdPool', 'Average Lat by Strata' = 'AvgLatStrata',
#                 'Average Lon by Strata' = 'AvgLonStrata', 'Year' = 'YEAR')
# 
# readr::write_csv(HabitatAssess, here::here(out.dir,"EnvirCov_HabitatAssess_Jan2022.csv"))

# #Environmental covariates by EPU for Scott Large Dynamic Factor Analysis:
# DFAdata <- ZoopDataEPU %>%
#   ungroup() %>%
#   dplyr::select('YEAR', 'CRUISE6', 'EPU', 'SEASON', 'Species', 'SVSPP', 
#                 'AvgRelCondEPU', 'AvgRelCondEPUSD',
#                 'AvgExpcatchwtEPU', 'AvgExpcatchnumEPU',
#                 'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
#                 'CopepodSmLgSpringEPU','CopepodSmLgSummmerEPU','CopepodSmLgFallEPU',
#                 'CopepodSmLgWinterEPU', 'TotCopSpringEPU','TotCopSummerEPU',
#                 'TotCopFallEPU','TotCopWinterEPU', 'ZoopAbundSpringEPU', 
#                 'ZoopAbundSummerEPU', 'ZoopAbundFallEPU', 'ZoopAbundWinterEPU')
#                 # 'Fproxy', 'TotalBiomass', 'RangeMagnitude','RangeDuration',
#                 # 'PropColumnColdPool') 
# # %>%
# #   dplyr::rename('Local Biomass'='AvgExpcatchwtStrata', 'Local Abundance'= 'AvgExpcatchnumStrata',
# #                 'Local Bottom Temp'= 'AvgBottomTempStrata', 'Local Surface Temp' = 'AvgSurfaceTempStrata',
# #                 'Winter Temp'= 'AvgTempWinter',
# #                 'Spring Temp'= 'AvgTempSpring', 'Summer Temp'= 'AvgTempSummer',
# #                 'Fall Temp'= 'AvgTempFall', 'Copepod Small-Large'= 'CopepodSmallLarge',
# #                 'Zooplankton Biomass'= 'ZooplBiomassAnomaly', 'Total Copepods'= 'TotalCopepodsMillions',
# #                 'Stock Biomass'= 'TotalBiomass',
# #                 'Fall Bloom Magnitude'= 'RangeMagnitude', 'Fall Bloom Duration'= 'RangeDuration',
# #                 'Prop Column Cold Pool'= 'PropColumnColdPool', 'Year' = 'YEAR')
# 
# readr::write_csv(DFAdata, here::here(out.dir,"FishCondition_EnvirCov_DFA2022.csv"))
# saveRDS(DFAdata,file = here::here("other",paste0("FishCondition_EnvirCov_DFA2022.rds")))

#Environmental covariates by Year for Rob Gamble EDM:
#EDM for mature mackerel >23cm:
annualcond <- cond.epu  %>% dplyr::filter(Species == 'Atlantic mackerel', LENGTH > 23, YEAR >= 1992)  %>%
  dplyr::group_by(YEAR) %>% dplyr::summarize(MatureMackerelCond = mean(RelCond), MatMackStdDevCond = sd(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()

#from MAB fall Zooplankton anomaly section of RegimeShifts_EnvirVar.R
#MAB fall Zooplankton anomaly from Ryan Morse:
MABseasonZooAbund <- readr::read_csv(here::here(data.dir, "MAB_mean_seasonal_anomalies.csv"))

MABfallZoop <- MABseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Fall') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, tlong_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

EDMzoop <- MABfallZoop %>% dplyr::select(SumZoop, year) %>%
 dplyr::rename(MABfallZoopAnom = SumZoop, YEAR = year)

EDMdataZoop <- dplyr::full_join(annualcond, EDMzoop, by='YEAR')

#Surfdata from RegimeShifts_EnvirVar.R:
#SurfTemp <-cond.epu %>% dplyr::group_by(YEAR) %>% dplyr::summarize(SpringSurfTemp = mean(!is.na(SURFTEMP)))
#Regime shifts in surface temp:
Surfdata <- cond.epu %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, SURFTEMP) %>%
  dplyr::filter(!is.na(SURFTEMP)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(AvgSurfTemp = mean(SURFTEMP))

EDMdataTemp <- dplyr::full_join(EDMdataZoop, Surfdata, by='YEAR')

#Bring in small minus large copepod size structure data:
CopepodEPUdata <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, CopepodSmallLarge) %>% group_by(EPU)

EDMdataTempCop <- EDMdataTemp <- dplyr::full_join(EDMdataTemp, CopepodEPUdata, by='YEAR', 'EPU')

EDMdata <- EDMdataTempCop %>% unique() %>% dplyr::filter(YEAR >= 1992)

#readr::write_csv(DFAdata, here::here(out.dir,"FishCondition_EnvirCov_DFA2022.csv"))
saveRDS(EDMdata,file = here::here("other",paste0("MackerelCondition_EDM2022.rds")))

#EDM for immature mackerel <=23cm:
annualcondImm <- cond.epu  %>% dplyr::filter(Species == 'Atlantic mackerel', LENGTH <= 23, YEAR >= 1992)  %>%
  dplyr::group_by(YEAR) %>% dplyr::summarize(ImmatureMackerelCond = mean(RelCond), ImmMackStdDevCond = sd(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()


#from GOM summer Zooplankton anomaly section of RegimeShifts_EnvirVar.R
#Mean GOM zooplankton abundance anomalies from Ryan Morse (GOM_mean_seasonal_anomalies.csv)
GOMseasonZooAbund <- readr::read_csv(here::here(data.dir, "GOM_mean_seasonal_anomalies.csv"))

GOMsummerZoop <- GOMseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Spring') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, mlucens_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

EDMzoopGOM <- GOMsummerZoop %>% dplyr::select(SumZoop, year) %>%
  dplyr::rename(GOMsummerZoopAnom = SumZoop, YEAR = year)

EDMdataZoopImm <- dplyr::full_join(annualcondImm, EDMzoopGOM, by='YEAR')

#AvgTempSpringData from line 120 of this code:
EDMtempSpr <- AvgTempSpringData %>% dplyr::rename('YEAR'='Year', 'GOM_AvgTempSpring'='GOM',
                                                  'GB_AvgTempSpring'='GB',
                                                  'MAB_AvgTempSpring'='MAB',
                                                  'SS_AvgTempSpring'='SS')

EDMdataImm2 <- dplyr::full_join(EDMtempSpr, EDMdataZoopImm, by='YEAR')

EDMdataImm3 <- EDMdataTemp <- dplyr::full_join(EDMdataImm2, CopepodEPUdata, by='YEAR', 'EPU')

EDMdataImm <- EDMdataImm3 %>% unique() %>% dplyr::filter(YEAR >= 1992)

saveRDS(EDMdataImm,file = here::here("other",paste0("ImmMackerel_FishCondition_EDM2022.rds")))

#Attempting to select values less than -0.3 or greater than 0.3 but not working:
# EnVarCorSig <- EnVarCor %>% filter(('AvgExpcatchwtStrata' < -0.3 | 'AvgExpcatchwtStrata' > 0.3) |
#                                    ('AvgExpcatchnumStrata'< -0.3 | 'AvgExpcatchnumStrata' > 0.3) |
#                                    ('AvgBottomTempStrata'< -0.3 | 'AvgBottomTempStrata' > 0.3) |
#                                     ('AvgTempWinter'< -0.3 | 'AvgTempWinter' > 0.3) |
#                                     ('AvgTempSpring'< -0.3 | 'AvgTempSpring' > 0.3) |
#                                     ('AvgTempSummer'< -0.3 | 'AvgTempSummer' > 0.3) |
#                                     ('AvgTempFall'< -0.3 | 'AvgTempFall' > 0.3) |
#                                    ('CopepodSmallLarge'< -0.3 | 'CopepodSmallLarge' > 0.3) |
#                                      ('ZooplBiomassAnomaly'< -0.3 | 'ZooplBiomassAnomaly' > 0.3) |
#                                      ('TotalCopepodsMillions'< -0.3 | 'TotalCopepodsMillions' > 0.3) | 
#                                    ('AvgStomFullStratalag'< -0.3 | 'AvgStomFullStratalag' > 0.3) |
#                                      ('Fproxy'< -0.3 | 'Fproxy' > 0.3) |
#                                      ('TotalBiomass'< -0.3 | 'TotalBiomass' > 0.3))

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
#Test for significance of covariation:
# cor.mtest <- function(mat, ...) {
#   mat <- as.matrix(mat)
#   n <- ncol(mat)
#   p.mat<- matrix(NA, n, n)
#   diag(p.mat) <- 0
#   for (i in 1:(n - 1)) {
#     for (j in (i + 1):n) {
#       tmp <- cor.test(mat[, i], mat[, j], ...)
#       p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#     }
#   }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# # matrix of the p-value of the correlation
# p.mat <- cor.mtest(EnvirVariables)
# #head(p.mat[, 1:5])
# 
# #Plot correlation matrix with ellipses showing neg or positive trend, not plotting indices that have p>0.001 significance:
#  CorrPlotEnVar <- here::here(out.dir,"CorrelationPlot_EnvirVariables_bloomLatYear2021.png")
#  png(CorrPlotEnVar, type = "cairo", width = 420, height = 420)
#  corrplot(EnVarCor, method= "ellipse", type="upper", order="hclust",
#   #       addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, tl.cex=0.75, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.001, insig = "blank",
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE )
# dev.off()
# 
#Send Andy condSPP:
condSPP <-  CondClean %>% dplyr::rename(
                                         'LocalBiomass'='AvgExpcatchwtStrata', 'LocalAbundance'= 'AvgExpcatchnumStrata',
                                        # by Year:
                                        'LocalBottomTemp'= 'AvgBottomTempStrata','LocalSurfaceTemp'='AvgSurfaceTempStrata',
                                        'WinterTemp'= 'AvgTempWinter',
                                        'SpringTemp'= 'AvgTempSpring', 'SummerTemp'= 'AvgTempSummer',
                                        'FallTemp'= 'AvgTempFall', 
                                        'CopepodSmall_Large'= 'CopepodSmallLarge',
                                        'ZooplanktonBiomass'= 'ZooplBiomassAnomaly', 'TotalCopepods'= 'TotalCopepodsMillions', 
                                        #'CopepodSmLg_SprStrata'= 'CopepodSmallLargeSprStrata', 
                                        #'CopepodSmLg_FallStrata'= 'CopepodSmallLargeFallStrata', 
                                        #'CopepodSmLg_AnnualStrata'= 'CopepodSmallLargeAnnualStrata',
                                        'StomachFullness'= 'AvgStomFullStratalag',  
                                        #                          'StockBiomass'= 'TotalBiomass',
                                        'FallBloomMagnitude'= 'RangeMagnitude', 'FallBloomDuration'= 'RangeDuration',
                                        'PropColumnColdPool'= 'PropColumnColdPool', 
                                        'AverageLatStrata' = 'AvgLatStrata',
                                         'AverageLonStrata' = 'AvgLonStrata')
                                       # by year:
                                       # 'AverageLatYear' = 'AvgLatStrata',
                                       # 'AverageLonYear' = 'AvgLonStrata')

#saveRDS(condSPP,file = here::here(out.dir,paste0("condSPP.rds")))
#*********
saveRDS(condSPP,file = here::here("other",paste0("condSPP.rds")))

#saveRDS(condSPP,file = here::here("other",paste0("condSPP_Year.rds")))

spp <- unique(CondClean$Species)
datalist = list()

#}
######End code before GAM analyses#####

#Run GAMS by species and StockUnit (not working):
#SppStockUnit <- unique(CondClean$Species, CondClean$StockUnit)

# 
# for(sp in spp) {
#   condSPP <- CondClean %>% dplyr::filter(Species==sp)
#   
#   #Rename columns:
#   condSPP <-  condSPP %>% dplyr::rename('LocalBiomass'='AvgExpcatchwtStrata', 'LocalAbundance'= 'AvgExpcatchnumStrata',
#                              'LocalBottomTemp'= 'AvgBottomTempStrata','WinterTemp'= 'AvgTempWinter',
#                              'SpringTemp'= 'AvgTempSpring', 'SummerTemp'= 'AvgTempSummer',
#                              'FallTemp'= 'AvgTempFall', 'CopepodSmall/Large'= 'CopepodSmallLarge',
#                              'ZooplanktonBiomass'= 'ZooplBiomassAnomaly', 'TotalCopepods'= 'TotalCopepodsMillions', 
#                              'StomachFullness'= 'AvgStomFullStratalag',  'StockBiomass'= 'TotalBiomass',
#                              'FallBloomMagnitude'= 'RangeMagnitude', 'FallBloomDuration'= 'RangeDuration',
#                              'PropColumnColdPool'= 'PropColumnColdPool', 'AverageLatStrata' = 'AvgLatStrata',
#                              'AverageLonStrata' = 'AvgLonStrata')
# 
#   
#   #set up for 2020 data with only representative tows, using variables with <0.3 covariance:
#  
#   #Fproxy and Total Biomass removing variables correlated >0.3 and adding shrinkage smoothers (bs="ts"):
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, bs="ts", k=10) +s(AvgTempWinter, bs="ts", k=10) +s(AvgExpcatchwtStrata, bs="ts", k=10) +s(AvgExpcatchnumStrata, bs="ts", k=10) +s(TotalBiomass, bs="ts", k=10) +s(Fproxy, bs="ts", k=10) +s(CopepodSmallLarge, bs="ts", k=10), data=condSPP)
#   #Same as above without shrinkage smoothers:
# #     form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgTempWinter, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10) +s(CopepodSmallLarge, k=10), data=condSPP)
# # #   
# # #    #na.gam.replace works if not including stomach data:  
# # #    #Including null space penalization (method="REML", select=T), which selects out variables with poor diviance explained:
# # #    #  condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, method="REML", select=T, na.action = na.gam.replace)
# # #    #Without null space penalization:
# #     condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, na.action = na.gam.replace)
# # #    
# #     GAMstats <- summary(condGAM)
# # #    #Trying to add in AIC but wasn't included in the output: 
# # #    #AIC(condGAM)
# # #    
# #    SumCondGAM <- t(c(sp, round(GAMstats$s.pv,3),  round(GAMstats$r.sq,3), round(GAMstats$dev.expl,3),  round(GAMstats$sp.criterion,3), GAMstats$n))
# # #    
# #     dl=data.frame(SumCondGAM)
# # #    
# # #    #Fproxy and Total Biomass removing variables correlated >0.3:
# #    GAMnames=c('Species', 'Bottom Temp Strata', 'Average Winter Bottom Temp', 'Local Biomass', 'Local Abundance',
# #               'Total Biomass', 'Fproxy', 'Copepod Small/Large Ratio', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# # #    
# # #    #error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
# #     names(dl)=GAMnames
# #     datalist[[sp]] <- dl
# # #   
# #    filename <- here::here(out.dir,paste0(sp,"_Mechanisms_NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBiomass_LocalAbundance_TotalBiomass_Fproxy_CopepodSmLrg_AvgCondStrata.jpg"))
# # #    
# #    jpeg(filename)
# #    #Getting error that margins too large when running gam.check single species with WAA, set mar =c(1,1,,):
# #    par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
# #    #   par(mfrow=c(2,2), mar=c(1,1,1,1), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
# #    plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
# #    dev.off()
# # 
# #    sink(here::here(out.dir,paste0(sp,"_GAMcheck_NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBio_LocalAbund_TotalBiomass_Fproxy_CopepodSmallLarge_AvgCondStrata.txt")))
# # 
# #    mgcv::gam.check(condGAM)
# #    mgcv.helper::vif.gam(condGAM)
# # #       
# #     sink()
# # # 
# #  }
# # # 
# #  AllSPP = do.call(rbind, datalist)
# # # 
# #  readr::write_csv(AllSPP, here::here(out.dir,"Mechanisms_2021NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBio_LocalAbund_TotalBiomass_Fproxy_CopepodSmLrg_AvgCondStrata.csv"))
# # # 
# #    
#    
#   
#   #turn on for testing a single species outside of loop:
#   #condSPP <- CondClean %>% dplyr::filter(Species=='Atlantic cod') %>% mutate(sp='Atlantic cod')
#   
#   #Full model
#   #   form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring) +s(YEAR), data=condSPP)
#   #Single index
# #  form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
#   # form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10), data=condSPP)
# #   form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHWT, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchwtStrata, k=10), data=condSPP)
#  #   form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchnumStrata, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHNUM, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(LON, LAT, k=25), data=condSPP)
# #   form.cond <- formula(AvgRelCondStrata ~ s(AvgLonStrata, AvgLatStrata, k=25), data=condSPP)
#   
#   # form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStrata, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStratalag, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullSpringStrata, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(CopepodSmallLarge, k=10), data=condSPP)
#  #  form.cond <- formula(AvgRelCondStrata ~ s(ZooplBiomassAnomaly, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(TotalCopepodsMillions, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSpring, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSummer, k=10), data=condSPP)
# #    form.cond <- formula(AvgRelCondStrata ~ s(AvgTempFall, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempWinter, k=10), data=condSPP)
#     form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
#   #form.cond <- formula(AvgRelCondStrata ~ s(Abundance, k=10), data=condSPP)
#  # form.cond <- formula(AvgRelCondStrata ~ s(TotalBiomass, k=10), data=condSPP)
#  # form.cond <- formula(AvgRelCondStrata ~ s(Fproxy, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(PropColumnColdPool, k=10), data=condSPP)
#   
#   
#   
#   #Eplains highest deviance:
# #    form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10) +s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgLonStrata, AvgLatStrata, k=25) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   
#   #Mechanisms model:
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #Not enough non-NAs:
#   # form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10) +s(Fproxy, k=10), data=condSPP)
#   
#   #Mechanism models selecting Expcatwt/Expcatnum, AvgStomFullStratalag/AvgStomFullSpringStrata, AvgTempSpring/AvgTempSummer/AvgTempFall, CopepodSmallLarge/ZooplBiomassAnomaly based on lowest p-values and if both zero, highest deviance explained:
#   ##For Smooth dogfish:
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##cod full mechanisms (would have to remove AvgStomFullStratalag to run for cod Deviance Explained = 0.91):
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##cod (previous run Deviance Explained = 0.125, can't get to run for cod now!):
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##summer flounder and red hake:
#   #   form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ## Spiny dogfish, silver hake (had to reduce K to 3 since got Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#   #A term has fewer unique covariate combinations than specified maximum degrees of freedom):
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchwtStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(CopepodSmallLarge, k=3) +s(AvgTempSummer, k=3), data=condSPP)
#   ##white hake:
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchwtStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(CopepodSmallLarge, k=3) +s(AvgTempFall, k=3), data=condSPP)
#   ##Winter skate:  
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##Witch flounder and bluefish:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##Sea raven:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##Acadian redfish:
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##goosefish:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##Little skate:
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchnumStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(ZooplBiomassAnomaly, k=3) +s(AvgTempFall, k=3), data=condSPP)
#   ##haddock:
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##American plaice, black sea bass
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##winter flounder:
#   #     form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##ocean pout:
#   ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##Spotted hake and weakfish:
#   ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##Thorny skate:
#   ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatcwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##Butterfish:
#   ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##Atlantic herring:
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   ##pollock, yellowtail:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##mackerel:
#   ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   
#   
#   #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
#   #(winter skate above)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #(Witch flounder and bluefish above):
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #(Sea raven above):
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##Acadian redfish:
#   #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##summer flounder and red hake:
#   #   form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   ##For Smooth dogfish:
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##goosefish:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #Remove stomach fullness for herring:
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   #Remove stomach fullness for cod:
#   #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #Remove stomach fullness for yellowtail:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #Remove stomach fullness for mackerel:
#   #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   
#   #With Fproxy and Total Biomass:
#  # form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   
#   #Fproxy and Total Biomass removing variables correlated >0.3 and adding shrinkage smoothers (bs="ts"):
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, bs="ts", k=10) +s(AvgTempWinter, bs="ts", k=10) +s(AvgExpcatchwtStrata, bs="ts", k=10) +s(AvgExpcatchnumStrata, bs="ts", k=10) +s(TotalBiomass, bs="ts", k=10) +s(Fproxy, bs="ts", k=10) +s(CopepodSmallLarge, bs="ts", k=10), data=condSPP)
#  #Same as above without shrinkage smoothers:
# #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgTempWinter, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10) +s(CopepodSmallLarge, k=10), data=condSPP)
#   
#   
#   #-----------------------------
#   ####For weight at age coefficients instead of condition GAMs:
#   #(winter skate above)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #(Witch flounder and bluefish above):
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #(Sea raven above):
#   #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#   #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   ##Acadian redfish:
#   #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##summer flounder and red hake:
#   #   form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   ##For Smooth dogfish:
#   #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
#   ##goosefish:
#   #    form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   
#   #Remove stomach fullness for herring:
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
#   #Remove stomach fullness for cod:
#   #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #Remove stomach fullness for yellowtail:
#   #    form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#   #Remove stomach fullness for mackerel:
#   #   form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=15) +s(AvgTempSpring, k=15), data=condSPP)
#   
#   
#   #Can add factor variable as a by variable: e.g. +s(LON, LAT, k=25, by = EPU)
#   #EXPCATCHWT had slightly more significance than EXPCATCHNUM for more species
#   
#   #na.gam.replace works if not including stomach data:  
#   #Including null space penalization (method="REML", select=T), which selects out variables with poor diviance explained:
# #  condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, method="REML", select=T, na.action = na.gam.replace)
#  #Without null space penalization:
#    condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, na.action = na.gam.replace)
#     
#   #    step.cond <- step.Gam(condGAM, scope= list("BOTTEMP" =~1+BOTTEMP+s(BOTTEMP),
#   #                                              "EXPCATCHNUM" =~1+EXPCATCHNUM+s(EXPCATCHNUM),
#   #                                              "LON, LAT" =~1+LON,LAT +s(LON,LAT),
#   #                                              "YEAR" =~1+YEAR))
#   
#    GAMstats <- summary(condGAM)
#   #  #Trying to add in AIC but wasn't included in the output: 
#   # #AIC(condGAM)
#   # 
#    SumCondGAM <- t(c(sp, round(GAMstats$s.pv,3),  round(GAMstats$r.sq,3), round(GAMstats$dev.expl,3),  round(GAMstats$sp.criterion,3), GAMstats$n))
#   # 
#    dl=data.frame(SumCondGAM)
#   #Full model output:
#   #GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'LON LAT', 'AvgStomFullLag', 'CopepodSL', 'AvgTempSpring', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #Full Model with reasonable mechanisms relating to condition changes:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'AvgStomFullLag', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #Not enough non-NAs:
#   # GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'AvgStomFullLag', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'Total Biomass', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #For Smooth dogfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##cod (have to remove AvgStomFullStratalag to run for cod):
# #  GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##summer flounder and red hake:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ## Spiny dogfish, silver hake:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##white hake:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Winter skate:  
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Witch flounder and bluefish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Sea raven:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Acadian redfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##fourspot, windowpane:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##goosefish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Little skate:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##haddock:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##American plaice, black sea bass
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##winter flounder:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##ocean pout:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Spotted hake and weakfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Thorny skate:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Butterfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Atlantic herring:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##pollock, yellowtail:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##mackerel:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
#   #(winter skate above)
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #(Witch flounder and bluefish above):
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #(Sea raven above):
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##Acadian redfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##summer flounder and red hake:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   ##For Smooth dogfish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   ##goosefish:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #Removed stomach fullness for herring:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #Remove stomach fullness for cod:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #Remove stomach fullness for yellowtail:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #Remove stomach fullness for mackerel:
#   #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# 
#     #With Fproxy and Total Biomass:
#  # GAMnames=c('Species', 'Bottom Temp Strata', 'AvgExpcatchwtStrata', 'Total Biomass', 'Fproxy', 'CopepodSmallLarge', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'Bottom Temp Strata', 'Total Biomass', 'Fproxy', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #Fproxy and Total Biomass removing variables correlated >0.3:
#   # GAMnames=c('Species', 'Bottom Temp Strata', 'Average Winter Bottom Temp', 'Local Biomass', 'Local Abundance',
#   #            'Total Biomass', 'Fproxy', 'Copepod Small/Large Ratio', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   
#    #Model with highest deviance explained:
#   #GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   
#   #single variable runs
# # GAMnames=c('Species', 'AvgStomFullSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# # GAMnames=c('Species', 'AvgStomFullLag', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   GAMnames=c('Species', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'AvgTempWinter', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#   #GAMnames=c('Species', 'ZooplBiomassAnomaly', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'CopepodSmallLarge', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'TotalCopepodsMillions', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#  #  GAMnames=c('Species', 'Bottom Temp Strata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #   GAMnames=c('Species', 'AvgExpcatchwtStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #  GAMnames=c('Species', 'AvgExpcatchnumStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#  #   GAMnames=c('Species', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #   GAMnames=c('Species', 'Total Biomass', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #   GAMnames=c('Species', 'AvgLonStrata AvgLatStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# #   GAMnames=c('Species', 'PropColumnColdPool', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#     
#   #error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
#    names(dl)=GAMnames
#    datalist[[sp]] <- dl
#   # 
#   #Use for testing plot with single species
#   #filename <-here::here(out.dir, paste0('GoosefishYEAR_condition.jpg'))
#   
#   #Single variable output:
#   #2021 icludes survdat data with corrected calibration coefficients and Wigley et al. L-W params:
#  #    filename <- here::here(out.dir,paste0(sp,"_StomFullSpringStrata2020_AvgCondStrata.jpg"))
#  #    filename <- here::here(out.dir,paste0(sp,"_StomFullStrataLag2020_AvgCondStrata.jpg"))
#      filename <- here::here(out.dir,paste0(sp,"2021NoOutiers_YEAR_AvgCondStrata.jpg"))
# #   filename <- here::here(out.dir,paste0(sp,"2021_LatLon_AvgCondStrata.jpg"))
#    #   filename <- here::here(out.dir,paste0(sp,"_AvgTempSpring_AvgCondStrata.jpg"))
#   #   filename <- here::here(out.dir,paste0(sp,"_AvgTempSummer_AvgCondStrata.jpg"))
#   #   filename <- here::here(out.dir,paste0(sp,"_AvgTempFall_AvgCondStrata.jpg"))
#   #   filename <- here::here(out.dir,paste0(sp,"_AvgTempWinter_AvgCondStrata.jpg"))
#   #    filename <- here::here(out.dir,paste0(sp,"_ZooplBiomassAnomaly_AvgCondStrata.jpg"))
#   #    filename <- here::here(out.dir,paste0(sp,"_CopepodSmallLarge_AvgCondStrata.jpg"))
#   #   filename <- here::here(out.dir,paste0(sp,"_TotalCopepods_AvgCondStrata.jpg"))
#  #  filename <- here::here(out.dir,paste0(sp,"_2021_BottomTempStrata_AvgCondStrata.jpg"))
#  #     filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchwtStrata_AvgCondStrata.jpg"))
# #  filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchnumStrata_AvgCondStrata.jpg"))
# #  filename <- here::here(out.dir,paste0(sp,"_Fproxy_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_TotalBiomass_AvgCondStrata.jpg"))
# #  filename <- here::here(out.dir,paste0(sp,"_TotalBiomass2020_AvgCondStrata.jpg"))
# #  filename <- here::here(out.dir,paste0(sp,"_Fproxy2020_AvgCondStrata.jpg"))
# #  filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchwtStrata2020_AvgCondStrata.jpg"))
# #   filename <- here::here(out.dir,paste0(sp,"_BottomTempStrata2020_AvgCondStrata.jpg"))
# #   filename <- here::here(out.dir,paste0(sp,"_TotalCopepods2020_AvgCondStrata.jpg"))
#   #     filename <- here::here(out.dir,paste0(sp,"_CopepodSmallLarge2020_AvgCondStrata.jpg"))
# #     filename <- here::here(out.dir,paste0(sp,"_ZooplBiomassAnomaly_AvgCondStrata.jpg"))
#  #   filename <- here::here(out.dir,paste0(sp,"_AvgTempSpring2020_AvgCondStrata.jpg"))
#  #      filename <- here::here(out.dir,paste0(sp,"_AvgTempSummer2020_AvgCondStrata.jpg"))
#  #       filename <- here::here(out.dir,paste0(sp,"_AvgTempFall2020_AvgCondStrata.jpg"))
#  #       filename <- here::here(out.dir,paste0(sp,"_AvgTempWinter2020_AvgCondStrata.jpg"))
#  #  filename <- here::here(out.dir,paste0(sp,"_PropColumnColdPool2020_AvgCondStrata.jpg"))
#    
#   
#   #Full model output:
#   # filename <- here::here(out.dir,paste0(sp,"_HighesDevExplYr_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))
#   
#   #Mechanism model:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_SummerTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.jpg"))
#   #Not enough non-NAs:
#   # filename <- here::here(out.dir,paste0(sp,"_Mechanisms_StomFullStrata_ZooplBiomass_Biom_Fproxy_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_ZooplBiomass_Fproxy_AvgCondStrata.jpg"))
#   
#   #For Smooth dogfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.jpg"))
#   ##cod (have to remove AvgStomFullStratalag to run for cod):
# #  filename <- here::here(out.dir,paste0(sp,"_Mechanisms2020_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##summer flounder and red hake:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ## Spiny dogfish, silver hake:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringSummer_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##white hake:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##Winter skate:  
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   ##Witch flounder and bluefish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##Sea raven:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##Acadian redfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##fourspot, windowpane:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_ZooplBiomass_AvgCondStrata.jpg"))
#   ##goosefish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.jpg"))
#   ##Little skate:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.jpg"))
#   ##haddock:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##American plaice, black sea bass
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##winter flounder:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##ocean pout:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
#   ##Spotted hake and weakfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SummerTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##Thorny skate:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
#   ##Butterfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.jpg"))
#   ##Atlantic herring:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
#   ##pollock, yellowtail:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##mackerel:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
#   
#   #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
#   #(winter skate above)
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   
#   #(Witch flounder and bluefish above):
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   #(Sea raven above):
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   
#   ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_ZooplBiomass_AvgCondStrata.jpg"))
#   
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##Acadian redfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   ##summer flounder and red hake:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
#   
#   ##For Smooth dogfish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   ##goosefish:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   
#   #Remove stomach fullness for herring:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_TotalCopepods_AvgCondStrata.jpg"))
#   #Remove stomach fullness for cod:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
#   #Remove stomach fullness for yellowtail:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
#   #Remove stomach fullness for mackerel:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.jpg"))
#   
#   #With Fproxy and Total Biomass:      
#  # filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_TotalBiomass_Fproxy_CopepodSmLrg_AvgTempFall_AvgCondStrata.jpg"))
#  # filename <- here::here(out.dir,paste0(sp,"_Mechanisms_TotalBiomass_Fproxy_CopepodSmLrg_AvgTempSpring_AvgCondStrata.jpg"))
#   
#   #Fproxy and Total Biomass removing variables correlated >0.3:
#   # filename <- here::here(out.dir,paste0(sp,"_Mechanisms_NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBiomass_LocalAbundance_TotalBiomass_Fproxy_CopepodSmLrg_AvgCondStrata.jpg"))
#   
#   #-----------------------------
#   ####For weight at age coefficients instead of condition GAMs:
#   #Remove stomach fullness for mackerel:
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.jpg"))
#   #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
#   
#   
#    jpeg(filename)
#   # #Getting error that margins too large when running gam.check single species with WAA, set mar =c(1,1,,):
#    par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
#   # #   par(mfrow=c(2,2), mar=c(1,1,1,1), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
#    plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
#    dev.off()
#   
#   
#   #  plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
#   
#   
#   #gam.check (run model checks including checking smoothing basis dimensions)
#   #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.txt")))
#   #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalAbundance_FallTemp_ZooplanktonBiom_AvgCondStrata.txt")))
#   #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalAbund_SummerTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.txt")))
#   #        sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.txt")))
#   #sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.txt")))
#   #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_TotalCopepods_AvgCondStrata.txt")))
#   #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_LocalBiomass_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_2021LocalAbundance_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_TotalBiomass2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_Fproxy2020_AvgCondStrata.txt")))
#   # sink(here::here(out.dir,paste0(sp,"_GAMcheck_LocalBiomass2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_BottomTempStrata2020_AvgCondStrata.txt")))
#   #sink(here::here(out.dir,paste0(sp,"_GAMcheck_TotalCopepods2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_CopepodSmallLarge2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_ZooplanktonAnomaly2020_AvgCondStrata.txt")))
#   # sink(here::here(out.dir,paste0(sp,"_GAMcheck_SpringTempAnom2020_AvgCondStrata.txt")))
#  #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_SummerTempAnom2020_AvgCondStrata.txt")))
#   # sink(here::here(out.dir,paste0(sp,"_GAMcheck_FallTempAnom2020_AvgCondStrata.txt")))
#   # sink(here::here(out.dir,paste0(sp,"_GAMcheck_WinterTempAnom2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_AvgStomFullStaraLag2020_AvgCondStrata.txt")))
# # sink(here::here(out.dir,paste0(sp,"_GAMcheck_AvgStomFullStaraSpring2020_AvgCondStrata.txt")))
# #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_PropColumnColdPool2021_AvgCondStrata.txt")))
#    
#         
#    sink(here::here(out.dir,paste0(sp,"_GAMcheck_2021YEAR_AvgCondStrata.txt")))
#  #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_2021LatLonStrata_AvgCondStrata.txt")))
#  #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_BottomTempStrata2021_AvgCondStrata.txt")))
#    
#   #With Fproxy and Total Biomass:
# #  sink(here::here(out.dir,paste0(sp, "_GAMcheck_LocalBiomass_TotalBiomass_Fproxy_CopepodSmallLarge_AvgTempFall2020_AvgCondStrata.txt")))
# #  sink(here::here(out.dir,paste0(sp, "_GAMcheck_TotalBiomass_Fproxy_CopepodSmallLarge_SpringTemp2020_AvgCondStrata.txt")))
# 
# #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBio_LocalAbund_TotalBiomass_Fproxy_CopepodSmallLarge_AvgCondStrata.txt")))
# #   
# #   
#      mgcv::gam.check(condGAM)
# #   
#    sink()
#  }
# # 
#  AllSPP = do.call(rbind, datalist)
# 
# #Full model output:
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_HighestDevExplYr_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
# 
# #Mechanisms model:
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_SummerTemp_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
# #Not enough non-NAs:
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_AvgRelCondStrata_Biom_Fproxy_ZooplBiomassAnomaly.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_NoStom_AvgRelCondStrata_Fproxy_ZooplBiomassAnomaly.csv"))   
# 
# #For Smooth dogfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.csv"))
# ##cod (have to remove AvgStomFullStratalag to run for cod):
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms2020_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
# ##summer flounder and red hake:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
# ## Spiny dogfish, silver hake:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringSummer_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##white hake:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##Winter skate:  
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# ##Witch flounder and bluefish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
# ##Sea raven:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
# ##Acadian redfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
# ##fourspot, windowpane:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_ZooplBiomass_AvgCondStrata.csv"))
# ##goosefish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.csv"))
# ##Little skate:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.csv"))
# ##haddock:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##American plaice, black sea bass
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##winter flounder:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##ocean pout:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
# ##Spotted hake and weakfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SummerTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##Thorny skate:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
# ##Butterfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.csv"))
# ##Atlantic herring:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
# ##pollock, yellowtail:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
# ##mackerel:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
# 
# #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
# #(winter skate above)
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# 
# #(Witch flounder and bluefish above):
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# #(Sea raven above):
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# 
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# 
# ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_ZooplBiomass_AvgCondStrata.csv"))
# 
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# ##Acadian redfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# ##summer flounder and red hake:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
# 
# ##For Smooth dogfish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# ##goosefish:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# 
# #Remove stomach fullness for herring:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_TotalCopepods_AvgCondStrata.csv"))
# #Remove stomach fullness for cod:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
# #Remove stomach fullness for yellowtail:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
# #Remove stomach fullness for mackerel:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.csv"))
# 
# #With Fproxy and Total Biomass:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_TotalBiomass_Fproxy_CopepodSmLrg_AvgTempFall_AvgCondStrata.csv"))  
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_TotalBiomass_Fproxy_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
# 
# #Fproxy and Total Biomass removing variables correlated >0.3:
# # readr::write_csv(AllSPP, here::here(out.dir,"Mechanisms_NoPenal_NAreplace_LocalTemp_WinterTemp_LocalBio_LocalAbund_TotalBiomass_Fproxy_CopepodSmLrg_AvgCondStrata.csv"))
# 
# 
# #-----------------------------
# ####For weight at age coefficients instead of condition GAMs:
# #Remove stomach fullness for mackerel:
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
# 
# 
# #Single variable output:
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_StomFullSpringStrata2020.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_StomFullStrataLag.csv"))   
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_2021Year_AvgCondStrata.csv"))  
#  
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_2021LatLonStrata_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSpring_AvgCondStrata.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSummer_AvgCondStrata.csv"))     
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempFall_AvgCondStrata.csv"))       
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempWinter_AvgCondStrata.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_ZooplBiomassAnomaly_AvgCondStrata.csv"))   
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_CopepodSmallLarge_AvgCondStrata.csv"))  
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalCopepods_AvgCondStrata.csv"))  
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_2021BottomTempStrata_AvgCondStrata.csv")) 
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchwtStrata_AvgCondStrata.csv")) 
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Fproxy_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalBiomass_AvgCondStrata.csv"))
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_PropColumnColdPool_AvgCondStrata.csv"))
#  
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalBiomass_AvgCondStrata_2020.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Fproxy_AvgCondStrata_2020.csv"))
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchwtStrata_AvgCondStrata.csv")) 
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchnumStrata_AvgCondStrata_2020.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_BottomTempStrata2020_AvgCondStrata.csv"))  
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalCopepods2020_AvgCondStrata.csv"))    
# # readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_CopepodSmallLarge_AvgCondStrata.csv"))
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_ZooplBiomassAnomaly2020_AvgCondStrata.csv")) 
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSpring2020_AvgCondStrata.csv"))   
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSummer2020_AvgCondStrata.csv"))     
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempFall2020_AvgCondStrata.csv"))       
# #readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempWinter2020_AvgCondStrata.csv"))   

