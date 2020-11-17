#' gam stuff
#'
#'@param annualcond tibble. Output from RelConditionEPU.R
#'@param out.dir character string. name of directory in which plots and data files will be saved
#'

library(mgcv)
library(gam)
library(dplyr)
library(readr)

#turn off function while changing code
#gam_calcs <- function(annualcond,out.dir="output") {

#turn on output dir when not using funtion
out.dir = "output"
data.dir <- "data"

gis.dir  <- "gis"

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

StockData <- StockStrata %>% tidyr::separate_rows(Strata) %>% dplyr::mutate(STRATUM = Strata) %>% 
  dplyr::mutate(SVSPP = if_else(SVSPP<100, as.character(paste0('0',SVSPP)),
                 if_else(SVSPP>=100, as.character(SVSPP), 'NA'))) %>%
  select(!Strata)

CondStockjoin <- dplyr::left_join(cond.epu, StockData, by = c("SVSPP", "STRATUM"))

CondStock <- CondStockjoin %>% mutate(YEAR = as.numeric(as.character(YEAR)))

#Samples without stock area designations (strata where species were sampled but aren't included in stock area definition):
CondStockMissing <- CondStock %>% filter(is.na(Stock))

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
#Creating Average Relative Condition by strata, species, sex
AvgStrataCond <- CondStock %>% group_by(CRUISE6, STRATUM, Species, sex) %>% 
  mutate(AvgRelCondStrata=(mean(RelCond)), AvgRelCondStrataSD = (sd(RelCond)), AvgExpcatchwtStrata = (mean(BIOMASS)),
         AvgExpcatchnumStrata= (mean(ABUNDANCE)), AvgLatStrata = (mean(LAT)), 
         AvgLonStrata = (mean(LON)), AvgBottomTempStrata = (mean(BOTTEMP))) %>%
  distinct(AvgRelCondStrata, .keep_all = T)



#Creating Average Relative Condition and Average Stomach Fullness by EPU, species, sex
#Couldn't run mechanisms model because data too sparse:
#AvgEPUCond <- stom.epu %>% group_by(CRUISE6, EPU, Species, sex) %>% 
#  mutate(AvgRelCondEPU=(mean(RelCond)), AvgStomFullEPU=(mean(stom_full))) %>%
#  distinct(AvgRelCondEPU, .keep_all = T)
#---------------------------------------------------------------

#Bringing in average temperature data
AvgTempSpringData <- readr::read_csv(here::here(data.dir, "AverageTempSpring2020.csv"))
AvgTempSummerData <- readr::read_csv(here::here(data.dir, "AverageTempSummer2020.csv"))
AvgTempFallData <- readr::read_csv(here::here(data.dir, "AverageTempFall2020.csv"))
AvgTempWinterData <- readr::read_csv(here::here(data.dir, "AverageTempWinter2020.csv"))

AvgTempSpringFormat <- AvgTempSpringData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempSpring, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempSummerFormat <- AvgTempSummerData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempSummer, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempFallFormat <- AvgTempFallData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempFall, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempWinterFormat <- AvgTempWinterData %>% dplyr::mutate(YEAR=Year) %>%
  gather(EPU, AvgTempWinter, c(GB, GOM,SS, MAB), na.rm=F)

AvgTemp <- Reduce(dplyr::full_join, list(AvgTempWinterFormat, AvgTempSpringFormat, AvgTempSummerFormat, AvgTempFallFormat))

CondAvgTemp <- dplyr::left_join(AvgStrataCond, AvgTemp, by=c("YEAR", "EPU"))

#----------------------------------------------------------------------------------
#Bring in GLORYS bottom temperature data by NEFSC survey strata (mismatch of some strata currently)
GLORYSdata <- readr::read_csv(here::here(data.dir, "GLORYS_bottom_temp_STRATA_1993_2018.csv"))

GLORYSformat <- GLORYSdata %>% 
  separate(date, c('Year2', 'MONTH', 'DAY'), sep='-') 

GLORYSseason <- GLORYSformat %>% group_by(Year2, STRATA) %>% 
  dplyr::mutate(YEAR=as.character(Year2), STRATUM = as.character(paste0('0',STRATA)), 
                GLORYSwinter=ifelse(season==1,(weighted.mean), NA), GLORYSspring=ifelse(season==2,(weighted.mean), NA),
                GLORYSsummer=ifelse(season==3,(weighted.mean), NA), GLORYSfall=ifelse(season==4,(weighted.mean), NA))

GLORYS2 <- GLORYSseason %>% group_by(YEAR, STRATUM) %>% 
  summarize(GLORYSwinter=mean(GLORYSwinter, na.rm=T), GLORYSspring=mean(GLORYSspring, na.rm=T),
            GLORYSsummer=mean(GLORYSsummer, na.rm=T),GLORYSfall=mean(GLORYSfall, na.rm=T))

CondGLORYS <- dplyr::left_join(CondAvgTemp, GLORYS2, by=c('YEAR', 'STRATUM'))

#See NAs for GLORYS merge:
#NAglorys <- CondGLORYS %>% filter(is.na(GLORYSwinter))

#Trying to determine why 27% of Condition data doesn't have corresponding GLORYS data:
#Occurs across all years and strata
GLORYSna <- CondGLORYS %>% filter(is.na(GLORYSwinter)) 
GLORYSna_not <- CondGLORYS %>% filter(!is.na(GLORYSwinter)) 
GLORYSnaStratum <- GLORYSna_not[!duplicated(GLORYSna_not[,'STRATUM']),]
GLORYSnaStratOrder <- GLORYSnaStratum %>% arrange(STRATUM)

#--------------------------------------------------------------------------------
#Bringing in ratio of small to large copepods
load(here::here("data","1977_2017_SLI_Calfin_Pseudo_Ctyp.rdata"))
#View(Zooplankton_Primary_Prod)
Calfin <- Zooplankton_Primary_Prod
#head(Calfin)
CalfinFormat <- Calfin %>% dplyr::rename(YEAR = year) %>% 
  select(YEAR, SLI.gbk, SLI.gom, SLI.mab, SLI.scs) %>% 
  gather(CalEPU, CopepodSmallLarge, c(SLI.gbk, SLI.gom, SLI.mab, SLI.scs)) %>%
  mutate(EPU = if_else(CalEPU=='SLI.gbk', 'GB',
                       if_else(CalEPU=='SLI.gom', 'GOM',
                               if_else(CalEPU=='SLI.mab', 'MAB',
                                       if_else(CalEPU=='SLI.scs', 'SS', 'NA')))))

CondCal <- dplyr::left_join(CondAvgTemp, CalfinFormat, by=c("YEAR", "EPU"))

#Bring in total zooplankton biomass 
ZoopBio <- readr::read_csv(here::here("data","EPUCopepodBiomassAnomalies.csv"))

Zoop <- ZoopBio %>% dplyr::rename(YEAR=Year)

CondZoo <- dplyr::left_join(CondCal, Zoop, by = c("YEAR", "EPU"))

#Bring in total copepods (as millions of individuals) from NEFSCZooplankton_v3_6b_v2018.xls:
TotalCopepods <- readr::read_csv(here::here("data","TotalCopepods2020.csv"))

TotCop <- dplyr::left_join(CondZoo, TotalCopepods, by = c("YEAR", "EPU"))

#--------------------------------------------------------------------------------
#Bloom time and magnitude data
Bloom <- readr::read_csv(here::here("data","Bloom_out_all2020.csv"))


#-------------------------------------------------------------------------------- 
#Average stomach fullness by Species, YEAR, EPU and sex for the year before
#This brings in stomach fullness data from allfh database (from StomFullnessData_allfh.R):
#average stomach fullness by EPU:
stomdata <- stom

#Creating Average Fall Stomach Fullness by year, STRATUM, species, sex
#Currently used for Condition GAM analyses and multi-model dataset:
AvgStomFullStrata <- stomdata %>% dplyr::filter(season == "FALL") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullStrata=(mean(stom_full)))

#Creating Average Fall Stomach Fullness by year, EPU, species, sex
AvgStomFullEPU <- stomdata %>% dplyr::filter(season == "FALL") %>%
  group_by(year, EPU, Species, pdsex) %>% 
  mutate(AvgStomFullEPU=(mean(stom_full)))

#Test for GAMs analysis to see if spring stomach fullness predicts fall condition:
#Creating Average Spring Stomach Fullness by year, STRATUM, species, sex
AvgStomFullSpringStrata <- stom %>% dplyr::filter(season == "SPRING") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullSpringStrata=(mean(stom_full)))

# change stomach data variables to merge with condition data: 
stom.data.EPU <- AvgStomFullEPU %>% dplyr::mutate(YEAR = year, SEASON = season, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  distinct(YEAR, EPU, Species, SEX, .keep_all = TRUE) %>%   select(YEAR, EPU, Species, SEASON, SEX, AvgStomFullEPU)

#Fall stomach data by strata:
stom.data.strata <- AvgStomFullStrata %>% dplyr::mutate(YEAR = year, SEASON = season, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  distinct(YEAR, STRATUM, Species, SEX, .keep_all = TRUE) %>%   select(YEAR, STRATUM, EPU, Species, SEASON, SEX, AvgStomFullStrata)

#Spring stomach data by strata as test for lagging in GAM:
stom.spring.strata <- AvgStomFullSpringStrata %>% dplyr::mutate(YEAR = year, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  distinct(YEAR, STRATUM, Species, SEX, .keep_all = TRUE) %>%   select(YEAR, STRATUM, EPU, Species, SEX, AvgStomFullSpringStrata)


stom.data.strata$SEX <- as.factor(stom.data.strata$SEX) 
stom.spring.strata$SEX <- as.factor(stom.spring.strata$SEX) 

#merge stomach fullness into condition data:
#make sure allfh data includes STRATUM as factor with leading zero for merge
#merge by strata for Condition GAM and multi-model dataset:
AvgStom <- dplyr::left_join(TotCop, stom.data.strata, by = c('YEAR', 'SEASON', 'STRATUM', 'EPU', 'Species', 'SEX'))

#spring stomach fullness merge by strata for Condition GAM lag:
AvgStomSpr <- dplyr::left_join(TotCop, stom.spring.strata, by = c('YEAR', 'STRATUM', 'EPU', 'Species', 'SEX')) %>%
  select('YEAR', 'STRATUM', 'EPU', 'Species', 'sex',
         'AvgRelCondStrata', 'AvgRelCondStrataSD', 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
         'AvgLatStrata', 'AvgLonStrata', 'AvgBottomTempStrata',
         'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall','CopepodSmallLarge',
         'ZooplBiomassAnomaly', 'TotalCopepodsMillions', 'AvgStomFullSpringStrata') %>%
  distinct()

#Old code for stomach data not from allfh:
#AvgStom <- CondCal %>% dplyr::group_by(Species, YEAR, EPU, sex) %>% dplyr::mutate(AvgStomFull=mean(AvgStomFullStrata, na.rm=TRUE))
##Can't get lag and mutate to work
#AvgStomLag <- AvgStom %>% dplyr::mutate(AvgStomLag1=(AvgStomFull %in% YEAR-1))
#AvgStomLag <- AvgStom %>% dplyr::lag(AvgStomLag1=(AvgStomFull, n=1)
#Clunky way of lagging but it works:
#Lagged stomach index by strata for Condition GAM:
A <- AvgStom %>% select(YEAR, Species, STRATUM, EPU, sex, AvgStomFullStrata)
B <- unique(A)
C <- B %>% dplyr::mutate(YEARstom= YEAR)
D <- C %>% dplyr::ungroup()
E <- D %>% dplyr::select(Species, YEARstom, STRATUM, EPU, sex, AvgStomFullStratalag=AvgStomFullStrata)
Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
AvgStom2 <- AvgStom %>% dplyr::select(-c(AvgStomFullStrata))
AvgStomStrataLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("Species", "YEAR","STRATUM", "EPU", "sex")) %>%
  select('YEAR', 'CRUISE6', 'STRATUM', 'EPU', 'SEASON','Species', 'sex',
         'AvgRelCondStrata', 'AvgRelCondStrataSD', 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
         'AvgLatStrata', 'AvgLonStrata', 'AvgBottomTempStrata',
         'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall','CalEPU', 'CopepodSmallLarge',
         'ZooplBiomassAnomaly', 'TotalCopepodsMillions', 'AvgStomFullStratalag') %>%
  distinct()


#Lagged stomach index by tow for multi-model dataset:
# A <- AvgStom %>% select(YEAR, Species, STRATUM, EPU, SEASON, sex, AvgStomFullStrata)
# B <- unique(A)
# C <- B %>% dplyr::mutate(YEARstom= YEAR)
# D <- C %>% dplyr::ungroup()
# E <- D %>% dplyr::select(Species, YEARstom, STRATUM, EPU, SEASON, sex, AvgStomFullStratalag=AvgStomFullStrata)
# Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
# AvgStom2 <- AvgStom %>% dplyr::select(-c(AvgStomFullStrata))
# 
# AvgStomTowLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("YEAR", "SEASON", "Species", "STRATUM", "EPU", "sex")) %>%
#   select('YEAR', 'SEASON','CRUISE6', 'STRATUM', 'STATION', 'TOW', 'BOTTEMP', 'LAT', 'LON', 'EPU', 'Species', 'sex', 
#          'EXPCATCHWT', 'EXPCATCHNUM', 
#          'AvgTowRelCond', 'AvgTowRelCondSD', 'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall',
#         'CalEPU', 'CopepodSmallLarge', 'ZooplBiomassAnomaly', 'AvgStomFullStratalag')
# AvgStomTowLag <- dplyr::distinct(AvgStomTowLag)

#check merge rows by counting distinct rows:
#DistRowsAvgStom2 <- nrow(dplyr::distinct(AvgStom2, YEAR, Species, STRATUM, STATION, TOW, EPU, SEASON, sex))
#DistRowsStomlag <- nrow(dplyr::distinct(Stomlag, YEAR, Species, STRATUM, EPU, SEASON, sex))

#Multi-model dataset:
#readr::write_csv(AvgStomTowLag, here::here(out.dir,"RelCondition_tow_EnvirIndices.csv"))   

#------------------------------------------------------------------------------------ 
#Add stock assessment data from Stock SMART: https://www.fisheries.noaa.gov/resource/tool-app/stock-smart
load(here::here("data","stockAssessmentData.Rdata"))
#View(stockAssessmentData)

StockAssDat <- stockAssessmentData %>%
  filter(Species %in% c('Smooth dogfish', 'Spiny dogfish', 'Winter skate', 'Little skate',
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
                        'Goosefish')) %>%
  filter(Region %in% c('Atlantic',
                       'Gulf of Maine / Georges Bank',
                       'Northwestern Atlantic Coast',
                       'Gulf of Maine',
                       'Gulf of Maine / Cape Hatteras',
                       'Mid',
                       'Atlantic Coast',
                       'Gulf of Maine / Northern Georges Bank',
                       'Southern Georges Bank / Mid',
                       'Georges Bank',
                       'Georges Bank / Southern New England',
                       'Southern New England / Mid',
                       'Cape Cod / Gulf of Maine'))

AssDat <- StockAssDat %>%
  select(Species, Region, Year, Value, Metric) %>%
  spread(Metric, Value) %>%
  dplyr::mutate(YEAR = Year) %>%
  #Sum total biomass (Abundance) across stocks for GAMs:
  group_by(Species, YEAR) %>%
  dplyr::mutate(TotalBiomass = sum(Abundance, na.rm=TRUE))
#Catch/biomass as index of Fmort for Goosefish for GAMs:
#Not working: FproxyDat <- AssDat %>%
#Not working:      dplyr::mutate(Fproxy = (Catch/Abundance)) %>%
#Not working:     replace_na(AssDat$Fmort = AssDat$Fproxy)  
AssDat$FproxyCatch <- (AssDat$Catch/AssDat$Abundance)
AssDat$Fproxy <- ifelse(is.na(AssDat$Fmort),AssDat$FproxyCatch,AssDat$Fmort) 

#Using Average stomach fullness lagged 1 year:
CondStockAss <- dplyr::left_join(AvgStomStrataLag, AssDat, by=c('Species', 'YEAR'))
#Using spring stomach fullness: 
#CondStockAss <- dplyr::left_join(AvgStomSpr, AssDat, by=c('Species', 'YEAR'))

#------------------------------------------------------------------------------------
#Weight at age coefficients from Kevin's GLM output: pt tab of glm_out_GrowthCovariates_species.xls
WAA <- readr::read_csv(here::here("data","GrowthCovariates_15species.csv"))
#Don't join on sex since F is changed to FALSE when importing csv into R:
CondWAAcoeff <- dplyr::left_join(CondStockAss, WAA, by=c('Species', 'YEAR' ,'SEASON'))

######End code before GAM analyses#####

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
CondClean <- CondWAAcoeff

####If using total biomass (Abundance) or Fmort from StockSMART in GAMs, remove species lacking data: 
# CondClean <- CondStockAss %>%
#   filter(Species %in% c('Smooth dogfish', 'Spiny dogfish', 'Winter skate', 'Little skate',
#                         'Thorny skate',
#                         'Silver hake',
#                         'Atlantic cod',
#                         'Haddock',
#                         'Pollock',
#                         'White hake',
#                         'Red hake',
#                         'American plaice',
#                         'Summer flounder',
#                         'Yellowtail flounder',
#                         'Winter flounder',
#                         'Windowpane',
#                         'Atlantic mackerel',
#                         'Butterfish',
#                         'Bluefish',
#                         'Black sea bass',
#                         'Acadian redfish',
#                         'Ocean pout',
#                         'Goosefish')) 

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
#' 'Bluefish',
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
CondCleanTotCop <- CondClean %>%
  dplyr::filter((is.na(TotalCopepodsMillions) | TotalCopepodsMillions < 10000))

CondCleanSpDogWt <- CondCleanTotCop %>%
  dplyr::filter(is.na(AvgExpcatchwtStrata) | (!(Species == "Spiny dogfish" & AvgExpcatchwtStrata >1500)))

CondClean <- CondCleanSpDogWt %>%
  dplyr::filter(is.na(AvgExpcatchnumStrata) | (!(Species == "Windowpane" & AvgExpcatchnumStrata >250)))

spp <- unique(CondClean$Species)
datalist = list()

for(sp in spp) {
  condSPP <- CondClean %>% dplyr::filter(Species==sp)
  
  #turn on for testing a single species outside of loop:
  #condSPP <- CondClean %>% dplyr::filter(Species=='Atlantic cod') %>% mutate(sp='Atlantic cod')
  
  #Full model
  #   form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring) +s(YEAR), data=condSPP)
  #Single index
  #form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHWT, k=10), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchwtStrata, k=10), data=condSPP)
  form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchnumStrata, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHNUM, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(LON, LAT, k=25), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStrata, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStratalag, k=10), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullSpringStrata, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(CopepodSmallLarge, k=10), data=condSPP)
  # form.cond <- formula(AvgRelCondStrata ~ s(ZooplBiomassAnomaly, k=10), data=condSPP)
  #form.cond <- formula(AvgRelCondStrata ~ s(TotalCopepodsMillions, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSummer, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempFall, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempWinter, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
  #form.cond <- formula(AvgRelCondStrata ~ s(Fproxy, k=10), data=condSPP)
  #form.cond <- formula(AvgRelCondStrata ~ s(Abundance, k=10), data=condSPP)
  
  
  #Eplains highest deviance:
  #  form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10) +s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgLonStrata, AvgLatStrata, k=25) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  
  #Mechanisms model:
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #Not enough non-NAs:
  # form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10) +s(TotalBiomass, k=10) +s(Fproxy, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10) +s(Fproxy, k=10), data=condSPP)
  
  #Mechanism models selecting Expcatwt/Expcatnum, AvgStomFullStratalag/AvgStomFullSpringStrata, AvgTempSpring/AvgTempSummer/AvgTempFall, CopepodSmallLarge/ZooplBiomassAnomaly based on lowest p-values and if both zero, highest deviance explained:
  ##For Smooth dogfish:
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##cod full mechanisms (would have to remove AvgStomFullStratalag to run for cod Deviance Explained = 0.91):
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##cod (previous run Deviance Explained = 0.125, can't get to run for cod now!):
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##summer flounder and red hake:
  #   form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ## Spiny dogfish, silver hake (had to reduce K to 3 since got Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
  #A term has fewer unique covariate combinations than specified maximum degrees of freedom):
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchwtStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(CopepodSmallLarge, k=3) +s(AvgTempSummer, k=3), data=condSPP)
  ##white hake:
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchwtStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(CopepodSmallLarge, k=3) +s(AvgTempFall, k=3), data=condSPP)
  ##Winter skate:  
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##Witch flounder and bluefish:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##Sea raven:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##Acadian redfish:
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##goosefish:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##Little skate:
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=3) +s(AvgExpcatchnumStrata, k=3) +s(AvgStomFullSpringStrata, k=3) +s(ZooplBiomassAnomaly, k=3) +s(AvgTempFall, k=3), data=condSPP)
  ##haddock:
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##American plaice, black sea bass
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##winter flounder:
  #     form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##ocean pout:
  ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##Spotted hake and weakfish:
  ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##Thorny skate:
  ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatcwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##Butterfish:
  ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##Atlantic herring:
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  ##pollock, yellowtail:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##mackerel:
  ##    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullSpringStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  
  
  #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
  #(winter skate above)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #(Witch flounder and bluefish above):
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #(Sea raven above):
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##Acadian redfish:
  #      form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##summer flounder and red hake:
  #   form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  ##For Smooth dogfish:
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##goosefish:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #Remove stomach fullness for herring:
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  #Remove stomach fullness for cod:
  #  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #Remove stomach fullness for yellowtail:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #Remove stomach fullness for mackerel:
  #    form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  
  #-----------------------------
  ####For weight at age coefficients instead of condition GAMs:
  #(winter skate above)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #(Witch flounder and bluefish above):
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #(Sea raven above):
  #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
  #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  ##Acadian redfish:
  #      form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##summer flounder and red hake:
  #   form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  ##For Smooth dogfish:
  #form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  ##goosefish:
  #    form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  
  #Remove stomach fullness for herring:
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=10) +s(AvgTempFall, k=10), data=condSPP)
  #Remove stomach fullness for cod:
  #  form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchnumStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #Remove stomach fullness for yellowtail:
  #    form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(CopepodSmallLarge, k=10) +s(AvgTempSpring, k=10), data=condSPP)
  #Remove stomach fullness for mackerel:
  #   form.cond <- formula(WeightAAcoefficient ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(TotalCopepodsMillions, k=15) +s(AvgTempSpring, k=15), data=condSPP)
  
  
  
  #Can add factor variable as a by variable: e.g. +s(LON, LAT, k=25, by = EPU)
  #EXPCATCHWT had slightly more significance than EXPCATCHNUM for more species
  
  #na.gam.replace works if not including stomach data:  
  condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)
  #, na.action = na.gam.replace)
  
  #    step.cond <- step.Gam(condGAM, scope= list("BOTTEMP" =~1+BOTTEMP+s(BOTTEMP),
  #                                              "EXPCATCHNUM" =~1+EXPCATCHNUM+s(EXPCATCHNUM),
  #                                              "LON, LAT" =~1+LON,LAT +s(LON,LAT),
  #                                              "YEAR" =~1+YEAR))
  
  GAMstats <- summary(condGAM)
  
  SumCondGAM <- t(c(sp, round(GAMstats$s.pv,3),  round(GAMstats$r.sq,3), round(GAMstats$dev.expl,3),  round(GAMstats$sp.criterion,3), GAMstats$n))
  
  dl=data.frame(SumCondGAM)
  #Full model output:
  #GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'LON LAT', 'AvgStomFullLag', 'CopepodSL', 'AvgTempSpring', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #Full Model with reasonable mechanisms relating to condition changes:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'AvgStomFullLag', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #Not enough non-NAs:
  # GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'AvgStomFullLag', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'Total Biomass', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass strata', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #For Smooth dogfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##cod (have to remove AvgStomFullStratalag to run for cod):
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##summer flounder and red hake:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ## Spiny dogfish, silver hake:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##white hake:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Winter skate:  
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Witch flounder and bluefish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Sea raven:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Acadian redfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##fourspot, windowpane:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##goosefish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Little skate:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##haddock:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##American plaice, black sea bass
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##winter flounder:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##ocean pout:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Spotted hake and weakfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Thorny skate:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Butterfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Atlantic herring:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##pollock, yellowtail:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##mackerel:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullSpring', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
  #(winter skate above)
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #(Witch flounder and bluefish above):
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #(Sea raven above):
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Zooplankton Biomass Anomaly', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##Acadian redfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##summer flounder and red hake:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Copepod Small/Large Ratio', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  ##For Smooth dogfish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  ##goosefish:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'AvgStomFullStrataLag', 'Total Copepods Millions', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #Removed stomach fullness for herring:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Total Copepods Millions', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #Remove stomach fullness for cod:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Abundance Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #Remove stomach fullness for yellowtail:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Copepod Small/Large Ratio', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #Remove stomach fullness for mackerel:
  #GAMnames=c('Species', 'Bottom Temp Strata', 'Local Biomass Strata', 'Total Copepods Millions', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  
  #Model with highest deviance explained:
  #GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  #single variable runs
  #GAMnames=c('Species', 'AvgStomFullSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'AvgTempWinter', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'ZooplBiomassAnomaly', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'CopepodSmallLarge', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #GAMnames=c('Species', 'TotalCopepodsMillions', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  # GAMnames=c('Species', 'Bottom Temp Strata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  # GAMnames=c('Species', 'AvgExpcatchwtStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  GAMnames=c('Species', 'AvgExpcatchnumStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #  GAMnames=c('Species', 'Fproxy', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  #  GAMnames=c('Species', 'Total Biomass', 'R sq.', 'Deviance Explained', 'GCV', 'n')
  
  
  #error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
  names(dl)=GAMnames
  datalist[[sp]] <- dl
  
  #Use for testing plot with single species
  #filename <-here::here(out.dir, paste0('GoosefishYEAR_condition.jpg'))
  
  #Single variable output:
  #   filename <- here::here(out.dir,paste0(sp,"_StomFullSpringStrata_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_YEAR_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_AvgTempSpring_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_AvgTempSummer_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_AvgTempFall_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_AvgTempWinter_AvgCondStrata.jpg"))
  #    filename <- here::here(out.dir,paste0(sp,"_ZooplBiomassAnomaly_AvgCondStrata.jpg"))
  #    filename <- here::here(out.dir,paste0(sp,"_CopepodSmallLarge_AvgCondStrata.jpg"))
  #   filename <- here::here(out.dir,paste0(sp,"_TotalCopepods_AvgCondStrata.jpg"))
  # filename <- here::here(out.dir,paste0(sp,"_BottomTempStrata_AvgCondStrata.jpg"))
  #     filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchwtStrata_AvgCondStrata.jpg"))
  filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchnumStrata_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Fproxy_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_TotalBiomass_AvgCondStrata.jpg"))
  
  #Full model output:
  # filename <- here::here(out.dir,paste0(sp,"_HighesDevExplYr_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))
  
  #Mechanism model:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_SummerTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.jpg"))
  #Not enough non-NAs:
  # filename <- here::here(out.dir,paste0(sp,"_Mechanisms_StomFullStrata_ZooplBiomass_Biom_Fproxy_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_ZooplBiomass_Fproxy_AvgCondStrata.jpg"))
  
  #For Smooth dogfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.jpg"))
  ##cod (have to remove AvgStomFullStratalag to run for cod):
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
  ##summer flounder and red hake:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
  ## Spiny dogfish, silver hake:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringSummer_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##white hake:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##Winter skate:  
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  ##Witch flounder and bluefish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
  ##Sea raven:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
  ##Acadian redfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.jpg"))
  ##fourspot, windowpane:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_ZooplBiomass_AvgCondStrata.jpg"))
  ##goosefish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.jpg"))
  ##Little skate:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.jpg"))
  ##haddock:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##American plaice, black sea bass
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##winter flounder:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##ocean pout:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
  ##Spotted hake and weakfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SummerTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##Thorny skate:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
  ##Butterfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.jpg"))
  ##Atlantic herring:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
  ##pollock, yellowtail:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.jpg"))
  ##mackerel:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_TotalCopepods_AvgCondStrata.jpg"))
  
  #####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
  #(winter skate above)
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  
  #(Witch flounder and bluefish above):
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  #(Sea raven above):
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  
  ##fourspot, windowpane (removed stomach fullness lagged for windowpane):
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_ZooplBiomass_AvgCondStrata.jpg"))
  
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  ##Acadian redfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  ##summer flounder and red hake:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.jpg"))
  
  ##For Smooth dogfish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  ##goosefish:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  
  #Remove stomach fullness for herring:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_FallTemp_TotalCopepods_AvgCondStrata.jpg"))
  #Remove stomach fullness for cod:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
  #Remove stomach fullness for yellowtail:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_CopepodSmLrg_AvgCondStrata.jpg"))
  #Remove stomach fullness for mackerel:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.jpg"))
  
  #-----------------------------
  ####For weight at age coefficients instead of condition GAMs:
  #Remove stomach fullness for mackerel:
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.jpg"))
  #filename <- here::here(out.dir,paste0(sp,"_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.jpg"))
  
  
  jpeg(filename)
  #Getting error that margins too large when running gam.check single species with WAA, set mar =c(1,1,,):
  par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
  #   par(mfrow=c(2,2), mar=c(1,1,1,1), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
  plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
  dev.off()
  
  
  #  plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
  
  
  #gam.check (run model checks including checking smoothing basis dimensions)
  #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.txt")))
  #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalAbundance_FallTemp_ZooplanktonBiom_AvgCondStrata.txt")))
  #  sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_LocalAbund_SummerTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.txt")))
  #        sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.txt")))
  #sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.txt")))
  #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_TotalCopepods_AvgCondStrata.txt")))
  #   sink(here::here(out.dir,paste0(sp,"_GAMcheck_LocalBiomass_AvgCondStrata.txt")))
  sink(here::here(out.dir,paste0(sp,"_GAMcheck_LocalAbundance_AvgCondStrata.txt")))
  
  mgcv::gam.check(condGAM)
  
  sink()
}

AllSPP = do.call(rbind, datalist)

#Full model output:
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_HighestDevExplYr_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   

#Mechanisms model:
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_SummerTemp_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
#Not enough non-NAs:
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_AvgRelCondStrata_Biom_Fproxy_ZooplBiomassAnomaly.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_NoStom_AvgRelCondStrata_Fproxy_ZooplBiomassAnomaly.csv"))   

#For Smooth dogfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.csv"))
##cod (have to remove AvgStomFullStratalag to run for cod):
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
##summer flounder and red hake:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
## Spiny dogfish, silver hake:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringSummer_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##white hake:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##Winter skate:  
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
##Witch flounder and bluefish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
##Sea raven:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
##Acadian redfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SummerTemp_StomFullStratalag_CopepodSmLrg_AvgCondStrata.csv"))
##fourspot, windowpane:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_ZooplBiomass_AvgCondStrata.csv"))
##goosefish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullStratalag_TotalCopepods_AvgCondStrata.csv"))
##Little skate:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.csv"))
##haddock:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##American plaice, black sea bass
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##winter flounder:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##ocean pout:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
##Spotted hake and weakfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SummerTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##Thorny skate:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
##Butterfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_StomFullSpring_ZooplBiomass_AvgCondStrata.csv"))
##Atlantic herring:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))
##pollock, yellowtail:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_CopepodSmLrg_AvgCondStrata.csv"))
##mackerel:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullSpring_TotalCopepods_AvgCondStrata.csv"))

#####Ran into sample size issues with stomach fullness from spring. Instead running iterations of other combinations:
#(winter skate above)
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))

#(Witch flounder and bluefish above):
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
#(Sea raven above):
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))

#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))

##fourspot, windowpane (removed stomach fullness lagged for windowpane):
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_ZooplBiomass_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_ZooplBiomass_AvgCondStrata.csv"))

#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
##Acadian redfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))
##summer flounder and red hake:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_CopepodSmLrg_AvgCondStrata.csv"))

##For Smooth dogfish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SpringTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))
##goosefish:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbund_FallTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))

#Remove stomach fullness for herring:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_FallTemp_TotalCopepods_AvgCondStrata.csv"))
#Remove stomach fullness for cod:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalAbundance_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
#Remove stomach fullness for yellowtail:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_CopepodSmLrg_AvgCondStrata.csv"))
#Remove stomach fullness for mackerel:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.csv"))

#-----------------------------
####For weight at age coefficients instead of condition GAMs:
#Remove stomach fullness for mackerel:
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_WAAcoeff_LocalBiomass_SpringTemp_TotalCopepods_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"_Mechanisms_WAAcoeff_LocalAbund_SummerTemp_StomFullStrataLag_TotalCopepods_AvgCondStrata.csv"))


#Single variable output:
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_StomFullSpringStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Year_AvgCondStrata.csv"))     
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSpring_AvgCondStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSummer_AvgCondStrata.csv"))     
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempFall_AvgCondStrata.csv"))       
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempWinter_AvgCondStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_ZooplBiomassAnomaly_AvgCondStrata.csv"))   
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_CopepodSmallLarge_AvgCondStrata.csv"))  
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalCopepods_AvgCondStrata.csv"))  
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_BottomTempStrata_AvgCondStrata.csv")) 
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchwtStrata_AvgCondStrata.csv")) 
readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchnumStrata_AvgCondStrata_2020.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Fproxy_AvgCondStrata.csv"))
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_TotalBiomass_AvgCondStrata.csv"))

