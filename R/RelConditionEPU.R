#' Calculated condition factor
#'
#' describe further. We will clean this up further if/when we make this into a package
#'
#' @param pullNewData boolean. Should a connection to oracle be made to pull data or use existing data
#' @param out.dir character string. name of directory in which plots and data files will be saved
#'
#' @return list of 2 items
#' \item{annualcond}{tibble (n x 6) Condition factor for each species, sex over time}
#' \item{stom}{data.frame (m x 52). stomach contents by year, cruise, sex, species etc.}
#'
#' @export


source("R/install_cond_packages.R")
source("R/connect_to_database.R")
source("R/pull_from_svdbs.R")
source("R/utilities.R")
source("R/plot_condition.R")
#source("R/StomFullnessData_allfh.R")
#Required packages
#library(devtools)
#devtools::install_github('slucey/RSurvey/Survdat', )
library(Survdat)
library(data.table)
# library(graphics)
# library(grDevices)
#library(TeachingDemos)
library(dplyr)
library(tidyr)
library(tidyverse)
#library(gapminder)
library(rgdal)
library(RODBC)
#library(gam)
library(magrittr)
#library(readr)

#Turn off the function when running outside of function to test new code
#RelConditionEPU <- function(pullNewData=F,out.dir="output") {
  # create output directory if it doesnt exist
#  if (!dir.exists(out.dir)) dir.create(out.dir)
  
#Turn this on when running outside of function
 out.dir="output"

  data.dir <- "data"
  gis.dir  <- "gis"


#-------------------------------------------------------------------------------
#This section is used to pull NEFSC survey bottom trawl data directly from SVDBS.
#It requires a current Oracle connection, permission to access the database and a user name/password. 


#laptop:
#source("C:\\Users\\laurel.smith\\Documents\\R\\Oracle_User_Data.R")
#sole <- odbcConnect("sole",uid=user.name,pwd=password,believeNRows=FALSE)

  #comment out if not running as function
if (pullNewData == T) {
  # grabs data from oracle
  uid <- readline("Please Enter your username for oracle: ")
  message(paste0("Pulling data from svdbs ... please wait ... data will be saved in directory: ",here::here(out.dir)))
  survey <- pull_from_svdbs(uid) 
  # save data pull
  dateOfPull <- format(Sys.time(), "%m-%d-%y")
  saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_",dateOfPull,".rds")))
#  saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_8-15-19.rds")))
} else {
  # Otherwise, load data below:
  #If not pulling from SVDBS, load NEFSC survey data:
  #load(file.path(data.dir, 'NEFSC_survey_data_8-15-19.RData', sep = ''))
  #or if that doesn't work:
  readRDS(file.path(data.dir, 'NEFSC_survey_data_02-13-20.rds', sep = ''))
  survey 
  #<- readRDS(here::here(out.dir, "NEFSC_survey_data_01-09-20.rds"))
}
  
 

fall <- survey[survey$SEASON == 'FALL',]
#------------------------------------------------------------------------------

#reading in condition lw paramteters for tidyverse:
LWparams <- readr::read_csv(here::here(data.dir, "lw_parameters_Condition.csv"))


#head(LWparams)
#View(LWparams)
#head(fall)

#Standardize syntax of Condition L-W data for merge with survey data:

#using tidyverse to recode sex:
LWpar <- dplyr::mutate(LWparams,
                SEX = SEXMF,
                SVSPP = LW_SVSPP)

LWpar$SEX[LWpar$SEXMF=='M'] <- 1
LWpar$SEX[LWpar$SEXMF=='F'] <- 2
LWpar$SEX[is.na(LWpar$SEXMF)] <- 0

#view(LWparams)
#Use seasonal L-W parameters when available

#this works to fill EXPONENT_FALL_COMP with SEASONLESS_EXPONENT if NA, otherwise use EXPONENT_FALL (but only if brought in as a data frame, not as a data table)
ind <- is.na(LWpar$EXPONENT_FALL)
LWpar$EXPONENT_FALL_COMPL <- LWpar$EXPONENT_FALL
LWpar$EXPONENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
#cbind(LWpar$EXPONENT_FALL,LWpar$EXPONENT_FALL_COMPL,LWpar$SEASONLESS_EXPONENT)

ind <- is.na(LWpar$EXPONENT_SPRING)
LWpar$EXPONENT_SPRING_COMPL <- LWpar$EXPONENT_SPRING
LWpar$EXPONENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
#cbind(LWpar$EXPONENT_SPRING,LWpar$EXPONENT_SPRING_COMPL,LWpar$SEASONLESS_EXPONENT)

ind <- is.na(LWpar$COEFFICIENT_FALL)
LWpar$COEFFICIENT_FALL_COMPL <- LWpar$COEFFICIENT_FALL
LWpar$COEFFICIENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
#cbind(LWpar$COEFFICIENT_FALL,LWpar$COEFFICIENT_FALL_COMPL,LWpar$SEASONLESS_COEFFICIENT)

ind <- is.na(LWpar$COEFFICIENT_SPRING)
LWpar$COEFFICIENT_SPRING_COMPL <- LWpar$COEFFICIENT_SPRING
LWpar$COEFFICIENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
#cbind(LWpar$COEFFICIENT_SPRING,LWpar$COEFFICIENT_SPRING_COMPL,LWpar$SEASONLESS_COEFFICIENT)

#merge with tidyr:
LWparInt <- transform(LWpar, SEX = as.integer(SEX))
summary(LWparInt)
summary(fall)
mergedata <- merge(fall, LWparInt, all.fall=T, all.LWparInt = F)
#left_join gave NAs for some scup and BSB L-W params
#mergedata <- left_join(fall, LWparInt, by= c('SVSPP', 'SEX'))

#checking for missing complete L-W params
nocompl <- dplyr::filter(mergedata, is.na(COEFFICIENT_FALL_COMPL))
unique(nocompl$SVSPP)
unique(nocompl$YEAR)

#filters out values without losing rows with NAs:
mergewt <- dplyr::filter(mergedata, is.na(INDWT) | INDWT<75)
mergewtno0 <- dplyr::filter(mergewt, is.na(INDWT) | INDWT>0)
mergelenno0 <- dplyr::filter(mergewtno0, is.na(LENGTH) | LENGTH>0)
mergelen <- dplyr::filter(mergelenno0, !is.na(LENGTH))
mergeindwt <- dplyr::filter(mergelen, !is.na(INDWT))

#Calculate relative condition:
#need to add in case when fall is missing, use seasonless
cond <- dplyr::mutate(mergeindwt, 
               predwt = (exp(COEFFICIENT_FALL_COMPL))*LENGTH**EXPONENT_FALL_COMPL,
               RelCond = INDWT/predwt*100)

#check where condition is missing
#nocond <- filter(cond, is.na(RelCond))
#unique(nocond$SVSPP)
#unique(nocond$YEAR)
#Scup (143) and black sea bass (141) have missing condition, but have seasonless coefficients and exponents.

#unique(cond$YEAR)

#Parse condition data by EPU:
#Grab strata
#load(file.path(data.dir, 'Survdat.RData'))

strata <- rgdal::readOGR(dsn=here::here(gis.dir),layer="EPU",verbose=F)

data.table::setnames(cond,"BEGLAT","LAT") # change name of column header
data.table::setnames(cond,"BEGLON","LON")
cond <- cond %>% dplyr::filter(!is.na(LAT)) # remove all NA's 

cond.epu <- Survdat::poststrat(as.data.table(cond), strata)
data.table::setnames(cond.epu, 'newstrata', 'EPU')
#check if scup exists, doesn't because LWparams only have unsexed 
#sort(unique(cond.epu$SEX[cond.epu$SVPP==143]))

#View(cond.epu)
#condno <- filter(cond.epu, is.na(SEX))

#summarize condition as annual average:

condstdev <- aggregate(cond.epu$RelCond, by = list('SVSPP'=cond.epu$SVSPP), sd)
names(condstdev)[ncol(condstdev)] = 'condSD'

cond.001wgt <- subset(cond.epu, cond.epu$INDWT == 0.001)
condClean <- subset(cond.epu, cond.epu$INDWT > 0.001)


condsd <- merge(condClean, condstdev, by='SVSPP', all.cond.epu=T, all.condClean = F)
cond.sd <- subset(condsd, condsd$RelCond < (100+condsd$condSD) & condsd$RelCond > (100-condsd$condSD))
#cond.sd <- subset(condsd, condsd$RelCond>=100-condsd$condSD | condsd$RelCond<=100+condsd$condSD)

cond.epu <- cond.sd %>% dplyr::filter(is.na(SEX) | SEX != 0) # remove all other category for sex (when I used != c(0, 4) it didn't remove all 4s)
cond.epu <- cond.epu %>% dplyr::filter(is.na(SEX) | SEX != 4)

cond.epu <- cond.epu %>% dplyr::mutate(sex = SEX)

#recoding SEX (1,2) to sex (M, F)
cond.epu$sex[cond.epu$SEX==1] <- 'M'
cond.epu$sex[cond.epu$SEX==2] <- 'F'



#Tried to use LOGGED_SPECIES_NAME for species names, but doesn't exist before 2001 and too many version of names
#Names for SVSPP codes '013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197':
cond.epu <- cond.epu %>% dplyr::mutate(Species = SVSPP)

cond.epu$Species[cond.epu$SVSPP==013] <- 'Smooth Dogfish'
cond.epu$Species[cond.epu$SVSPP==015] <- 'Spiny Dogfish'
cond.epu$Species[cond.epu$SVSPP==023] <- 'Winter Skate'
cond.epu$Species[cond.epu$SVSPP==026] <- 'Little Skate'
cond.epu$Species[cond.epu$SVSPP==028] <- 'Thorny Skate'
cond.epu$Species[cond.epu$SVSPP==032] <- 'Atl Herring'
cond.epu$Species[cond.epu$SVSPP==072] <- 'Silver Hake'
cond.epu$Species[cond.epu$SVSPP==073] <- 'Atl Cod'
cond.epu$Species[cond.epu$SVSPP==074] <- 'Haddock'
cond.epu$Species[cond.epu$SVSPP==075] <- 'Pollock'
cond.epu$Species[cond.epu$SVSPP==076] <- 'White Hake'
cond.epu$Species[cond.epu$SVSPP==077] <- 'Red Hake'
cond.epu$Species[cond.epu$SVSPP==078] <- 'Spotted Hake'
cond.epu$Species[cond.epu$SVSPP==102] <- 'American Plaice'
cond.epu$Species[cond.epu$SVSPP==103] <- 'Summer Flounder'
cond.epu$Species[cond.epu$SVSPP==104] <- 'Fourspot'
cond.epu$Species[cond.epu$SVSPP==105] <- 'Yellowtail'
cond.epu$Species[cond.epu$SVSPP==106] <- 'Winter Flounder'
cond.epu$Species[cond.epu$SVSPP==107] <- 'Witch Flounder'
cond.epu$Species[cond.epu$SVSPP==108] <- 'Windowpane Flounder'
cond.epu$Species[cond.epu$SVSPP==121] <- 'Mackerel'
cond.epu$Species[cond.epu$SVSPP==131] <- 'Butterfish'
cond.epu$Species[cond.epu$SVSPP==135] <- 'Bluefish'
cond.epu$Species[cond.epu$SVSPP==141] <- 'Black Sea Bass'
cond.epu$Species[cond.epu$SVSPP==143] <- 'Scup'
cond.epu$Species[cond.epu$SVSPP==145] <- 'Weakfish'
cond.epu$Species[cond.epu$SVSPP==155] <- 'Acadian Redfish'
cond.epu$Species[cond.epu$SVSPP==164] <- 'Sea Raven'
cond.epu$Species[cond.epu$SVSPP==193] <- 'Ocean Pout'
cond.epu$Species[cond.epu$SVSPP==197] <- 'Goosefish'

#Summarize annually and filter based on count of condition data by species
annualcond <- cond.epu %>% dplyr::group_by(Species, sex, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condNshelf <- dplyr::filter(annualcond, nCond>=3)
condNshelfSpp <- condNshelf %>% dplyr::add_count(Species, sex) %>% 
  dplyr::filter(n >= 20)


#Summarize annually by EPU
annualcondEPU <- cond.epu %>% dplyr::group_by(Species,EPU, sex, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcondEPU, nCond>=3)
condNSppEPU <- condN %>% dplyr::add_count(Species, EPU, sex) %>% 
  dplyr::filter(n >= 20)

#Output for socio-economic models:
annualcondEPUlen <- cond.epu %>% dplyr::group_by(Species,SVSPP, EPU, YEAR, LENGTH) %>% dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n())
#condN <- dplyr::filter(annualcondEPU, nCond>=3)
condNSppEPUlen <- annualcondEPUlen %>% dplyr::add_count(Species, EPU) 
#%>% 
#  dplyr::filter(n >= 20)

condEPUlen <- condNSppEPUlen 
readr::write_csv(condEPUlen, here::here(out.dir,"RelCond2019_EPU_length.csv"))

#Summarize annually by Strata
annualcondStrata <- cond.epu %>% dplyr::group_by(Species,STRATUM, sex, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcondStrata, nCond>=3)
condNSppStrata <- condN %>% dplyr::add_count(Species, STRATUM, sex) %>% 
  dplyr::filter(n >= 20)
#Format output to be read into plotting function:


#View(annualcond)
#condFormat <- tidyr::gather(condN, key= c("Species", "EPU", "sex"), value = "MeanCond", -YEAR)
#condFormat <- tidyr::spread(condN, key = c("Species", "EPU", "sex"), value = 'MeanCond', -YEAR)

readr::write_csv(condNshelfSpp, here::here(out.dir,"AnnualRelCond2019_shelf.csv"))

condSS <- condNSpp %>% dplyr::filter(EPU == "SS")
readr::write_csv(condSS, here::here(out.dir,"AnnualRelCond2019_SS.csv"))

condGOM <- condNSpp %>% dplyr::filter(EPU == "GOM")
readr::write_csv(condGOM, here::here(out.dir,"AnnualRelCond2019_GOM.csv"))

condGB <- condNSpp %>% dplyr::filter(EPU == "GB")
readr::write_csv(condGB,here::here(out.dir, "AnnualRelCond2019_GB.csv"))

condMAB <- condNSpp %>% dplyr::filter(EPU == "MAB")
readr::write_csv(condMAB, here::here(out.dir,"AnnualRelCond2019_MAB.csv"))


#select fluke annual condition data:
#condfluke <- filter(annualcond, SVSPP==103, SEX==2)
#group_by(condfluke, YEAR, SEX)
#View(condfluke)
#fluke <- condfluke %>% drop_na(YEAR)
#describe(stom)
#unique(stom$YEAR)
#View(fluke)

#write.csv(fluke, "Fluke_Cond.csv")

#Comment out output while adding in code and running outside of the function
return(list(condNshelfSpp=condNshelfSpp,condGB=condGB,condSS=condSS,condGOM=condGOM,condMAB=condMAB,stom = stom.epu))

}
