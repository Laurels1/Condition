#' Pull age-length data from NEFSC survey database

#' @param pullNewData boolean. Should a connection to oracle be made to pull data or use existing data
#' @param out.dir character string. name of directory in which plots and data files will be saved
#'
#' @return list of 2 items
#' \item{annualcond}{tibble (n x 6) Condition factor for each species, sex over time}
#' \item{stom}{data.frame (m x 52). stomach contents by year, cruise, sex, species etc.}
#'
#' @export


source("R/connect_to_database.R")
source("R/age_pull_from_svdbs.R")
#Required packages
#library(devtools)
#devtools::install_github('slucey/RSurvey/Survdat', )
library(data.table)
# library(graphics)
# library(grDevices)
#library(TeachingDemos)
#library(dplyr)
#library(tidyr)
library(tidyverse)
#library(gapminder)
library(rgdal)
library(Survdat)
#library(RODBC)
#library(gam)
library(magrittr)
#library(readr)

#Turn off the function when running outside of function to test new code
SurveyAgeLen <- function(age_pullNewData=T,out.dir="output") {
  # create output directory if it doesnt exist
  if (!dir.exists(out.dir)) dir.create(out.dir)
  
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
  if (age_pullNewData == T) {
    # grabs data from oracle
    uid <- readline("Please Enter your username for oracle: ")
    message(paste0("Pulling data from svdbs ... please wait ... data will be saved in directory: ",here::here(out.dir)))
    AgeSurvey <- age_pull_from_svdbs(uid) 
    # save data pull
    dateOfPull <- format(Sys.time(), "%m-%d-%y")
    saveRDS(AgeSurvey,file = here::here(out.dir,paste0("NEFSC_survey_AgeLenData_",dateOfPull,".rds")))
    #  saveRDS(survey,file = here::here(out.dir,paste0("NEFSC_survey_data_8-15-19.rds")))
  } else {
    # Otherwise, load data below:
    #If not pulling from SVDBS, load NEFSC survey data:
    #load(file.path(data.dir, 'NEFSC_survey_data_8-15-19.RData', sep = ''))
    AgeSurvey <- readRDS(here::here(out.dir, "NEFSC_survey_AgeLenData_12-12-19.rds"))
  }
  
  
  fallAge <- AgeSurvey[AgeSurvey$SEASON == 'FALL',]
  
  fallAge$Species[fallAge$SVSPP==013] <- 'Smooth Dogfish'
  fallAge$Species[fallAge$SVSPP==015] <- 'Spiny Dogfish'
  fallAge$Species[fallAge$SVSPP==023] <- 'Winter Skate'
  fallAge$Species[fallAge$SVSPP==026] <- 'Little Skate'
  fallAge$Species[fallAge$SVSPP==028] <- 'Thorny Skate'
  fallAge$Species[fallAge$SVSPP==032] <- 'Atl Herring'
  fallAge$Species[fallAge$SVSPP==072] <- 'Silver Hake'
  fallAge$Species[fallAge$SVSPP==073] <- 'Atl Cod'
  fallAge$Species[fallAge$SVSPP==074] <- 'Haddock'
  fallAge$Species[fallAge$SVSPP==075] <- 'Pollock'
  fallAge$Species[fallAge$SVSPP==076] <- 'White Hake'
  fallAge$Species[fallAge$SVSPP==077] <- 'Red Hake'
  fallAge$Species[fallAge$SVSPP==078] <- 'Spotted Hake'
  fallAge$Species[fallAge$SVSPP==102] <- 'American Plaice'
  fallAge$Species[fallAge$SVSPP==103] <- 'Summer Flounder'
  fallAge$Species[fallAge$SVSPP==104] <- 'Fourspot'
  fallAge$Species[fallAge$SVSPP==105] <- 'Yellowtail'
  fallAge$Species[fallAge$SVSPP==106] <- 'Winter Flounder'
  fallAge$Species[fallAge$SVSPP==107] <- 'Witch Flounder'
  fallAge$Species[fallAge$SVSPP==108] <- 'Windowpane Flounder'
  fallAge$Species[fallAge$SVSPP==121] <- 'Mackerel'
  fallAge$Species[fallAge$SVSPP==131] <- 'Butterfish'
  fallAge$Species[fallAge$SVSPP==135] <- 'Bluefish'
  fallAge$Species[fallAge$SVSPP==141] <- 'Black Sea Bass'
  fallAge$Species[fallAge$SVSPP==143] <- 'Scup'
  fallAge$Species[fallAge$SVSPP==145] <- 'Weakfish'
  fallAge$Species[fallAge$SVSPP==155] <- 'Acadian Redfish'
  fallAge$Species[fallAge$SVSPP==164] <- 'Sea Raven'
  fallAge$Species[fallAge$SVSPP==193] <- 'Ocean Pout'
  fallAge$Species[fallAge$SVSPP==197] <- 'Goosefish'
  
  readr::write_csv(fallAge, here::here(out.dir,"NEFSC_fall_survey_AgeLen2018.csv"))
  
  return(list())
  
}
