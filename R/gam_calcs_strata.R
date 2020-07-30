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

#-------------------------------------------------------------
#Not working currently to source, but reminder to run these files first:
#source("R/StomFullnessData_allfh.R")
#source("R/RelConditionEPU.R")

#Explore different ways of aggregating Relative Condition:
#Creating Average Relative Condition and Average Stomach Fullness by tow, species, sex
  #For use in multi-model dataset:
#AvgTowCond <- cond.epu %>% group_by(CRUISE6, STRATUM, STATION, TOW, Species, sex) %>% 
#  mutate(AvgTowRelCond=(mean(RelCond)), AvgTowRelCondSD=(sd(RelCond)))

#Creating Average Relative Condition by strata, species, sex
AvgStrataCond <- cond.epu %>% group_by(CRUISE6, STRATUM, Species, sex) %>% 
  mutate(AvgRelCondStrata=(mean(RelCond)), AvgRelCondStrataSD = (sd(RelCond)), AvgExpcatchwtStrata = (mean(EXPCATCHWT)),
         AvgExpcatchnumStrata= (mean(EXPCATCHNUM)), AvgLatStrata = (mean(LAT)), 
        AvgLonStrata = (mean(LON)), AvgBottomTempStrata = (mean(BOTTEMP))) 
#        %>%  distinct(AvgRelCondStrata, .keep_all = T))

#Creating Average Relative Condition and Average Stomach Fullness by EPU, species, sex
#Couldn't run mechanisms model because data too sparse:
#AvgEPUCond <- stom.epu %>% group_by(CRUISE6, EPU, Species, sex) %>% 
#  mutate(AvgRelCondEPU=(mean(RelCond)), AvgStomFullEPU=(mean(stom_full))) %>%
#  distinct(AvgRelCondEPU, .keep_all = T)
#---------------------------------------------------------------

#Bringing in average temperature data
AvgTempSpringData <- readr::read_csv(here::here(data.dir, "AverageTempSpring.csv"))
AvgTempSummerData <- readr::read_csv(here::here(data.dir, "AverageTempSummer.csv"))
AvgTempFallData <- readr::read_csv(here::here(data.dir, "AverageTempFall.csv"))
AvgTempWinterData <- readr::read_csv(here::here(data.dir, "AverageTempWinter.csv"))

AvgTempSpringFormat <- AvgTempSpringData %>% dplyr::rename(YEAR=Year) %>%
  gather(EPU, AvgTempSpring, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempSummerFormat <- AvgTempSummerData %>% dplyr::rename(YEAR=Year) %>%
  gather(EPU, AvgTempSummer, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempFallFormat <- AvgTempFallData %>% dplyr::rename(YEAR=Year) %>%
  gather(EPU, AvgTempFall, c(GB, GOM,SS, MAB), na.rm=F)

AvgTempWinterFormat <- AvgTempWinterData %>% dplyr::rename(YEAR=Year) %>%
  gather(EPU, AvgTempWinter, c(GB, GOM,SS, MAB), na.rm=F)

AvgTemp <- Reduce(dplyr::full_join, list(AvgTempWinterFormat, AvgTempSpringFormat, AvgTempSummerFormat, AvgTempFallFormat))

CondAvgTemp <- dplyr::left_join(AvgStrataCond, AvgTemp, by=c("YEAR", "EPU"))

#--------------------------------------------------------------------------------
#Bringing in ratio of small to large copepods
load(here::here("data","1977_2017_SLI_Calfin_Pseudo_Ctyp.rdata"))
#View(Zooplankton_Primary_Prod)
Calfin <- Zooplankton_Primary_Prod
#head(Calfin)
CalfinFormat <- Calfin %>% dplyr::rename(YEAR = year) %>% select(YEAR, SLI.gbk, SLI.gom, SLI.mab, SLI.scs) %>% 
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
AvgStom <- dplyr::left_join(CondZoo, stom.data.strata, by = c('YEAR', 'SEASON', 'STRATUM', 'EPU', 'Species', 'SEX'))

#spring stomach fullness merge by strata for Condition GAM lag:
AvgStomSpr <- dplyr::left_join(CondZoo, stom.spring.strata, by = c('YEAR', 'STRATUM', 'EPU', 'Species', 'SEX')) %>%
select('YEAR', 'STRATUM', 'EPU', 'Species', 'sex',
       'AvgRelCondStrata', 'AvgRelCondStrataSD', 'AvgExpcatchwtStrata', 'AvgExpcatchnumStrata',
       'AvgLatStrata', 'AvgLonStrata', 'AvgBottomTempStrata',
       'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall','CopepodSmallLarge',
       'ZooplBiomassAnomaly', 'AvgStomFullSpringStrata') %>%
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
          'ZooplBiomassAnomaly', 'AvgStomFullStratalag') %>%
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


######End code before GAM analyses#####


#---------------------------------------------------------------------------------
#allfh data includes a better audit of food habits data and eliminates the need for these removals:
#Removed outlier where American Plaice stom_full >0.3, 
#Removed 4 outliers where Butterfish STOM_VOLUME >10,
#Removed 1 outlier where spotted hake EXPCATCHNUM >5000
#CondClean <- AvgStomStrataLag 

 #Remove Bluefish since not enough spring stomach samples
  CondClean <- AvgStomSpr%>%
   dplyr::filter(!(Species == "Bluefish"))
#%>% dplyr::filter(
  #(is.na(AvgStomFullStrata) | !(Species == "American Plaice" & AvgStomFullStrata >0.3)),
   #                                     (is.na(STOM_VOLUME) | !(Species == "Butterfish" & STOM_VOLUME >10)),
 #                                       (is.na(EXPCATCHNUM) | !(Species == "Spotted Hake" & EXPCATCHNUM >5000)))


#--------------------------------------------------------------------------------
####GAM analyses relating condition to environmental parameters, by species:
#remove Sea Raven for LON/LAT runs since not enough data:
#CondClean <- CondClean %>% dplyr::filter(Species!="Sea Raven")

spp <- unique(CondClean$Species)
datalist = list()

for(sp in spp) {
condSPP <- CondClean %>% dplyr::filter(Species==sp)
  
#turn on for testing a single species outside of loop:
#condSPP <- CondClean %>% dplyr::filter(Species=='Goosefish') %>% mutate(sp='Goosefish')

#Full model
#   form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring) +s(YEAR), data=condSPP)
#Single index
#form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
 # form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10), data=condSPP)
# form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHWT, k=10), data=condSPP)
 # form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchwtStrata, k=10), data=condSPP)
# form.cond <- formula(AvgRelCondStrata ~ s(AvgExpcatchnumStrata, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(EXPCATCHNUM, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(LON, LAT, k=25), data=condSPP)
# form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStrata, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullStratalag, k=10), data=condSPP)
form.cond <- formula(AvgRelCondStrata ~ s(AvgStomFullSpringStrata, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(CopepodSmallLarge, k=10), data=condSPP)
#form.cond <- formula(AvgRelCondStrata ~ s(ZooplBiomassAnomaly, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSpring, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempSummer, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempFall, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgTempWinter, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10), data=condSPP)
#Eplains highest deviance:
#  form.cond <- formula(AvgRelCondStrata ~ s(YEAR, k=10) +s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgLonStrata, AvgLatStrata, k=25) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#Mechanisms model:
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#  form.cond <- formula(AvgRelCondStrata ~ s(AvgBottomTempStrata, k=10) +s(AvgExpcatchwtStrata, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSummer, k=10), data=condSPP)
  

                        #Can add factor variable as a by variable: e.g. +s(LON, LAT, k=25, by = EPU)
                        #EXPCATCHWT had slightly more significance than EXPCATCHNUM for more species
  
   condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)
  
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


#Model with highest deviance explained:
#GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'YEAR', 'Bottom Temp Strata', 'Local Biomass Strata', 'LON LAT strata', 'AvgStomFullLag', 'Zooplankton Biomass Anomaly', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')

#single variable runs
GAMnames=c('Species', 'AvgStomFullSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'AvgTempSummer', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'AvgTempFall', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'AvgTempWinter', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'ZooplBiomassAnomaly', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#GAMnames=c('Species', 'CopepodSmallLarge', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# GAMnames=c('Species', 'Bottom Temp Strata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# GAMnames=c('Species', 'AvgExpcatchwtStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')
# GAMnames=c('Species', 'AvgExpcatchnumStrata', 'R sq.', 'Deviance Explained', 'GCV', 'n')


#error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
names(dl)=GAMnames
datalist[[sp]] <- dl

#Use for testing plot with single species
#filename <-here::here(out.dir, paste0('GoosefishYEAR_condition.jpg'))

#Single variable output:
   filename <- here::here(out.dir,paste0(sp,"_StomFullSpringStrata_AvgCondStrata.jpg"))
#   filename <- here::here(out.dir,paste0(sp,"_YEAR_AvgCondStrata.jpg"))
#   filename <- here::here(out.dir,paste0(sp,"_AvgTempSpring_AvgCondStrata.jpg"))
#   filename <- here::here(out.dir,paste0(sp,"_AvgTempSummer_AvgCondStrata.jpg"))
#   filename <- here::here(out.dir,paste0(sp,"_AvgTempFall_AvgCondStrata.jpg"))
#   filename <- here::here(out.dir,paste0(sp,"_AvgTempWinter_AvgCondStrata.jpg"))
#    filename <- here::here(out.dir,paste0(sp,"_ZooplBiomassAnomaly_AvgCondStrata.jpg"))
#    filename <- here::here(out.dir,paste0(sp,"_CopepodSmallLarge_AvgCondStrata.jpg"))
    # filename <- here::here(out.dir,paste0(sp,"_BottomTempStrata_AvgCondStrata.jpg"))
    # filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchwtStrata_AvgCondStrata.jpg"))
#    filename <- here::here(out.dir,paste0(sp,"_AvgExpcatchnumStrata_AvgCondStrata.jpg"))
    
#Full model output:
# filename <- here::here(out.dir,paste0(sp,"_HighesDevExplYr_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))

#Mechanism model:
#filename <- here::here(out.dir,paste0(sp,"_Mechanisms_StomFullStrata_ZooplBiomass_AvgCondStrata.jpg"))
#filename <- here::here(out.dir,paste0(sp,"_Mechanisms_SummerTemp_StomFullStratalag_ZooplBiomass_AvgCondStrata.jpg"))

    jpeg(filename)
   par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
   plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
   dev.off()
   
   #plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
 
   
#gam.check (run model checks including checking smoothing basis dimensions)
#   sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_AvgCondStrata.txt")))
   
#   mgcv::gam.check(condGAM) 
   
#   sink()
}

AllSPP = do.call(rbind, datalist)

#Full model output:
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_HighestDevExplYr_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   

#Mechanisms model:
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Mechanisms_SummerTemp_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   

#Single variable output:
readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_StomFullSpringStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_Year_AvgCondStrata.csv"))     
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSpring_AvgCondStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempSummer_AvgCondStrata.csv"))     
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempFall_AvgCondStrata.csv"))       
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgTempWinter_AvgCondStrata.csv"))   
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_ZooplBiomassAnomaly_AvgCondStrata.csv"))   
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_CopepodSmallLarge_AvgCondStrata.csv"))  
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_BottomTempStrata_AvgCondStrata.csv")) 
# readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchwtStrata_AvgCondStrata.csv")) 
#readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgExpcatchnumStrata_AvgCondStrata.csv"))

