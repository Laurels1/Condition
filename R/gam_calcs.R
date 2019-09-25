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

#Creating average Relative Condition and stomach fullness by tow, species, sex
AvgTowCond <- stom.epu %>% group_by(CRUISE6, STRATUM, STATION, TOW, Species, sex) %>% 
  mutate(AvgTowRelCond=(mean(RelCond)), AvgTowStomFull=(mean(stom_full))) %>%
  distinct(AvgTowRelCond, .keep_all = T)

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

CondAvgTemp <- dplyr::left_join(AvgTowCond, AvgTemp, by=c("YEAR", "EPU"))

#Bringing in ratio of small to large copepods
load("~/EDAB/Condition/Condition/data/1977_2017_SLI_Calfin_Pseudo_Ctyp.rdata")
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
  
#Average stomach fullness by Species, YEAR, EPU and sex for the year before
AvgStom <- CondCal %>% dplyr::group_by(Species, YEAR, EPU, sex) %>% dplyr::mutate(AvgStomFull=mean(stom_full, na.rm=TRUE))
##Can't get lag and mutate to work
#AvgStomLag <- AvgStom %>% dplyr::mutate(AvgStomLag1=(AvgStomFull %in% YEAR-1))
#AvgStomLag <- AvgStom %>% dplyr::lag(AvgStomLag1=(AvgStomFull, n=1)
#Clunky way of lagging but it works:
A <- AvgStom %>% select(YEAR, Species, EPU, sex, AvgStomFull)
B <- unique(A)
C <- B %>% dplyr::mutate(YEARstom= YEAR)
D <- C %>% dplyr::ungroup()
E <- D %>% dplyr::select(Species, YEARstom, EPU, sex, AvgStomFullLag=AvgStomFull)
Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
AvgStom2 <- AvgStom %>% dplyr::select(-c(AvgStomFull))
AvgStomLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("Species", "YEAR", "EPU", "sex"))

#Removed outlier where American Plaice stom_full >0.3, 
#Removed 4 outliers where Butterfish STOM_VOLUME >10,
#Removed 1 outlier where spotted hake EXPCATCHNUM >5000
CondClean <- AvgStomLag %>% dplyr::filter((is.na(stom_full) | !(Species == "American Plaice" & stom_full >0.3)),
                                        (is.na(STOM_VOLUME) | !(Species == "Butterfish" & STOM_VOLUME >10)),
                                        (is.na(EXPCATCHNUM) | !(Species == "Spotted Hake" & EXPCATCHNUM >5000)))


####GAM analyses relating condition to environmental parameters, by species:

spp <- unique(CondClean$Species)
datalist = list()

for(sp in spp) {
condSPP <- CondClean %>% dplyr::filter(Species==sp)
  
#turn on for testing a single species outside of loop:
#condSPP <- CondClean %>% dplyr::filter(Species=='Goosefish') %>% mutate(sp='Goosefish')

#Full model
#   form.cond <- formula(AvgTowRelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring) +s(YEAR), data=condSPP)
#Single index
#  form.cond <- formula(AvgTowRelCond ~ s(BOTTEMP, k=10), data=condSPP)
#  form.cond <- formula(AvgTowRelCond ~ s(EXPCATCHWT, k=10), data=condSPP)
#  form.cond <- formula(AvgTowRelCond ~ s(EXPCATCHNUM, k=10), data=condSPP)
#  form.cond <- formula(AvgTowRelCond ~ s(LON, LAT, k=25), data=condSPP)
#  form.cond <- formula(AvgTowRelCond ~ s(AvgStomFullLag, k=10), data=condSPP)
#  form.cond <- formula(AvgTowRelCond ~ s(stom_full, k=10), data=condSPP)
  form.cond <- formula(AvgTowRelCond ~ s(CopepodSmallLarge, k=10), data=condSPP)
  #form.cond <- formula(AvgTowRelCond ~ s(AvgTempSpring, k=10), data=condSPP)
  #form.cond <- formula(AvgTowRelCond ~ s(AvgTempSummer, k=10), data=condSPP)
  #form.cond <- formula(AvgTowRelCond ~ s(AvgTempFall, k=10), data=condSPP)
  #form.cond <- formula(AvgTowRelCond ~ s(AvgTempWinter, k=10), data=condSPP)
#Eplains highest deviance:
  #form.cond <- formula(AvgTowRelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(stom_full, k=10) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring), data=condSPP)
#Mechanisms model:
  #form.cond <- formula(AvgTowRelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(AvgStomFullLag, k=10) +s(CopepodSmallLarge) +s(AvgTempSpring), data=condSPP)


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
#GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'LON LAT', 'AvgStomFull', 'CopepodSL', 'AvgTempSpring', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
GAMnames=c('Species', 'CopepodSL', 'R sq.', 'Deviance Explained', 'GCV', 'n')


#error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
names(dl)=GAMnames
datalist[[sp]] <- dl

#Use for testing plot with single species
#filename <-here::here(out.dir, paste0('GoosefishYEAR_condition.jpg'))

   filename <- here::here(out.dir,paste0(sp,"_CopepodSL_AvgCond.jpg"))
   jpeg(filename)
   par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
   plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
   dev.off()
   
   #plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
 
   
### gam.check not working, erorr says could not find function "gam.check"
#   gam.check(condGAM) # run model checks including checking smoothing basis dimensions
}

AllSPP = do.call(rbind, datalist)

readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgRelCond_CopepodSL.csv"))   

