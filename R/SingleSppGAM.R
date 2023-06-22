#Single species GAM run:

library(mgcv)
library(gam)
library(dplyr)
library(readr)

out.dir = "output"
data.dir <- "data"

gis.dir  <- "gis"

#CreatAvgStrataConding Average Relative Condition by strata, species, sex
AvgStrataCond <- cond.epu %>% group_by(CRUISE6, STRATUM, Species, sex) %>% 
  mutate(AvgRelCondStrata=(mean(RelCond)), AvgExpcatchwtStrata = (mean(EXPCATCHWT)),
         AvgExpcatchnumStrata= (mean(EXPCATCHNUM)), AvgLatStrata = (mean(LAT))) %>%
  distinct(AvgRelCondStrata, .keep_all = T)

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
AvgStomFullStrata <- stomdata %>% dplyr::filter(season == "FALL") %>%
  group_by(year, STRATUM, Species, pdsex) %>% 
  mutate(AvgStomFullStrata=(mean(stom_full)))

#Creating Average Fall Stomach Fullness by year, EPU, species, sex
AvgStomFullEPU <- AvgStomFullStrata %>% dplyr::filter(season == "FALL") %>%
  group_by(year, EPU, Species, pdsex) %>% 
  mutate(AvgStomFullEPU=(mean(stom_full)))

# change stomach data variables to merge with condition data: 
stom.data <- AvgStomFullEPU %>% dplyr::mutate(YEAR = year, SEASON = season, INDID = pdid, SEX = pdsex, INDWT = pdwgt) %>%
  distinct(YEAR, STRATUM, EPU, Species, SEX, .keep_all = TRUE) %>%   select(YEAR, STRATUM, EPU, Species, SEASON, SEX, AvgStomFullEPU, AvgStomFullStrata)

#merge stomach fullness into condition data:
#make sure allfh data includes STRATUM as factor with leading zero for merge
AvgStom <- dplyr::left_join(CondZoo, stom.data, by = c('YEAR', 'STRATUM', 'EPU', 'Species', 'SEX'))

#Clunky way of lagging but it works:
A <- AvgStom %>% select(YEAR, Species, STRATUM, EPU, sex, AvgStomFullStrata)
B <- unique(A)
C <- B %>% dplyr::mutate(YEARstom= YEAR)
D <- C %>% dplyr::ungroup()
E <- D %>% dplyr::select(Species, YEARstom, STRATUM, EPU, sex, AvgStomFullStratalag=AvgStomFullStrata)
Stomlag <- E %>% dplyr::mutate(YEAR = YEARstom+1)
AvgStom2 <- AvgStom %>% dplyr::select(-c(AvgStomFullStrata))
AvgStomStrataLag <- dplyr::left_join(AvgStom2, Stomlag, by=c("Species", "YEAR","STRATUM", "EPU", "sex")) %>%
  select('YEAR', 'CRUISE6', 'STRATUM', 'BOTTEMP', 'LAT', 'LON', 'EPU', 'Species', 'sex', 
         'EXPCATCHWT', 'EXPCATCHNUM', 'RelCond', 'condSD', 'AvgRelCondStrata',
         'AvgTempWinter', 'AvgTempSpring', 'AvgTempSummer', 'AvgTempFall','CalEPU', 'CopepodSmallLarge',
         'ZooplBiomassAnomaly', 'AvgStomFullStratalag')

#Removed outlier where American Plaice stom_full >0.3, 
#Removed 4 outliers where Butterfish STOM_VOLUME >10,
#Removed 1 outlier where spotted hake EXPCATCHNUM >5000
CondClean <- AvgStomStrataLag %>% dplyr::filter(
  #(is.na(AvgStomFullStrata) | !(Species == "American Plaice" & AvgStomFullStrata >0.3)),
  #                                     (is.na(STOM_VOLUME) | !(Species == "Butterfish" & STOM_VOLUME >10)),
  (is.na(EXPCATCHNUM) | !(Species == "Spotted Hake" & EXPCATCHNUM >5000)))


#--------------------------------------------------------------------------------
####GAM analyses relating condition to environmental parameters, by species:

datalist = list()

condSPP <- CondClean %>% dplyr::filter(Species=='Goosefish') %>% mutate(sp='Goosefish')

#Mechanisms model:
form.cond <- formula(AvgRelCondStrata ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(AvgStomFullStratalag, k=10) +s(ZooplBiomassAnomaly, k=10) +s(AvgTempSpring, k=10), data=condSPP)
#Can add factor variable as a by variable: e.g. +s(LON, LAT, k=25, by = EPU)

condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)

GAMstats <- summary(condGAM)

SumCondGAM <- t(c(sp, round(GAMstats$s.pv,3),  round(GAMstats$r.sq,3), round(GAMstats$dev.expl,3),  round(GAMstats$sp.criterion,3), GAMstats$n))

dl=data.frame(SumCondGAM)
#Full model output:
#GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'LON LAT', 'AvgStomFullLag', 'CopepodSL', 'AvgTempSpring', 'YEAR', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#Full Model with reasonable mechanisms relating to condition changes:
GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'AvgStomFullLag', 'Zooplanton Biomass Anomaly', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#Model with highest deviance explained:
#GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'LON LAT','Stomach fullness','AvgStomFullLag', 'CopepodSL', 'AvgTempSpring', 'R sq.', 'Deviance Explained', 'GCV', 'n')
#single variable runs
#GAMnames=c('Species', 'Zooplankton Biomass Anomaly', 'R sq.', 'Deviance Explained', 'GCV', 'n')


#error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
names(dl)=GAMnames
datalist[[sp]] <- dl

#Use for testing plot with single species
filename <-here::here(out.dir, paste0('GoosefishYEAR_condition.jpg'))

jpeg(filename)
par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
dev.off()

#plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs

#gam.check (run model checks including checking smoothing basis dimensions)
  sink(here::here(out.dir,paste0(sp,"_GAMcheck_Mechanisms_AvgCondStrata.txt")))

   mgcv::gam.check(condGAM)

   sink()

AllSPP = do.call(rbind, datalist)

readr::write_csv(AllSPP, here::here(out.dir,"GAM_Summary_AvgRelCondStrata_ZooplBiomassAnomaly.csv"))   
