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

#Average stomach fullness by Species, YEAR, EPU and sex for the year before
AvgStom <- stom.epu %>% dplyr::group_by(Species, YEAR, EPU, sex) %>% dplyr::mutate(AvgStomFull=mean(stom_full, na.rm=TRUE))
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


#GAM analyses relating condition to environmental parameters, by species:

spp <- unique(CondClean$Species)
datalist = list()

#for(sp in spp) {
#condSPP <- CondClean %>% dplyr::filter(Species==sp)
  
#turn on for testing a single species outside of loop:
condSPP <- CondClean %>% dplyr::filter(Species=='Spiny Dogfish')

   form.cond <- formula(RelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHWT, k=10) +s(LON, LAT, k=25) +s(AvgStomFullLag, k=10) +(YEAR), data=condSPP)
                         #+s(stom_full, k=10)                      
                        #Can add factor variable as a by variable: e.g. +s(LON, LAT, k=25, by = EPU)
                        #EXPCATCHWT had slightly more significance than EXPCATCHNUM for more species
  
   condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)
  
#    step.cond <- step.Gam(condGAM, scope= list("BOTTEMP" =~1+BOTTEMP+s(BOTTEMP),
#                                              "EXPCATCHNUM" =~1+EXPCATCHNUM+s(EXPCATCHNUM),
#                                              "LON, LAT" =~1+LON,LAT +s(LON,LAT),
#                                              "YEAR" =~1+YEAR))

GAMstats <- summary(condGAM)

SumCondGAM <- t(c(sp, round(GAMstats$s.pv,3),  round(GAMstats$r.sq,3), round(GAMstats$dev.expl,3),  round(GAMstats$sp.criterion,3)))

dl=data.frame(SumCondGAM)
GAMnames=c('Species', 'Bottom Temp', 'Local Biomass', 'Lat Lon', 'Stomach Fullness', 'R sq.', 'Deviance Explained', 'GCV')
#error if you try to add YEAR to GAMnames because GAM doesn't include YEAR as a variable.
names(dl)=GAMnames
datalist[[sp]] <- dl

#Use for testing plot with single species
#filename <-here::here(out.dir, paste0('Spiny Dogfish_condition.jpg'))

   filename <- here::here(out.dir,paste0(sp,"_condition.jpg"))
   jpeg(filename)
   par(mfrow=c(2,2), mar=c(2.15,2.15,0.15,0.25), mgp=c(0.25,1,0), cex=0.75, tck=-0.015)
   plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
   dev.off()
   
   #plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
 
   
### gam.check not working, erorr says could not find function "gam.check"
#   gam.check(condGAM) # run model checks including checking smoothing basis dimensions
#}

AllSPP = do.call(rbind, datalist)

readr::write_csv(AllSPP, here::here(out.dir,"GAM_Condition_test.csv"))   

