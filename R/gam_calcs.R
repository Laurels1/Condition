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
  
#Removed outlier where American Plaice stom_full >0.3, 
#Removed 4 outliers where Butterfish STOM_VOLUME >10,
#Removed 1 outlier where spotted hake EXPCATCHNUM >5000
##Need to figure out how to not drop NAs
CondClean <- stom.epu %>% dplyr::filter(!(is.na(STOM_VOLUME) | Species == "American Plaice" & stom_full >0.3),
                                        !(is.na(STOM_VOLUME) | Species == "Butterfish" & STOM_VOLUME >10),
                                        !(is.na(EXPCATCHNUM) | Species == "Spotted Hake" & EXPCATCHNUM >5000))


   #GAM analyses relating condition to environmental parameters, by species:

spp <- unique(stom.epu$Species)
datalist = list()

#spp <- spp[1]
for(sp in spp) {
condSPP <- stom.epu %>% dplyr::filter(Species==sp)
  
condSPP <- stom.epu %>% dplyr::filter(SVSPP==13)

   form.cond <- formula(RelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHNUM, k=10) +s(LON, LAT, k=25) +s(stom_full, k=10), data=condSPP)
                        #+s(EPU)  + s(stom_full))
  
   condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)
  
#    step.cond <- step.Gam(condGAM, scope= list("BOTTEMP" =~1+BOTTEMP+s(BOTTEMP),
#                                              "EXPCATCHNUM" =~1+EXPCATCHNUM+s(EXPCATCHNUM),
#                                              "LON, LAT" =~1+LON,LAT +s(LON,LAT),
#                                              "YEAR" =~1+YEAR))
  
## Can't add output dir to sink
#sink(paste0(out.dir/sp,"_condition.txt"))
#print(summary(condGAM))
#sink()

GAMstats <- summary(condGAM)

SumCondGAM <- c(Species = sp, p.coeff = GAMstats$s.pv, r.sp = GAMstats$r.sq, dev.expl = GAMstats$dev.expl, gcv = GAMstats$sp.criterion)
SumCondGAM$sp <- sp
datalist[[sp]] <- SumCondGAM


   filename <- here::here(out.dir,paste0(sp,"_condition.jpg"))
   jpeg(filename)
   plot(condGAM, pages=1, residuals=TRUE, rug=T, cex=1.05, lwd=6) #show partial residuals
   dev.off()
   
   #plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
 
   
### gam.check not working, erorr says could not find function "gam.check"
 #  gam.check(condGAM) # run model checks including checking smoothing basis dimensions
}

AllSPP = do.call(rbind, datalist)

readr::write_csv(AllSPP, here::here(out.dir,"GAM_Condition_Summary.csv"))   

#   stepgam <- gam::step.Gam(condGAM,scope=list(
#     "BOTTEMP" = ~ 1 + BOTTEMP + lo(BOTTEMP) + S(BOTTEMP)),
#     trace =T)
  
#   summary (stepgam)
#   gamrun1.se <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6, se=T)
#   gamrun1.res <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6,residuals=T)

  
#}
