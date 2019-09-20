#' gam stuff
#'
#'@param annualcond tibble. Output from RelConditionEPU.R
#'@param out.dir character string. name of directory in which plots and data files will be saved
#'

library(mgcv)
library(gam)
library(dplyr)

#turn off function while changing code
#gam_calcs <- function(annualcond,out.dir="output") {
  
#   RelCond <- stom.epu
   #GAM analyses relating condition to environmental parameters:
##Need to loop over species:
#SppGAM <- dplyr::do(stom.epu, group_by(Species))

spp <- unique(stom.epu$SVSPP)
#spp <- spp[1]
for(sp in spp) {
condSPP <- stom.epu %>% dplyr::filter(SVSPP==sp)
  
   form.cond <- formula(RelCond ~ s(BOTTEMP, k=10) +s(EXPCATCHNUM, k=10) +s(LON, LAT, k=25) +s(stom_full, k=10), data=condSPP)
                        #+s(EPU)  + s(stom_full))
  
   condGAM <- mgcv::gam(form.cond, family= gaussian, data=condSPP, select=T)
  
#    step.cond <- step.Gam(condGAM, scope= list("BOTTEMP" =~1+BOTTEMP+s(BOTTEMP),
#                                              "EXPCATCHNUM" =~1+EXPCATCHNUM+s(EXPCATCHNUM),
#                                              "LON, LAT" =~1+LON,LAT +s(LON,LAT),
#                                              "YEAR" =~1+YEAR))
  

sink(paste0(sp,"_condition.txt"))
print(summary(condGAM))
sink()


   filename <- paste0(sp,"condition.jpg")
   jpeg(filename)
   plot(condGAM, pages=1, residuals=TRUE, rug=T) #show partial residuals
   dev.off()
   
   #plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
 
   
### plot GAM not working, erorr says could not find function "gam.check"
 #  gam.check(condGAM) # run model checks including checking smoothing basis dimensions
}
   
#   stepgam <- gam::step.Gam(condGAM,scope=list(
#     "BOTTEMP" = ~ 1 + BOTTEMP + lo(BOTTEMP) + S(BOTTEMP)),
#     trace =T)
  
#   summary (stepgam)
#   gamrun1.se <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6, se=T)
#   gamrun1.res <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6,residuals=T)

  
#}
