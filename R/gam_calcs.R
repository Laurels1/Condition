#' gam stuff
#'
#'@param annualcond tibble. Output from RelConditionEPU.R
#'@param out.dir character string. name of directory in which plots and data files will be saved
#'

library(gam)
library(dplyr)

#turn off function while changing code
#gam_calcs <- function(annualcond,out.dir="output") {
  
#   RelCond <- stom.epu
   #GAM analyses relating condition to environmental parameters:
##Need to loop over species:
SppGAM <- dplyr::do(stom.epu, group_by(Species))

   form.cond <- formula(RelCond ~ s(BOTTEMP) +s(EXPCATCHNUM))
                        #+s(EPU)  + s(stom_full))
  
   condGAM <- gam::gam(form.cond, family= gaussian, data=stom.epu)
  
   summary(condGAM)
   
   plot(condGAM, pages=1, residuals=TRUE) #show partial residuals
   plot(condGAM, pages=1, seWithMean=TRUE) #'with intercept' CIs
   
### plot GAM not working, erorr says could not find function "gam.check"
   gam.check(condGAM) # run model checks including checking smoothing basis dimensions
  
#   stepgam <- gam::step.Gam(condGAM,scope=list(
#     "BOTTEMP" = ~ 1 + BOTTEMP + lo(BOTTEMP) + S(BOTTEMP)),
#     trace =T)
  
#   summary (stepgam)
#   gamrun1.se <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6, se=T)
#   gamrun1.res <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6,residuals=T)

  
#}
