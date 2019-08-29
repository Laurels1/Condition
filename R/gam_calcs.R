#' gam stuff
#'
#'@param annualCondition tibble. Output from RelConditionEPU.R
#'@param out.dir character string. name of directory in which plots and data files will be saved
#'
#'

gam_calcs <- function(annualCondition,out.dir="output") {
  
   RelCond <- annualCondition
   #GAM analyses relating condition to environmental parameters:
   form.cond <- formula(RelCond ~ s(BOTTEMP))
                        #+ s(EXPCATCHNUM) + s(stom_full))
  
   condGAM <- gam::gam(form.cond, family= gaussian, data=stom.epu)
  
  
   stepgam <- gam::step.Gam(condGAM,scope=list(
     "BOTTEMP" = ~ 1 + BOTTEMP + lo(BOTTEMP) + S(BOTTEMP)),
     trace =T)
  
   summary (stepgam)
   gamrun1.se <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6, se=T)
   gamrun1.res <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6,residuals=T)

  
}