# for scott

############### #initial data ######################

# fishCondition <- stom.epu %>% dplyr::select(Species, sex, YEAR,EPU, RelCond)
# save(fishCondition,file=here::here("output","fishCondition.rda"))


############### adding copepods, temp, etc. #################################
# Note: CondClean is a variable created in gam_calcs.R which requires RelConditionEPU.R to be run (stom.epu) used
fishCondition <- CondClean %>% dplyr::select(Species, sex, YEAR,EPU, RelCond,EXPCATCHWT,EXPCATCHNUM,stom_full,
                           AvgTempWinter,AvgTempSpring,AvgTempFall,AvgTempSummer,CopepodSmallLarge)

save(fishCondition,file=here::here("output","fishCondition.rda"))
