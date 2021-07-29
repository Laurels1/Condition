#' Fits best model using all data
#'
#' Reads in results of fitted models then refits using a much larger pool of data
#' Rows are still removed if NAs are present
#'
#'
#'
#'
#'
library(magrittr)

final <- readRDS(here::here("output/automate","finalModels.RDS"))
# read in main data file 
cond <- readRDS(file=here::here("other","condSPP.rds")) %>% 
  dplyr::ungroup()

speciesList <- cond %>% 
  dplyr::distinct(Species) %>%
  dplyr::pull()

# for each species
fullModels <- list()
for (aspecies in speciesList) {
  message(paste0("Species: ",aspecies))
  speciesVars <- final[[aspecies]]$variables
  speciesFormula <- final[[aspecies]]$formula
  speciesn <- final[[aspecies]]$n
  
  if (is.null(speciesVars)) {
    message("Species not fitted: not enough data")
    next
  }
  # pull the data
  speciesData <- cond %>% 
    dplyr::filter(Species == aspecies) %>% 
    dplyr::select(AvgRelCondStrata,all_of(speciesVars))
  
  # remove rows that contain NAs.
  # mgcv::gam will do this automatically but we need some checks before hand
  # We need to calculate the number of distinct values of each var to make
  # sure there are enough degrees of freedom available for the smooth
  speciesData <- speciesData[!is.na(rowSums(speciesData)),]
  
  # fit the GAM
  speciesFit <- mgcv::gam(formula = speciesFormula, data = speciesData) #, na.action = na.action
  
  fullModels[[aspecies]]$model <- speciesFit
  fullModels[[aspecies]]$data <- speciesData 
  fullModels[[aspecies]]$summary <- summary(speciesFit)
  fullModels[[aspecies]]$nExtra <- c(summary(speciesFit)$n - speciesn,(summary(speciesFit)$n - speciesn)/speciesn)
}

saveRDS(fullModels,file = here::here("output/automate","fullModels.rds"))
