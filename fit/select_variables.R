#' Grab appropriate variables for a fit.
#' 
#' For each species fit full model. Due to missing data some species wont have the same type of full model
#' for example. the cold pool is only relevant or some species, stomach data is missing for others

library(magrittr)
# load data
data <- readRDS(file=here::here("other","condSPP.rds")) %>% 
  dplyr::rename(CopepodSmall_Large = `CopepodSmall/Large`) %>%
  dplyr::ungroup()

cond <- data %>% dplyr::select(YEAR, CRUISE6, STRATUM, sex, AvgRelCondStrata, Species,SVSPP, LocalBiomass, LocalAbundance, 
                       StockBiomass,
                       Fproxy, 
                       LocalBottomTemp, PropColumnColdPool,
                       SpringTemp, SummerTemp, FallTemp, WinterTemp,
                       CopepodSmall_Large,
                       ZooplanktonBiomass,TotalCopepods, StomachFullness,FallBloomMagnitude ,FallBloomDuration,
                       AverageLatStrata,AverageLonStrata
                       )


# variables and levels
localDensity <- c("LocalBiomass","LocalAbundance")
populationDensity <- "StockBiomass"
fishing <- "Fproxy"
localEnv <- c("LocalBottomTemp","PropColumnColdPool")
broadEnv <- c("WinterTemp","SpringTemp","SummerTemp","FallTemp")
copepod <- "CopepodSmall_Large"
resource <- c("ZooplanktonBiomass","TotalCopepods","StomachFullness","FallBloomMagnitude","FallBloomDuration")
spatialLon <- "AverageLonStrata"
spatialLat <- "AverageLatStrata"

# create full matrix of models
modelScenarios <- expand.grid(populationDensity,
            fishing,
            localEnv,
            localDensity,
            broadEnv,
            copepod,
            resource)


# create a character vector of species names
speciesList <- cond %>%
  dplyr::distinct(Species) %>% 
  dplyr::pull()

modelResults <- list()

for (aspecies in speciesList) {  
  print(aspecies)
  
  # check for incomplete data
  
  
  
  
  # now loop through each of the models
  for (iloop in 1:nrow(modelScenarios)) {

    
    # pull variable names for the model
    modelSpecs <- as.vector(as.matrix(modelScenarios[iloop,]))
    # pull the variable data
    speciesData <- cond %>% 
      dplyr::filter(Species == aspecies) %>%
      dplyr::select(AvgRelCondStrata, dplyr::all_of(modelSpecs), AverageLonStrata, AverageLatStrata )
    
    # fit the GAM model
    # create formula. Each explanatory variable has k = 10.
    # Add spatialLon, spatialLat jointly
    
    # get character vector of explanatory variables
    explanatoryVariableNames <- speciesData %>% 
      dplyr::select(-AvgRelCondStrata,-AverageLonStrata,-AverageLatStrata) %>%
      names()
    
    # create the formula for the model
    mymodel <- paste("AvgRelCondStrata ", paste0("s(",explanatoryVariableNames,",k=10)" ,collapse=" + "), sep="~")
    mymodel <- as.formula(paste0(mymodel," + s(AverageLatStrata,AverageLonStrata, k=25)"))

    # fit the GAM
    modelFit <- mgcv::gam(formula = mymodel, data = speciesData)
    
    #store the results
    modelResults[[iloop]] <- summary(modelFit)
    

  }
  
  # sort models. pick best one, two, five and see how different.
  
  # forrard/backward stepwise fit for best model to determine sig variables
  
}


