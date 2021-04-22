#' Grab appropriate variables for a fit.
#' 
#' For each species fit full model. Due to missing data some species wont have the same type of full model
#' for example. the cold pool is only relevant or some species, stomach data is missing for others

source(here::here("fit","clean_data.R"))
source(here::here("fit","clean_model.R"))

library(magrittr)

#na.action = "na.omit" # "na.gam.replace" % mean of column
NANpropAllowed <- 0.5 # proportion of NA's in an explanatory variable before it is removed from the model
k <- 10
latlonk <- 25

# read in main data file 
data <- readRDS(file=here::here("other","condSPP.rds")) %>% 
  dplyr::rename(CopepodSmall_Large = `CopepodSmall/Large`) %>% # change variable name because of special character
  dplyr::ungroup()

# select subset of variables needed in analysis
cond <- data %>% dplyr::select(AvgRelCondStrata, Species, SVSPP, LocalBiomass, LocalAbundance,
                               StockBiomass,
                               Fproxy,
                               LocalBottomTemp, PropColumnColdPool,
                               SpringTemp, SummerTemp, FallTemp, WinterTemp,
                               CopepodSmall_Large,
                               ZooplanktonBiomass,TotalCopepods, StomachFullness,FallBloomMagnitude ,FallBloomDuration,
                               AverageLatStrata,AverageLonStrata,
                               AssessmentYear
)


# define variable and levels
localDensity <- c("LocalBiomass","LocalAbundance")
populationDensity <- "StockBiomass"
fishing <- "Fproxy"
localEnv <- c("LocalBottomTemp","PropColumnColdPool")
broadEnv <- c("WinterTemp","SpringTemp","SummerTemp","FallTemp")
copepod <- "CopepodSmall_Large"
resource <- c("ZooplanktonBiomass","TotalCopepods","StomachFullness","FallBloomMagnitude","FallBloomDuration")
spatialLon <- "AverageLonStrata"
spatialLat <- "AverageLatStrata"

# model should only contain one value from each variable
df <- data.frame(variables = c("localDensity","localDensity",
                               "populationDensity",
                               "fishing",
                               "localEnv","localEnv",
                               "broadEnv","broadEnv","broadEnv","broadEnv",
                               "copepod",
                               "resource","resource","resource","resource","resource"
                               ),
                 level = c(localDensity,
                           populationDensity,
                           fishing,
                           localEnv,
                           broadEnv,
                           copepod,
                           resource),
                 num = c(2,2,1,1,2,2,4,4,4,4,1,5,5,5,5,5))

# create full matrix of models. Note: leave out lat and lo. they will be added to all models jointly
modelScenarios <- expand.grid(populationDensity,
                              fishing,
                              localEnv,
                              localDensity,
                              broadEnv,
                              copepod,
                              resource)

# unique list of all variables. use this to pull out from main data file
allVars <- c(unique(as.vector(as.matrix(modelScenarios))),"AverageLonStrata", "AverageLatStrata")

# create a character vector of species names
speciesList <- cond %>%
  dplyr::distinct(Species) %>% 
  dplyr::pull()

modelResults <- list()

for (aspecies in speciesList) {  
  print(aspecies)
  
  # check for incomplete data. we use all variables to help clean and interpolate data
  allSpeciesData <- cond %>% 
    dplyr::filter(Species == aspecies)
  
  # remove any variables that have < NANpropAllowed
  cleanedData <- clean_data(allSpeciesData,allVars,NANpropAllowed,k)
  print(cleanedData$omittedVars)

  # now loop through each of the models
  for (iloop in 1:nrow(modelScenarios)) {
    message(paste0("model # ",iloop, " for ",aspecies))
    # pull variable names for the model
    modelSpecs <- as.vector(as.matrix(modelScenarios[iloop,]))

    # check to see if this model has variables filtered out
    modelSpecPlus <- clean_model(modelSpecs,df,cleanedData$omittedVars)
    
    if (is.null(modelSpecPlus$modelSpecs)) {
      dummyModel <- list()
      message("skip")
      dummyModel$sp.criterion <- NA
      names(dummyModel$sp.criterion) <- "GCV.Cp"
      modelResults[[iloop]] <- dummyModel
      next
    }
    modelSpecs <- modelSpecPlus$modelSpecs

    # pull the variable data
    if (modelSpecPlus$latlon) { # use lat and lon
      
      speciesData <- cleanedData$data %>% 
        dplyr::select(AvgRelCondStrata, dplyr::all_of(modelSpecs), AverageLonStrata, AverageLatStrata )
      
    } else { # dont use lat and lon
      
      speciesData <- cleanedData$data %>% 
        dplyr::select(AvgRelCondStrata, dplyr::all_of(modelSpecs))

    }
    
    # fit the GAM model
    # create formula. Each explanatory variable has k = 10.
    # Add spatialLon, spatialLat jointly
    
    # get character vector of explanatory variables
    explanatoryVariableNames <- speciesData %>% 
      dplyr::select(-AvgRelCondStrata,-AverageLonStrata,-AverageLatStrata) %>%
      names()
    
    # create the formula for the model
    mymodel <- paste("AvgRelCondStrata ", paste0("s(",explanatoryVariableNames," ,bs=\"ts\",k=",k,")" ,collapse=" + "), sep="~")
    if (modelSpecPlus$latlon) {
      mymodel <- paste0(mymodel," + s(AverageLatStrata,AverageLonStrata, bs=\"ts\", k=",latlonk,")")
    }

    if (nrow(speciesData) < 100) next
    # fit the GAM
    modelFit <- mgcv::gam(formula = as.formula(mymodel), data = speciesData) #, na.action = na.action
    
    #store the results
    modelResults[[iloop]] <- summary(modelFit)
    
  }
  
  # sort models. pick best one, two, five and see how different.
  GCV <- unlist(lapply(modelResults,`[[`,"sp.criterion"))
  bestModel <- which(min(GCV,na.rm=T)==GCV)
  modelResults[[bestModel]]
  # forward/backward stepwise fit for best model to determine sig variables
  
}


