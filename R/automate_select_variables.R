#' Grab appropriate variables for a fit.
#' 
#' For each species fit full model. Due to missing data some species wont have the same type of full model
#' for example. the cold pool is only relevant or some species, stomach data is missing for others.
#' 
#' Missing data is removed from data set rather than letting mgcv do it. This reduces Errors from mgcv.
#' Variables are removed if insufficient data
#' Model Fit stats are saved to a list and saved as RDS files
#' Plots of the best full model (based on GCV) are saved to plots folder
#' 
#' 

source(here::here("R","automate_clean_data.R"))
source(here::here("R","automate_clean_model.R"))

library(magrittr)

#na.action = "na.omit" # "na.gam.replace" % mean of column
NANpropAllowed <- 0.5 # proportion of NA's in an explanatory variable before it is removed from the model
kstart <- 10
k <- kstart
latlonk <- 25
makePlots <- TRUE

if (!dir.exists(here::here("output","automate"))) {
  dir.create(here::here("output","automate"))
}

# read in main data file 
data <- readRDS(file=here::here("other","condSPP.rds")) %>% 
  dplyr::ungroup()

# select subset of variables needed in analysis
cond <- data %>% dplyr::select(AvgRelCondStrata, Species, SVSPP, LocalBiomass, LocalAbundance,
  #                             StockBiomass,
  #                             Fproxy,
                               LocalBottomTemp, LocalSurfaceTemp, PropColumnColdPool,
                               SpringTemp, SummerTemp, FallTemp, WinterTemp,
                              CopepodSmall_Large,
                              # CopepodSmLg_SprStrata, CopepodSmLg_FallStrata, CopepodSmLg_AnnualStrata,
#                               CopepodSmallLargeStrataWinter, CopepodSmallLargeStrataSpring, CopepodSmallLargeStrataSummer, CopepodSmallLargeStrataFall,
 #                              TotalCopepodStrataWinter, TotalCopepodStrataSpring, TotalCopepodStrataSummer, TotalCopepodStrataFall,
  #                             ZooplAbundStrataWinter, ZooplAbundStrataSpring,ZooplAbundStrataSummer, ZooplAbundStrataFall,
                               ZooplanktonBiomass,TotalCopepods, 
                               StomachFullness,FallBloomMagnitude,FallBloomDuration,
                               AverageLatStrata,AverageLonStrata, YEAR
#                               AssessmentYear
) 

# define variable and levels
localDensity <- c("LocalBiomass","LocalAbundance")
#populationDensity <- "StockBiomass"
#fishing <- "Fproxy"
localEnv <- c("LocalBottomTemp", "PropColumnColdPool")
broadEnv <- c("WinterTemp","SpringTemp","SummerTemp","FallTemp")
#copepod <- c("CopepodSmallLargeStrataWinter", "CopepodSmallLargeStrataSpring", "CopepodSmallLargeStrataSummer", "CopepodSmallLargeStrataFall")
copepod <- c("CopepodSmall_Large")
#resource <- c("TotalCopepodStrataWinter", "TotalCopepodStrataSpring", "TotalCopepodStrataSummer", "TotalCopepodStrataFall","ZooplAbundStrataWinter", "ZooplAbundStrataSpring","ZooplAbundStrataSummer", "ZooplAbundStrataFall","StomachFullness","FallBloomMagnitude","FallBloomDuration")
resource <- c("TotalCopepods","ZooplanktonBiomass","StomachFullness","FallBloomMagnitude","FallBloomDuration")
temporal <- "YEAR"
spatialLon <- "AverageLonStrata"
spatialLat <- "AverageLatStrata"

# model should only contain one value from each variable
df <- data.frame(variables = c("localDensity","localDensity",
  #                             "populationDensity",
   #                            "fishing",
                               "localEnv","localEnv",
                               "broadEnv","broadEnv","broadEnv","broadEnv",
                               "copepod",
                               "resource","resource","resource","resource","resource",
                               "temporal"),
                 level = c(localDensity,
        #                   populationDensity,
         #                  fishing,
                           localEnv,
                           broadEnv,
                           copepod,
                           resource,
                           temporal),
                 num = c(2,2,
            #             1,
             #            1,
                         2,2,
        #                 3,3,3,
                         4,4,4,4,
                         1,
 #                        11,11,11,11,11,11,11,11,11,11,11,
                         5,5,5,5,5,
                         1))

# create full matrix of models. Note: leave out lat and lo. they will be added to all models jointly
modelScenarios <- expand.grid(
 # populationDensity,
  #                            fishing,
                              localEnv,
                              localDensity,
                              broadEnv,
                              copepod,
                              resource,
                              temporal)

# unique list of all variables. use this to pull out from main data file
allVars <- c(unique(as.vector(as.matrix(modelScenarios))),"AverageLonStrata", "AverageLatStrata")

# create a character vector of species names
speciesList <- cond %>%
  dplyr::distinct(Species) %>% 
  dplyr::pull()


# Loop over species -------------------------------------------------------

mainList <- list()
finalModels <- list()
for (aspecies in speciesList) {  
  print(aspecies)
  # pre allocate variables
  spcriterion <- NULL
  varsUsed <- list()
  modelResults <- list()
  
  # check for incomplete data. We use all variables to help clean and interpolate data
  allSpeciesData <- cond %>% 
    dplyr::filter(Species == aspecies)
  
  # remove any variables that have < NANpropAllowed
  cleanedData <- automate_clean_data(allSpeciesData,allVars,NANpropAllowed,k)
  print(cleanedData$omittedVars)


# Loop over models -----------------------------------------------------

  for (iloop in 1:nrow(modelScenarios)) {
  #for (iloop in c(113)) {
   #   message(paste0("model # ",iloop, " for ",aspecies))
    # pull variable names for the model
    modelSpecs <- as.vector(as.matrix(modelScenarios[iloop,]))

    # remove variables identified by data_clean()
    # modelSpecsPlus contains list of variables for model fitting
    modelSpecPlus <- automate_clean_model(modelSpecs,df,cleanedData$omittedVars)

    if (is.null(modelSpecPlus$modelSpecs)) { # if null then can not fit model
      #message("skip")
      modelResults[[iloop]] <- NA
      varsUsed[[iloop]] <- NA
      next
    } else {
      modelSpecs <- modelSpecPlus$modelSpecs
      message(iloop)
    }

    # pull the variable data
    if (modelSpecPlus$latlon) { # use lat and lon
      
      speciesData <- cleanedData$data %>% 
        dplyr::select(AvgRelCondStrata, dplyr::all_of(modelSpecs), AverageLonStrata, AverageLatStrata )
      
    } else { # dont use lat and lon
      
      speciesData <- cleanedData$data %>% 
        dplyr::select(AvgRelCondStrata, dplyr::all_of(modelSpecs))

    }
    if (nrow(speciesData) < 100) { # too few data points. Should include this in clean function
      modelResults[[iloop]] <- NA
      next
    } 
    
    # fit the GAM model
    # create formula. Each explanatory variable has k = 10.
    # Add spatialLon, spatialLat jointly
    
    # get character vector of explanatory variables
    explanatoryVariableNames <- speciesData %>% 
      dplyr::select(-AvgRelCondStrata,-AverageLonStrata,-AverageLatStrata) %>%
      names()
    # list all variables used
    modelSelectedVars <- explanatoryVariableNames
    
    
    while(1) {
    
      # create the formula for the model
      mymodel <- paste("AvgRelCondStrata ", paste0("s(",explanatoryVariableNames," ,bs=\"ts\",k=",k,")" ,collapse=" + "), sep="~")
      if (modelSpecPlus$latlon) { # use lat and lon
        mymodel <- paste0(mymodel," + s(AverageLatStrata,AverageLonStrata, bs=\"ts\", k=",latlonk,")")
        modelSelectedVars <- c(modelSelectedVars,"AverageLatStrata","AverageLonStrata")
      }
  
      result <- tryCatch({
        # fit the GAM
        modelFit <- mgcv::gam(formula = as.formula(mymodel), data = speciesData) #, na.action = na.action
      }, error = function(e) {
        message(paste0(paste0("k = ",k," is too small")))
        return(NULL)
      })
      
      if (!is.null(result)) {
        k <- kstart
        break        
      } else {
        if (k < 2*kstart) {
          k <- k + 1
          next
        } else {
          k <- kstart
          result <- NULL
          break
        }
      }
    
    }
          
    #store the results
    modelResults[[iloop]] <- result
    # store sp.criterion to select best model
    if (!is.null(result)) {
      spcriterion[iloop] <- summary(result)$sp.criterion
    } else {
      spcriterion[iloop] <- NA
    }
    # list variables used in fit
    varsUsed[[iloop]] <- modelSelectedVars

  }
  
  # store info for each model for later exploration
  mainList[[aspecies]]$cleaning <- cleanedData # the cleaned version of the data
  mainList[[aspecies]]$species <- aspecies # species name

  if (is.null(spcriterion)) { # not enough data for any model
    finalModels[[aspecies]] <- NULL
    mainList[[aspecies]]$models <- NULL # the model object
    next
  } else {
    mainList[[aspecies]]$models <- modelResults
  }

  # sort models. pick best one, two, five and see how different.
  bestModel <- which(min(spcriterion,na.rm=T)==spcriterion)

  finalModels[[aspecies]]$model <- modelResults[[bestModel]]
  finalModels[[aspecies]]$summary <- summary(modelResults[[bestModel]])
  finalModels[[aspecies]]$GCV <- summary(modelResults[[bestModel]])$sp.criterion
  finalModels[[aspecies]]$s.pv <- summary(modelResults[[bestModel]])$s.pv
  finalModels[[aspecies]]$r.sq <- summary(modelResults[[bestModel]])$r.sq
  finalModels[[aspecies]]$dev.expl <- summary(modelResults[[bestModel]])$dev.expl
  finalModels[[aspecies]]$n <- summary(modelResults[[bestModel]])$n  # 
  finalModels[[aspecies]]$gamcheck <- mgcv::k.check(modelResults[[bestModel]])
  finalModels[[aspecies]]$species <- aspecies
  finalModels[[aspecies]]$variables <- varsUsed[[bestModel]]
  finalModels[[aspecies]]$formula <- modelResults[[bestModel]]$formula

  # forward/backward stepwise fit for best model to determine sig variables
  # Stepwise fitting --------------------------------------------------------
  # do this in automate_fit_best_model.r



# Plotting ----------------------------------------------------------------

  if (makePlots) {
    # plot best model and save as png
    png(filename = here::here("output","automate",paste0(gsub("\\s",aspecies,"ZooplEPU_Year_no100"),".png")),
        width = 1000,height=1000,units="px")
    plot(finalModels[[aspecies]]$model,pages=1,residuals=T, rug=T)
    dev.off()
  }  
}


saveRDS(mainList,file = here::here("output","automate","allModels_k20_ZooplEPU_no100_Year.RDS"))
saveRDS(finalModels,file = here::here("output","automate","finalModels_k20_ZooplEPU_no100_Year.RDS"))

#allModels <- readRDS(file=here::here("output","automate","allModels.RDS"))
#finalModels <- readRDS(file=here::here("output","automate","finalModels.RDS"))
