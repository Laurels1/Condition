#' Grab appropriate variables for a fit.
#' 
#' For each species fit full model. Due to missing data some species wont have the same type of full model
#' for example. the cold pool is only relevant or some species, stomach data is missing for others

library(magrittr)
# variables and levels
bottomTemp <- "Bottom Temp"
populationDensity <- "stockBiomass"
fishing <- "Fproxy"
localEnv <- c("bottomTemp","coldPool")
localDensity <- c("Biomass","Abundance")
broadEnv <- c("WinterTemp","SpringTemp","SummerTemp","FallTemp")
resource <- c("zooBiomass","TotalCopepod","stomachFullness","FallBloomMagnitude","FallBloomDuration")
spatial <- c("Latlon")

# create full matrix of models
modelScenarios <- expand.grid(bottomTemp=bottomTemp,
            populationDensity=populationDensity,
            fishing=fishing,
            localEnv=localEnv,
            localDensity=localDensity,
            broadEnv=broadEnv,
            resource=resource,
            spatial=spatial)


# you could loop over all of the species
# speciesList would be a character vector of all species
for (aspecies in speciesList) {
  # now loop through each of the models
  for (iloop in 1:nrow(modelScenarios)) {
    # assuming you have your data formatted in a particular way, which we could do
    modelSpecs <- modelScenarios[iloop,]
    
    speciesData <- bigDataFrame %>% dplyr::filter(Species == aspecies) %>% 
      dplyr::filter(variable == modelSpecs$bottomTemp) %>%
      dplyr::filter(variable == modelSpecs$populationDensity) %>%
      dplyr::filter(variable == modelSpecs$fishing) %>%
      dplyr::filter(variable == modelSpecs$localEnv) %>%
      dplyr::filter(variable == modelSpecs$localDensity) %>%
      dplyr::filter(variable == modelSpecs$broadEnv) %>%
      dplyr::filter(variable == modelSpecs$resource) %>%
      dplyr::filter(variable == modelSpecs$spatial)
      
# This would require that all of the data is in one big data frame (in this example called "bigDataFrame") 
# Either add ALL variables for ALL species and have NA for missing years or ..
#
#
# The data will need to have fields. 
# Species - species name
# variable - name of variable. This must match the names in the modelScenarios df above. 
# Of course these names can be whatever you want. They just need to match
# year - year of data point
# value - value of variable

# for example:
#
# species   year   variable    value
# dogfish   1971    Fproxy       .2
# dogfish   1972    Fproxy       .4
# dogfish   1973    Fproxy       .1
# ...
# cod       1971    summerTemp   22
# cod       1972    summerTemp   25
# cod       1973    summerTemp   23

    # now run model with data set and store output
    
    
  }
  
  # sort models. pick best one, two, five and see how different.
  
  # fowrard/backward stepwise fit for best model to determine sig variables
  
}


