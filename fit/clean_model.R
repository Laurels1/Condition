#' reduce model variables 
#' 
#' Based on cleaning of data some variables may have been omitted for a given species.
#' Reduce the model to reflect this
#'


clean_model <- function(modelSpecs,df,omittedVars) {
  
  # check to see if lat and long should be added to model
  latlon <-  T
  if (any(c("AverageLonStrata","AverageLatStrata") %in% omittedVars)) {
    latlon <- F
  }
 
  # now find which other variables were omitted
  vars <- df %>% dplyr::filter(level %in% omittedVars)
  
  if (nrow(vars) != 0) { # cleaning. there are variables to remove
    
    # if num = 1 then remove variable from analysis and continue with fit
    # if any num > 1 then skip skip fit
    
    if (any(vars$num == 1)) {
      removeVar <- df %>% 
        dplyr::filter(num ==1) %>% 
        dplyr::select(level) %>% 
        dplyr::pull()
      
      modelSpecs <- modelSpecs[!(modelSpecs %in% removeVar)]
    }
    
    # variables with a multiple levels.
    # check to see if they are in model in the first place
    if (any(vars$num > 1)) {
      if (any(modelSpecs %in% omittedVars)) {
        modelSpecs <- NULL
      } else {
        # do nothing. nothing to remove
      }
    }
    
    
  }
  
  return(list(modelSpecs=modelSpecs,latlon=latlon))

  
}

