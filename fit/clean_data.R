#' function to clean the data.
#' 
#' Remove explanatory variables that have too many NAs. 
#' Remove rows that have NAs in any column since model can't deal with missing values. 
#' or interpolate. The number of distinct values for each variable is also compared to k to ensure
#' there are enough degrees of freedom for the smoothers
#' 
#' @param rawData Data frame. 
#' @param vars Character vector. All explanatory variable names. these wil be the column names in \{rawData}
#' @param NAallow Numeric scalar. The proportion of allowed NAs in each column. Otherwise variable is removed
#' @param k Numeric scalar. mgcv::gam argument representing smoother
#' 

clean_data <- function(rawData,vars,NAallow,k) {
  
  # select explanatory variables
  explanatoryVars <- rawData %>% 
    dplyr::select(AvgRelCondStrata,all_of(vars))
  
  # find the proportion of NAs in each column
  propOfNAs <- colSums(is.na(explanatoryVars))/nrow(explanatoryVars)
  # determine if this is an acceptable amount
  indexofNAs <- propOfNAs < NAallow

  # extract variables that will be used in analysis
  cleanData <- explanatoryVars[indexofNAs]
  
  # find variables that were omitted
  omittedVars <- names(explanatoryVars)[!indexofNAs]
  
  # remove rows that contain NAs.
  # mgcv::gam will do this automatically but we need some checks before hand
  # We need to calculate the number of distinct values of each var to make
  # sure there are enough degrees of freedom available for the smooth
  propRowsRemoved <- sum(is.na(rowSums(cleanData)))/nrow(cleanData)
  # clean data after NA rows removed
  cleanData <- cleanData[!is.na(rowSums(cleanData)),]
  
  # now check for number of distinct values in each of the variables.
  dfdistinct <- data.frame(nDistinct=rep(NA,length(names(cleanData))),row.names = names(cleanData))
  for (aname in names(cleanData)) {
   dfdistinct[aname,1] <-  length(unique(cleanData[[aname]]))
  }
  # remove any variables which have number of distinct values < k
  removeVarsk <- rownames(dfdistinct)[dfdistinct < k]
  indexk <- names(cleanData) %in% removeVarsk
  cleanData <- cleanData[,!indexk]
  
  # add names to omittedVars list
  omittedVars <- c(omittedVars,removeVarsk)
  
  return(list(data=cleanData,omittedVars=omittedVars,propRowsRemoved=propRowsRemoved))
  
  
  
}

