#' Process best model fits and display results in table
#' 
#' Read in all best model fits and crerate a table for each species to display results
#' Table include each covariate in model. For significant variables show trends as sparklines
#'




library(magrittr)
library(kableExtra)
finalMods <- readRDS(here::here("output/automate","finalModels.RDS"))

create_summary_table <- function(finalModels=finalMods,outDir=here::here("output","conditionMat.png")) {

fitListBySpecies <- list()
fitListBySpeciespVal <- list()

# get list of species and variables
speciesList <- unlist(lapply(finalModels,"[[","species"))
variableList <-  unique(unlist(lapply(finalModels,"[[","variables")))

# loop over each species to obtain fits for variables
for (aspecies in speciesList) {
  # return fitted values for each species/variable
  fits <- lapply(plot(finalModels[[aspecies]]$model,select=0),"[[","fit")
  # get names of fitted variables
  labels <- unlist(lapply(plot(finalModels[[aspecies]]$model,select=0),"[[","ylab"))
  labels <- stringr::str_extract(labels,"[a-zA-Z_]{2,}")
  names(fits) <- labels
  
  # get significance of variables
  pvalues <- finalModels[[aspecies]]$summary$s.pv
  names(pvalues) <- labels
  
  # store properties of fit for each species in list
  for (avar in variableList) {
    if (avar == "AverageLatStrata") next 
    if (any(names(fits)==avar)) {
      #store fitted values of variable by species. use this to plot in table
      fitListBySpecies[[avar]][[aspecies]] <- fits[[avar]]
      #store significance of variable by species. use this to color cell in table
      fitListBySpeciespVal[[avar]][[aspecies]] <- pvalues[[avar]]
    } else {
      #not valid for this species.
      fitListBySpecies[[avar]][[aspecies]] <- 0
      fitListBySpeciespVal[[avar]][[aspecies]] <- NA
    }
  }
  
}

# create table
# need exaustive list of variables (columns) and species (rows)
allVars <- names(fitListBySpecies)
inline_plot <- data.frame(species = as.vector(speciesList))
inline_plot[,allVars] <- ""
# add additional columns, n, GCV, deviance
dfn <- as.data.frame(unlist(lapply(finalModels,"[[","n")))
dfGCV <-  as.data.frame(unlist(lapply(finalModels,"[[","GCV")))
dfDev <-  as.data.frame(unlist(lapply(finalModels,"[[","dev.expl")))
dfMain <- cbind(dfn,dfGCV,dfDev)
colnames(dfMain) <- c("n","GCV","Dev")
inline_plot <- cbind(inline_plot,dfMain)
inline_plot[,1] <- NULL

# create a list of lists to hold species/variable trends
minimax <-  list(pch = ".", cex = 0, col = "grey")

# create table. uses backColor function to color cell 
# clunky and specific to given set of parameters
# Need to generalize for any parameter names
inline_plot %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = FALSE) %>%
  column_spec(2, image = spec_plot(fitListBySpecies$StockBiomass, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$StockBiomass)) %>% 
  column_spec(3, image = spec_plot(fitListBySpecies$Fproxy, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$Fproxy)) %>%
  column_spec(4, image = spec_plot(fitListBySpecies$LocalBottomTemp, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$LocalBottomTemp)) %>%
  column_spec(5, image = spec_plot(fitListBySpecies$LocalBiomass, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$LocalBiomass)) %>%
  column_spec(6, image = spec_plot(fitListBySpecies$FallTemp, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$FallTemp)) %>%
  column_spec(7, image = spec_plot(fitListBySpecies$CopepodSmall_Large, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$CopepodSmall_Large)) %>%
  column_spec(8, image = spec_plot(fitListBySpecies$FallBloomMagnitude, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$FallBloomMagnitude)) %>%
  column_spec(9, image = spec_plot(fitListBySpecies$AverageLonStrata, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$AverageLonStrata)) %>%
  column_spec(10, image = spec_plot(fitListBySpecies$LocalAbundance, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$LocalAbundance)) %>%
  column_spec(11, image = spec_plot(fitListBySpecies$FallBloomDuration, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$FallBloomDuration)) %>%
  column_spec(12, image = spec_plot(fitListBySpecies$SpringTemp, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$SpringTemp)) %>%
  column_spec(13, image = spec_plot(fitListBySpecies$TotalCopepods, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$TotalCopepods)) %>%
  column_spec(14, image = spec_plot(fitListBySpecies$WinterTemp, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$WinterTemp)) %>%
  column_spec(15, image = spec_plot(fitListBySpecies$ZooplanktonBiomass, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$ZooplanktonBiomass)) %>%
  column_spec(16, image = spec_plot(fitListBySpecies$SummerTemp, same_lim = FALSE,minmax = minimax),background = backColor(fitListBySpeciespVal$SummerTemp)) %>%
  save_kable(.,file = outDir)
  
}



#' colors cells based on value
backColor <- function(data){
  colorsV <- list()  
  for (afield in names(data)) {
    if (is.na(data[[afield]])) {
      col <- ""
    } else if (data[[afield]] <= 0.05) {
      col <- "green"
    } else {
      col <- "red"
    }
    colorsV[[afield]]<- col
  }  
  colorsV <- unlist(colorsV)
  return(colorsV)
}