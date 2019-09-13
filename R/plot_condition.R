#' Plots annual condition
#' 
#' Describe further
#' 
#' @param annualCondition tibble. (nx6). Columns = Species,EPU,sex,YEAR,MeanCond, nCond
#' @param out.dir character string. Name of directory where plots will be saved
#' 
#' @return Plots
#' 
#' @export


plot_condition <- function(annualCondition,out.dir = "output") {
  
  speciesNames <- unique(annualCondition$Species)
  # quantiles to group into
  quantilesNormal <- qnorm(c(0.25,0.5,0.75)) 
  # values Q1 <= 0.25
  # values 0.25 > Q2 <= 0.5
  # values 0.5 > Q3 <= 0.75
  # values 0.75 > Q4
  yearRange <- c(min(annualCondition$YEAR):max(annualCondition$YEAR))
  # create an expanded grid to fill in missing years with NA's
  fullTable <- dplyr::as_tibble(expand.grid(Species=speciesNames,YEAR= yearRange,sex = c("M","F"),stringsAsFactors = F))
  annualCondition <- dplyr::right_join(annualCondition,fullTable,by=c("Species","YEAR","sex"))
  
  #This section below is for plotting annual condition:
  conditionMatrix <- yearRange # inital value
  rowNames <- "Year" # initial value 
  for (species in speciesNames) { # for each plot each species is a row
    for (asex in c("M","F")) {
      # pull each species/Sex one at a time. 
      speciesData <- annualCondition %>% dplyr::filter(Species == species & sex == asex) %>% dplyr::arrange(YEAR)
      # normalize data first
      normData <- (speciesData$MeanCond - mean(speciesData$MeanCond,na.rm = T))/sd(speciesData$MeanCond,na.rm=T)
      # each year is placed in category 1-4 based on quantiles above
      quant <- 1
      for (iq in quantilesNormal) {
        quant <- quant + (normData > iq)
      }
      conditionMatrix <- rbind(conditionMatrix,quant)
      rowNames <- rbind(rowNames,paste0(species," - ",asex))
    }
  }
  conditionMatrix <- as.data.frame(conditionMatrix)
  rownames(conditionMatrix) <- as.vector(rowNames)
  
  #lattice::levelplot(conditionMatrix)
  graph.colors<-colorRampPalette(c('#C6E2FF','#00008B'))
  image(t(conditionMatrix[2:59,]),xaxt="n",yaxt="n",col=graph.colors(4))
  
  axis(1,at=seq(0,1,length.out=dim(conditionMatrix)[2]),labels=conditionMatrix[1,],las=2)
  axis(4,at=seq(0,1,length.out=dim(conditionMatrix)[1]-1),labels=as.vector(tail(rowNames,-1)),las=1)
  
  
  return(conditionMatrix)

}
  
    
