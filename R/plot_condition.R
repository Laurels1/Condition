#' Plots annual condition
#' 
#' Plots normalized condition factor (categorized into 4 quantiles)
#' Quantiles used are 25%, 50%, 75% (This can be generalized at a later date)
#' 
#' @param annualCondition Tibble. (nx6). Columns = Species,EPU,sex,YEAR,MeanCond, nCond
#' @param filename Character string. Name of output file (excluding file extendion)
#' @param out.dir Character string. Name of directory where plots will be saved
#' 
#' @return data frame and exports a plot
#' \item{conditionMatrix}{Values designates the quantile for each Species (row), in a given year (column)}
#' 
#' @section Plot:
#' 
#' The file format of the exported plot is jpg (size etc can be generalized at a later date)
#' 
#' @export


plot_condition <- function(annualCondition,filename= "test1",out.dir = "output") {
    
  speciesNames <- annualCondition %>% dplyr::filter(sex == "F") %>%
    dplyr::select(Species) %>%
    dplyr::distinct(Species)
  speciesNames <- speciesNames$Species
  

  print(speciesNames)
  
  #speciesNames <- sort(unique(annualCondition$Species))
    
  #List species alphabetically, doesn't work:
  #speciesNames <- annualCondition$Species %>% dplyr::distinct(Species) %>% dplyr::arrange(desc(Species))  
  # quantiles to group into
#    quantilesNormal <- qnorm(c(0.25,0.5,0.75)) 
  #5 groupings:
     quantilesNormal <- qnorm(c(0.2,0.4,0.6,0.8)) 
     nQuants <- length(quantilesNormal)+1
  # values Q1 <= 0.25
  # values 0.25 > Q2 <= 0.5
  # values 0.5 > Q3 <= 0.75
  # values 0.75 > Q4
  yearRange <- c(min(annualCondition$YEAR):max(annualCondition$YEAR))
  # create an expanded grid to fill in missing years with NA's
  fullTable <- dplyr::as_tibble(expand.grid(Species=speciesNames,YEAR= yearRange,sex = c("F"),stringsAsFactors = F))
  annualCondition <- dplyr::right_join(annualCondition,fullTable,by=c("Species","YEAR","sex"))
  
  #This section below is for plotting annual condition:
  conditionMatrix <- NULL# inital value
  rowNames <- NULL # initial value 
  for (species in speciesNames) { # each species is a row in the plot
    for (asex in c("F")) {
      # pull each species/Sex one at a time. 
      
      speciesData <- annualCondition %>% dplyr::filter(Species == species & sex == "F") %>% dplyr::arrange(YEAR)
      if (dim(speciesData)[1] == 0) {next} # no data
      # normalize data first
      normData <- (speciesData$MeanCond - mean(speciesData$MeanCond,na.rm = T))/sd(speciesData$MeanCond,na.rm=T)
      # each year is placed in category 1-4 based on quantiles above
      quant <- 1
      for (iq in quantilesNormal) {
        quant <- quant + (normData > iq)
      }
      conditionMatrix <- rbind(conditionMatrix,quant)
      rowNames <- rbind(rowNames,paste0(species))
    }
  }
    # convert to dataframe and add row and column names
   conditionMatrix <- as.data.frame(conditionMatrix)
   rownames(conditionMatrix) <- as.vector(rowNames)
   colnames(conditionMatrix) <- as.vector(yearRange)

   # plot the matrix with specified color palette
#   jpeg(filename = here::here(out.dir,paste0(filename,".jpg")), res = 200, height = 2000, width = 1450)
   jpeg(filename = here::here(out.dir,paste0(filename,".jpg")), res = 200, height = 1350, width = 1450)
#  graph.colors<-colorRampPalette(c('#C6E2FF','#00008B')) #blue color palette
   #graph.colors<-colorRampPalette(c('steelblue2','gray77','tomato2')) #blue color palette
   viridisColors <- viridis::viridis_pal()(nQuants)
   graph.colors<-colorRampPalette(viridisColors) #blue color palette
#   graph.colors <- colorspace::diverge_hcl #Red lowest, blue highest
#  graph.colors <- RColorBrewer::brewer.pal(5, "RdBu") #Divergent ramp red to blue
#  par(mar = c(4, 2, 2, 11), fig = c(0, 1, 0.1, 1)) #fig indicates how much of plot to take up, with c(0,1,0,1) being the whole space
   par(mar = c(1.4, 1.5, 1.5, 8), fig = c(0, 1, 0.15, 1))
      image(t(conditionMatrix),xaxt="n",yaxt="n",col=graph.colors(nQuants))
  # xaxis
   axis(1,at=seq(0,1,length.out=length(yearRange)),labels=yearRange,cex.axis = 0.8,las=2)
   # yaxis (need to arrange in alphabetical)
   axis(4,at=seq(0,1,length.out=length(rowNames)),labels=rowNames,cex.axis=0.8,las=1)
  
   # legend
#   par(mar = c(4.5, 8, 0.0, 14), fig = c(0, 1, 0, 0.1), new = T)
   par(mar = c(3, 8, 0.0, 14), fig = c(0, 1, 0, 0.1), new = T) # first number of mar is height of legend
#   image(x=seq(0, 1, by = 1 / 4), z = t(matrix(1:4,1,4)), axes = F, col = graph.colors(4), xlim = c(0,1), ylim = c(0,1),xlab="")
#   axis(1, at = seq(1/8, 1, by = 1 / 4), labels = c( 1, 2, 3, 4), cex = 2)
#   axis(2,at = 0.5, labels="Quantile",las=2)
  #legend for 5 groupings:
   image(x=seq(0, 1, by = 1 / nQuants), z = t(matrix(1:nQuants,1,nQuants)), axes = F, col = graph.colors(nQuants), xlim = c(0,1), ylim = c(0,1),xlab="")
   axis(1, at = seq(1/8, 1, by = 1 / nQuants), labels = c( "Poor Condition", "Bad", "Neutral", "Good", "Good Condition"), cex = 2)
   #axis(2,at = 0.5,las=2)
    box()
   dev.off()
   
   return(conditionMatrix)

}
  
    
