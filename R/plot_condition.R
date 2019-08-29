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
  
  #This section below is for plotting annual condition:
  
  #-------------------------------------------------------------------------------
#CF <- as.data.table(read.csv(file.path(data.dir, "NEFMC_RelativeWeight_2019.csv")))
#CF <- as.data.table(read.csv(file.path(data.dir, "AnnualRelCond2019_audited.csv")))
CF <- as.data.table(annualCondition)
#formatting annual condition for plotting

#CF <- CF[c(1:34),]
#Convert to long format
CF.fixed <- CF[, c(1:4), with = F]
CF.fixed[, YEAR := 1992]
setnames(CF.fixed, "X1992", "CF")

for(i in 5:ncol(CF)){
  CF.year <- CF[, c(1:3, i), with = F]
  CF.year[, YEAR := 1992 + (i - 4)]
  setnames(CF.year, paste("X", 1992 + (i-4), sep = ''), "CF")
  CF.fixed <- rbind(CF.fixed, CF.year)
}

setkey(CF.fixed,
       species,
       stock,
       sex)

CF.fixed[, z := ztrans(CF), by = key(CF.fixed)]

#Rearrangements for the plot
CF.data <- matrix(CF.fixed[, z], nrow(unique(CF.fixed)), length(unique(CF.fixed[, YEAR])), byrow = T)

setkey(CF.fixed,
       species,
       stock,
       sex)

#labels for the plot
CF.fixed[sex == 'male', sex.label := ', M']
CF.fixed[sex == 'female', sex.label := ', F']
CF.fixed[sex == 'combined', sex.label := '']
CF.fixed[stock == 'unit', stock := '']

setkey(CF.fixed,
       species,
       stock,
       sex)

graph.colors<-colorRampPalette(c('#C6E2FF','#00008B'))

opar <- par()
jpeg(file = here::here(out.dir, "NEFMC_Fish_RelCondition_2019.jpg"), res = 200, height = 1450,
     width = 2000)
#Main figure
par(mar = c(2, 2, 3, 18), fig = c(0, 1, 0.1, 1))

#image rotates matrix 90 degrees so use transverse matrix and order variables in reverse order in .csv file
image(y = image.x(nrow(unique(CF.fixed))),
      x = image.x(length(unique(CF.fixed[, YEAR]))), z = t(CF.data),
      breaks = c(-10, -1, 0, 1, 10), col = graph.colors(4),
      xlim = c(0,1), ylim = c(0,1), axes = F, xlab = '', ylab = '', useRaster = T)

my.axis(3, at = image.x(length(unique(CF.fixed[, YEAR])))[seq(5, 25, 5)] -
          0.5 * (image.x(length(unique(CF.fixed[, YEAR])))[2]),
        labels = seq(1995, 2015, 5))

axis(4, at = image.x(nrow(unique(CF.fixed)))[-1] -
       0.5 * (image.x(nrow(unique(CF.fixed)))[2]),
     label = paste(unique(CF.fixed)[, species], ' ', unique(CF.fixed)[, stock],
                   unique(CF.fixed)[, sex.label], sep = ''), las = T, lwd = 0)

my.box()

#Key
par(mar = c(0.7, 0.5, 0.2, 0.5), fig = c(0.2, 0.5, 0, 0.1), new = T)
image(x = image.x(4), z = t(matrix(1:4,1,4)), col = graph.colors(4), axes = F,
      xlim = c(0,1), ylim = c(0,1))
axis(3, at = image.x(4), labels = c(-3, -1, 0, 1, 3), cex = 2)
my.box()

dev.off()

par(opar)


}
