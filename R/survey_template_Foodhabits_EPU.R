#New Survey Template for use with Survdat package
#SML

#User parameters
#if(Sys.info()['sysname']=="Windows"){
    r.dir    <- "C:\\Users\\laurel.smith\\Documents\\EDAB\\Condition\\Condition"
    data.dir <- "C:\\Users\\laurel.smith\\Documents\\EDAB\\Condition\\Condition"
#    gis.dir  <- "L:\\EcoAp\\SurvanLike"
    out.dir  <- "C:\\Users\\laurel.smith\\Documents\\EDAB\\Condition\\Condition"
#    memory.limit(4000)
#}
#if(Sys.info()['sysname']=="Linux"){
#    r.dir    <- "/home/slucey/slucey/Rworkspace/RSurvey"
#    data.dir <- "/home/slucey/slucey/EcoAP/Data/survey"
#    gis.dir  <- "/home/slucey/slucey/Rworkspace/GIS_files"
#    out.dir  <- "/home/slucey/slucey/EcoAP/Data/survey"
#}

#-------------------------------------------------------------------------------
#Required packages
#May need to download Survdat package from GitHub
#devtools::install_github('slucey/RSurvey/Survdat')
library(data.table); library(rgdal); 
    library(Survdat)

#-------------------------------------------------------------------------------
#View poststrat function to determine variables required
  poststrat
    
#Grab survdat.r
load(file.path(data.dir, 'allfh.RData'))
    
    allfh$LAT <- allfh$declat 
    allfh$LON <- allfh$declon *-1
    allfh$CRUISE6 <- allfh$cruise6
    allfh$STRATUM <- allfh$stratum
    allfh$STATION <- allfh$station
    
#    head(allfh)

x <- as.data.table(allfh)

#Find missing values
which(is.na(x$LAT))
which(is.na(x$LON))
which(is.na(x$CRUISE6))
which(is.na(x$STRATUM))
which(is.na(x$STATION))

#Remove rows with missing LAT or LON
x <- x[!is.na(x$LAT),]
x <- x[!is.na(x$LON),]

#head(x)

#Grab strata
EPUstrata <- readOGR(gis.dir, 'EPU')

#Post stratify data if necessary
fh.epu <- poststrat(x, EPUstrata)
setnames(fh.epu, 'newstrata', 'EPU')

#head(fh.epu)

#Output results either to a flat .csv file or .RData set
write.csv(fh.epu, file = file.path(out.dir, 'FoodHabits_EPU.csv'), row.names = F)
save(     fh.epu, file = file.path(out.dir, 'FoodHabits_EPU.RData'))
