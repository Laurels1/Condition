
rm(list=ls())
setwd("L:\\EcoAp\\Condition")
ls()

#Fish condition factor
#SML
data.dir <- "L:\\EcoAp\\Condition\\"
out.dir <- "L:\\EcoAp\\Condition\\"
gis.dir  <- "L:\\EcoAp\\Survanlike"

source("survdat_functions.r")
#-------------------------------------------------------------------------------
#Required packages
library(devtools)
devtools::install_github('slucey/RSurvey/Survdat', )
library(data.table); library(graphics); library(grDevices); library(TeachingDemos);library(dplyr)
library(tidyr); library(tidyverse); library(gapminder); library(rgdal); library(Survdat)
library(RODBC)
library(gam)

#-------------------------------------------------------------------------------
#User created functions for plotting condition:
ztrans <- function(x){
    meanx <- mean(x, na.rm = T)
    sdx   <- sd(  x, na.rm = T)
    z     <- (x - meanx) / sdx
    return(z)
}

image.x <- function(x){
    output <- seq(0, 1, by = 1 / x)
    return(output)
}

my.points <- function (x, y, pt.col, ...){
    points(x, y, pch = 16, col = pt.col, cex = 1.5, ...)
}
my.lines  <- function(x, y, ...){
    lines(x, y, col = 'grey', lwd = 4, ...)
}
my.axis   <- function(x, ...){
    axis(x, cex.axis=1.5, lwd=2, ...)
}
my.box    <- function(lwd = 2, ...){
    box(lwd = lwd, ...)
}
my.legend <- function(leg.txt, ...){
    legend('topleft', legend = leg.txt, bty='n', cex=1.8, ...)
}
my.mtext  <- function(x, txt, line, ...){
    mtext(x, text = txt, line = line, cex = 2, outer = T, ...)
}

#-------------------------------------------------------------------------------
#This section is used to pull NEFSC survey bottom trawl data directly from SVDBS. It requires a current Oracle connection, permission to access the database and a user name/password. Instead, a static data set can be used (e.g. NEFSC_survey_data_...Rdata)
#Fall survey data:

user.name <- 
password <- 

#laptop:
source("C:\\Users\\laurel.smith\\Documents\\R\\Oracle_User_Data.R")
library(RODBC)
sole <- odbcConnect("sole",uid=user.name,pwd=password,believeNRows=FALSE)


# ----------------------------------------------------------------------------------------------------------- #

qry <- c(
  "select b.cruise6,b.stratum,b.tow,b.station,
  s.est_year year,season, est_month month,est_day day,
  substr(est_time,1,2)||substr(est_time,4,2) time,
  round(substr(beglat,1,2) + (substr(beglat,3,7)/60),6) beglat,
  round(((substr(beglon,1,2) + (substr(beglon,3,7)/60)) * -1), 6) beglon,
  setdepth,surftemp, bottemp,
  b.svspp,logged_species_name, sex,length,age,maturity,indid,indwt,stom_volume,stom_wgt, expcatchwt, expcatchnum
  from union_fscs_svbio b, union_fscs_svcat p, union_fscs_svsta s, svdbs_cruises c
  where
  b.svspp in ('013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197') and
  (b.cruise6=s.cruise6) and
  (c.cruise6=b.cruise6) and
  (p.cruise6=c.cruise6) and
  (p.stratum=b.stratum) and
  (b.stratum=s.stratum) and
  (p.station=b.station) and
  (b.station=s.station) and
  (p.svspp=b.svspp) and
  (p.tow=b.tow) and
  (b.tow=s.tow) ;"
)


survey <- sqlQuery(sole, qry)


save.image(file.path(out.dir, 'NEFSC_survey_data_8-15-19.RDATA'))

#End of direct SVDBS data pull. Otherwise, load data below:
#-------------------------------------------------------#

#If not pulling from SVDBS, load NEFSC survey data:
load(file.path(data.dir, 'NEFSC_survey_data_8-15-19.RData', sep = ''))

fall <- survey[survey$SEASON == 'FALL',]

#reading in condition lw paramteters for tidyverse:
LWparams <- readr::read_csv(file.path(data.dir, "lw_parameters_Condition.csv"))

#head(LWparams)
#View(LWparams)
#head(fall)

#Standardize syntax of Condition L-W data for merge with survey data:

#using tidyverse to recode sex:
LWpar <- mutate(LWparams,
                SEX = SEXMF,
                SVSPP = LW_SVSPP)

LWpar$SEX[LWpar$SEXMF=='M'] <- 1
LWpar$SEX[LWpar$SEXMF=='F'] <- 2
LWpar$SEX[is.na(LWpar$SEXMF)] <- 0

#view(LWparams)
#Use seasonal L-W parameters when available

#this works to fill EXPONENT_FALL_COMP with SEASONLESS_EXPONENT if NA, otherwise use EXPONENT_FALL (but only if brought in as a data frame, not as a data table)
ind <- is.na(LWpar$EXPONENT_FALL)
LWpar$EXPONENT_FALL_COMPL <- LWpar$EXPONENT_FALL
LWpar$EXPONENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
cbind(LWpar$EXPONENT_FALL,LWpar$EXPONENT_FALL_COMPL,LWpar$SEASONLESS_EXPONENT)

ind <- is.na(LWpar$EXPONENT_SPRING)
LWpar$EXPONENT_SPRING_COMPL <- LWpar$EXPONENT_SPRING
LWpar$EXPONENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_EXPONENT[ind]
cbind(LWpar$EXPONENT_SPRING,LWpar$EXPONENT_SPRING_COMPL,LWpar$SEASONLESS_EXPONENT)

ind <- is.na(LWpar$COEFFICIENT_FALL)
LWpar$COEFFICIENT_FALL_COMPL <- LWpar$COEFFICIENT_FALL
LWpar$COEFFICIENT_FALL_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
cbind(LWpar$COEFFICIENT_FALL,LWpar$COEFFICIENT_FALL_COMPL,LWpar$SEASONLESS_COEFFICIENT)

ind <- is.na(LWpar$COEFFICIENT_SPRING)
LWpar$COEFFICIENT_SPRING_COMPL <- LWpar$COEFFICIENT_SPRING
LWpar$COEFFICIENT_SPRING_COMPL[ind]<-LWpar$SEASONLESS_COEFFICIENT[ind]
cbind(LWpar$COEFFICIENT_SPRING,LWpar$COEFFICIENT_SPRING_COMPL,LWpar$SEASONLESS_COEFFICIENT)

#merge with tidyr:
LWparInt <- transform(LWpar, SEX = as.integer(SEX))
summary(LWparInt)
summary(fall)
mergedata <-merge(fall, LWparInt, all.fall=T, all.LWparInt = F)
#left_join gave NAs for some scup and BSB L-W params
#mergedata <- left_join(fall, LWparInt, by= c('SVSPP', 'SEX'))

#checking for missing complete L-W params
nocompl <- filter(mergedata, is.na(COEFFICIENT_FALL_COMPL))
unique(nocompl$SVSPP)
unique(nocompl$YEAR)

#filters out values without losing rows with NAs:
mergewt <- filter(mergedata, is.na(INDWT) | INDWT<75)
mergewtno0 <- filter(mergewt, is.na(INDWT) | INDWT>0)
mergelenno0 <- filter(mergewtno0, is.na(LENGTH) | LENGTH>0)
mergelen <- filter(mergelenno0, !is.na(LENGTH))
mergeindwt <- filter(mergelen, !is.na(INDWT))

#Calculate relative condition:
#need to add in case when fall is missing, use seasonless
cond <- mutate(mergeindwt, 
               predwt = (exp(COEFFICIENT_FALL_COMPL))*LENGTH**EXPONENT_FALL_COMPL,
               RelCond = INDWT/predwt*100)

#check where condition is missing
#nocond <- filter(cond, is.na(RelCond))
#unique(nocond$SVSPP)
#unique(nocond$YEAR)
#Scup (143) and black sea bass (141) have missing condition, but have seasonless coefficients and exponents.

#unique(cond$YEAR)

#This adds columns for stomach fullness without deleting rows with NA stomach data:
stom <- cond
stom$'stom_grams' <- NA
stom[!is.na(stom$STOM_WGT),'stom_grams'] <- stom[!is.na(stom$STOM_WGT),'STOM_WGT']
stom[is.na(stom$STOM_WGT),'stom_grams'] <- stom[is.na(stom$STOM_WGT),'STOM_VOLUME']

#stom.new <- stom[,c('STOM_WGT',"STOM_VOLUME",'stom_grams')]
#stom.new[!is.na(stom.new$STOM_WGT),]

stom$'stom_full' <- NA
stom$'stom_full' <- (stom$'stom_grams'/1000)/stom$'INDWT'

stom <- filter(stom, is.na(stom_full) | stom_full <1)
stom <- filter(stom, !is.na(YEAR))

#View(cond)
#View(stom)

#Parse condition data by EPU:
#Grab strata
#load(file.path(data.dir, 'Survdat.RData'))
strata <- readOGR(gis.dir, 'EPU')

setnames(stom,"BEGLAT","LAT") # change name of column header
setnames(stom,"BEGLON","LON")
stom <- stom %>% filter(!is.na(LAT)) # remove all NA's 

library(rgdal)
library(magrittr)
stom.epu <- poststrat(as.data.table(stom), strata)
setnames(stom.epu, 'newstrata', 'EPU')
#check if scup exists, doesn't because LWparams only have unsexed 
#sort(unique(stom.epu$SEX[stom.epu$SVPP==143]))

#View(stom.epu)
#stomno <- filter(stom.epu, is.na(SEX))

#summarize condition as annual average:

condstdev <- aggregate(stom.epu$RelCond, by = list('SVSPP'=stom.epu$SVSPP), sd)
names(condstdev)[ncol(condstdev)] = 'condSD'

stom.001wgt <- subset(stom.epu, stom.epu$INDWT == 0.001)
condClean <- subset(stom.epu, stom.epu$INDWT > 0.001)


condsd <- merge(condClean, condstdev, by='SVSPP', all.stom.epu=T, all.condClean = F)
stom.sd <- subset(condsd, condsd$RelCond < (100+condsd$condSD) & condsd$RelCond > (100-condsd$condSD))
#stom.sd <- subset(condsd, condsd$RelCond>=100-condsd$condSD | condsd$RelCond<=100+condsd$condSD)




stom.epu <- stom.sd %>% filter(is.na(SEX) | SEX != 0) # remove all other category for sex (when I used != c(0, 4) it didn't remove all 4s)
stom.epu <- stom.epu %>% filter(is.na(SEX) | SEX != 4)

stom.epu <- mutate(stom.epu,
                sex = SEX)

#recoding SEX (1,2) to sex (M, F)
stom.epu$sex[stom.epu$SEX==1] <- 'M'
stom.epu$sex[stom.epu$SEX==2] <- 'F'



#Tried to use LOGGED_SPECIES_NAME for species names, but doesn't exist before 2001 and too many version of names
#Names for SVSPP codes '013','015','023','026','028','032','072','073','074','075','076','077','078','102','103','104','105','106','107','108','121','131','135','141','143','145','155','164','193','197':
stom.epu <- mutate(stom.epu,
                   Species = SVSPP)

stom.epu$Species[stom.epu$SVSPP==013] <- 'Smooth Dogfish'
stom.epu$Species[stom.epu$SVSPP==015] <- 'Spiny Dogfish'
stom.epu$Species[stom.epu$SVSPP==023] <- 'Winter Skate'
stom.epu$Species[stom.epu$SVSPP==026] <- 'Little Skate'
stom.epu$Species[stom.epu$SVSPP==028] <- 'Thorny Skate'
stom.epu$Species[stom.epu$SVSPP==032] <- 'Atl Herring'
stom.epu$Species[stom.epu$SVSPP==072] <- 'Silver Hake'
stom.epu$Species[stom.epu$SVSPP==073] <- 'Atl Cod'
stom.epu$Species[stom.epu$SVSPP==074] <- 'Haddock'
stom.epu$Species[stom.epu$SVSPP==075] <- 'Pollock'
stom.epu$Species[stom.epu$SVSPP==076] <- 'White Hake'
stom.epu$Species[stom.epu$SVSPP==077] <- 'Red Hake'
stom.epu$Species[stom.epu$SVSPP==078] <- 'Spotted Hake'
stom.epu$Species[stom.epu$SVSPP==102] <- 'American Plaice'
stom.epu$Species[stom.epu$SVSPP==103] <- 'Summer Flounder'
stom.epu$Species[stom.epu$SVSPP==104] <- 'Fourspot'
stom.epu$Species[stom.epu$SVSPP==105] <- 'Yellowtail'
stom.epu$Species[stom.epu$SVSPP==106] <- 'Winter Flounder'
stom.epu$Species[stom.epu$SVSPP==107] <- 'Witch Flounder'
stom.epu$Species[stom.epu$SVSPP==108] <- 'Windowpane Flounder'
stom.epu$Species[stom.epu$SVSPP==121] <- 'Mackerel'
stom.epu$Species[stom.epu$SVSPP==131] <- 'Butterfish'
stom.epu$Species[stom.epu$SVSPP==135] <- 'Bluefish'
stom.epu$Species[stom.epu$SVSPP==141] <- 'Black Sea Bass'
stom.epu$Species[stom.epu$SVSPP==143] <- 'Scup'
stom.epu$Species[stom.epu$SVSPP==145] <- 'Weakfish'
stom.epu$Species[stom.epu$SVSPP==155] <- 'Acadian Redfish'
stom.epu$Species[stom.epu$SVSPP==164] <- 'Sea Raven'
stom.epu$Species[stom.epu$SVSPP==193] <- 'Ocean Pout'
stom.epu$Species[stom.epu$SVSPP==197] <- 'Goosefish'

#GAM analyses relating condition to environmental parameters:
form.cond <- formula(RelCond ~ s(BOTTEMP))
                     #+ s(EXPCATCHNUM) + s(stom_full))

condGAM <- gam(form.cond, family= gaussian, data=stom.epu)
stepgam <- step.gam(condGAM,scope=list(
  "BOTTEMP" = ~ 1 + BOTTEMP + lo(BOTTEMP) + S(BOTTEMP)),
  trace =T)

summary (stepgam)
gamrun1.se <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6, se=T)
gamrun1.res <-plot (stepgam, ylab="Relative Condition", rug=T, cex=1.05, lwd=6,residuals=T)


#Get count on condition by species
annualcond <- stom.epu %>% group_by(Species,EPU, sex, YEAR) %>% summarize(MeanCond = mean(RelCond), nCond = n())
condN <- filter(annualcond, nCond>=3)

#View(annualcond)
condFormat <- gather(condN, key= c('Species', 'EPU', 'sex'), value = 'MeanCond', -YEAR)


condFormat <- spread(condN, key = c(Species, EPU, sex), value = 'MeanCond', -YEAR)

write.csv(condN, "AnnualRelCond2019_audited.csv")

condSS <- filter(condFormat, EPU == SS)
write.csv(condSS, "AnnualRelCond2019_SS.csv")

condGOM <- filter(condFormat, EPU == GOM)
write.csv(condGOM, "AnnualRelCond2019_GOM.csv")

condGB <- filter(condFormat, EPU == GB)
write.csv(condGB, "AnnualRelCond2019_GB.csv")

condMAB <- filter(condFormat, EPU == MAB)
write.csv(condMAB, "AnnualRelCond2019_MAB.csv")


#select fluke annual condition data:
#condfluke <- filter(annualcond, SVSPP==103, SEX==2)
#group_by(condfluke, YEAR, SEX)
#View(condfluke)
#fluke <- condfluke %>% drop_na(YEAR)
#describe(stom)
#unique(stom$YEAR)
#View(fluke)

#write.csv(fluke, "Fluke_Cond.csv")


#This section below is for plotting annual condition:

#-------------------------------------------------------------------------------
#CF <- as.data.table(read.csv(file.path(data.dir, "NEFMC_RelativeWeight_2019.csv")))
CF <- as.data.table(read.csv(file.path(data.dir, "AnnualRelCond2019_audited.csv")))
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
jpeg(file = file.path(out.dir, 'NEFMC_Fish_RelCondition_2019.jpg'), res = 200, height = 1450,
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




