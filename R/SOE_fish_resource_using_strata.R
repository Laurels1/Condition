#SML
#Required packages----
library(data.table); library(survdat); library(here); library(lwgeom)

#Pull Data----
channel <- dbutils::connect_to_database(server="sole",uid="slucey")

#getBio for individual weights (use survdat for raw data in Condition analyses and SOE plots):
survey <- survdat::get_survdat_data(channel, getBio = T)
                                    #getLengths = F)
survdat <- survey$survdat

#****end here for SOE condition data

#Aggregate species----
#Grab species list
#I need to get SOE_species_list.RData
# load(here::here('data_raw/SOE_species_list.RData'))
# 
# #Merge to get species group and fed managed status
# survdat <- merge(survdat, unique(species[, list(SVSPP, SOE.20, Fed.Managed)]), 
#                  by = 'SVSPP', all.x = T)
# #Combine agg group and manage status
# survdat[, SOE.Managed := paste(SOE.20, Fed.Managed)]

#Change seasons from all caps
# survdat[SEASON == 'FALL',   SEASON := 'Fall']
# survdat[SEASON == 'SPRING', SEASON := 'Spring']

#Calculate stratified means
#Strata sets
EPU <- c('MAB', 'GB', 'GOM', 'SS')
MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

survey.data <- c()

for(iepu in 1:length(EPU)){
  epu.strata <- get(EPU[iepu])
  #Separate inshore/offshore
  epu.inshore  <- epu.strata[which(epu.strata >= 2000)]
  epu.offshore <- epu.strata[which(epu.strata <  2000)]
  
  #Calculate stratified means
  all <- survdat::calc_stratified_mean(survdat, filterByArea = epu.strata,
                                       filterBySeason = c('FALL', 'SPRING'),
                                       groupDescription = 'SVSPP', tidy = T)
                                       #For aggregate species:
  #                                     groupDescription = 'SOE.20', tidy = T)
  # all.fmc <- survdat::calc_stratified_mean(survdat, filterByArea = epu.strata, 
  #                                          filterBySeason = c('Fall', 'Spring'),
  #                                          groupDescription = 'SVSPP', tidy = T)
  # inshore.all <- survdat::calc_stratified_mean(survdat, filterByArea = epu.inshore,
  #                                              filterBySeason = c('Fall', 'Spring'),
  #                                              groupDescription = 'SVSPP',
  #                                              tidy = T)
  # offshore.all <- survdat::calc_stratified_mean(survdat, filterByArea = epu.offshore,
  #                                               filterBySeason = c('Fall', 'Spring'),
  #                                               groupDescription = 'SVSPP',
  #                                               tidy = T)
  # inshore.fmc <- survdat::calc_stratified_mean(survdat, filterByArea = epu.inshore,
  #                                              filterBySeason = c('Fall', 'Spring'),
  #                                              groupDescription = 'SVSPP',
  #                                              tidy = T)
  # offshore.fmc <- survdat::calc_stratified_mean(survdat, filterByArea = epu.offshore,
  #                                               filterBySeason = c('Fall', 'Spring'),
  #                                               groupDescription = 'SVSPP',
  #                                               tidy = T)
  
  #Correct variable names before combining into one dataset
  #stratified biomass
  all[variable == 'strat.biomass', Var := paste(SVSPP, SEASON, 'Biomass Index')]
  # all.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
  #                                                   SEASON, 'Biomass Index')]
  # inshore.all[variable == 'strat.biomass', Var := paste(SOE.20, SEASON, 
  #                                                       'Biomass Index - inshore')]
  # inshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
  #                                                       SEASON, 
  #                                                       'Biomass Index - inshore')]
  # offshore.all[variable == 'strat.biomass', Var := paste(SOE.20, SEASON, 
  #                                                        'Biomass Index - offshore')]
  # offshore.fmc[variable == 'strat.biomass', Var := paste(SOE.Managed, 'managed species -',
  #                                                        SEASON, 
  #                                                        'Biomass Index - offshore')]
  # 
  #Standard Error
  all[variable == 'biomass.SE', Var := paste(SVSPP, SEASON, 
                                                     'Biomass Standard Error')]
  # all.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
  #                                                    SEASON, 
  #                                                    'Biomass Standard Error')]
  # inshore.all[variable == 'biomass.SE', Var := paste(SOE.20, SEASON, 
  #                                                    'Biomass Standard Error - inshore')]
  # inshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
  #                                                    SEASON, 
  #                                                    'Biomass Standard Error - inshore')]
  # offshore.all[variable == 'biomass.SE', Var := paste(SOE.20, SEASON, 
  #                                                     'Biomass Standard Error - offshore')]
  # offshore.fmc[variable == 'biomass.SE', Var := paste(SOE.Managed, 'managed species -',
  #                                                     SEASON, 
  #                                                     'Biomass Standard Error - offshore')]
  # 
  #Combine into one dataset
  epu.all <- 
    # rbindlist(list(all, all.fmc, inshore.all, inshore.fmc, offshore.all, 
    #                         offshore.fmc), use.names = F)
all[, Region := EPU[iepu]]
  
  #Combine with other EPUs
  survey.data <- rbindlist(list(survey.data, epu.all))
}

#Remove unnecessary columns/rows
# survey.data[, c('SOE.20', 'SEASON', 'variable') := NULL]
# survey.data <- survey.data[!is.na(Var), ]

#Rename columns and add new columns
setnames(survey.data, c('YEAR', 'value', 'units'), c('Time', 'Value', 'Units'))
survey.data[, Source := 'NEFSC bottom trawl survey (survdat)']

#Set column order
setcolorder(survey.data, c('Time', 'Value', 'Var', 'Units', 'Region', 'Source'))

save(survey.data, file = here::here('data/Survdat.RData'))
str(survey.data)
