library(ggplot2)
library(dplyr)
library(tidyr)

out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Summarize annually by EPU (use for SOE plots)
# annualcondEPU <- cond.epu %>% dplyr::group_by(Species,EPU, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
# condN <- dplyr::filter(annualcondEPU, nCond>=3) %>% ungroup()
# condNSppEPU <- condN %>% dplyr::add_count(Species, EPU) %>% 
#   dplyr::filter(n >= 20)

# #Summarize annually over all EPUs for butterfish WG:
annualcond <- cond.epu %>% dplyr::group_by(Species,YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
condNSpp <- condN %>% dplyr::add_count(Species) %>%
    dplyr::filter(n >= 20)
# 
# #Mean butterfish condition for line plot (SingleSpecies_ConditionPlot.R):
 ButtCondPlot <- condNSpp %>% dplyr::filter(Species == 'Butterfish') %>% dplyr::select(MeanCond, YEAR)
# 
# #Test for regime shifts in butterfish (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
ButtCond <- cond.epu %>% dplyr::filter(Species == 'Butterfish') %>% dplyr::select(RelCond, YEAR)
ButtRegime <- rpart::rpart(RelCond~YEAR, data=ButtCond)
ButtPlot <- rpart.plot::rpart.plot(ButtRegime)
#Outputs pruning tree table:
printcp(ButtRegime)

# #Pull regime shift years into new data frame to add to plot (use the simplest tree 
# #within one standard error (xstd) of the best tree (lowest xerror)):
ButtResults <- as.data.frame(ButtRegime[["splits"]])
ButtSplit1 <- ButtResults$index[1]
ButtSplit2 <- ButtResults$index[2]
# 
# #Summarize annually BY SEX over all EPUs for butterfish WG:
# annualcond <- cond.epu %>% dplyr::group_by(Species,YEAR, SEX) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
# condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
# condNSppSex <- condN %>% dplyr::add_count(Species) %>% 
#     dplyr::filter(n >= 20)
# 
# #Mean FEMALE butterfish condition for line plot (SingleSpecies_ConditionPlot.R):
# FemButtCondPlot <- condNSppSex %>% dplyr::filter(Species == 'Butterfish', SEX == 2) %>% dplyr::select(MeanCond, YEAR)
# 
# #Test for regime shifts in FEMALE butterfish (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# ButtCond <- cond.epu %>% dplyr::filter(Species == 'Butterfish', SEX == 2) %>% dplyr::select(RelCond, YEAR)
# ButtRegime <- rpart::rpart(RelCond~YEAR, data=ButtCond)
# FemButtPlot <- rpart.plot::rpart.plot(ButtRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# ButtResults <- as.data.frame(ButtRegime[["splits"]])
# FemButtSplit1 <- ButtResults$index[1]
# FemButtSplit2 <- ButtResults$index[2]
# 
# #Mean MALE butterfish condition for line plot (SingleSpecies_ConditionPlot.R):
# MaleButtCondPlot <- condNSppSex %>% dplyr::filter(Species == 'Butterfish', SEX == 1) %>% dplyr::select(MeanCond, YEAR)
# 
# #Test for regime shifts in MALE butterfish (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# ButtCond <- cond.epu %>% dplyr::filter(Species == 'Butterfish', SEX == 1) %>% dplyr::select(RelCond, YEAR)
# ButtRegime <- rpart::rpart(RelCond~YEAR, data=ButtCond)
# MaleButtPlot <- rpart.plot::rpart.plot(ButtRegime)
# 
# #Male Butterfish: Pull regime shift years into new data frame to add to plot:
# ButtResults <- as.data.frame(ButtRegime[["splits"]])
# MaleButtSplit1 <- ButtResults$index[1]
# MaleButtSplit2 <- ButtResults$index[2]

#Regime shift analysis for all species together over all EPUs:
# AllSppCond <- condNSpp %>% dplyr::select(MeanCond, YEAR)
# AllSppRegime <- rpart::rpart(MeanCond~YEAR, data=AllSppCond)
# AllSppPlot <- rpart.plot::rpart.plot(AllSppRegime)
# 
# #Pull regime shift years into new data frame to add to plot:
# AllSppResults <- as.data.frame(AllSppRegime[["splits"]])
# AllSppSplit1 <- AllSppResults$index[1]
# AllSppSplit2 <- AllSppResults$index[2]

#Single species regime shift plots:
#Summarize annually over all EPUs for mackerel:
#USE SPRING SURVEY
#Do sensitivity tests for maturity cut-offs between 23-29cm and then run for mature and immature fish separately:
# annualcond <- cond.epu  %>% dplyr::filter(Species == 'Atlantic mackerel', LENGTH > 23, YEAR >= 1992)  %>%
#   dplyr::group_by(YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), StdDevCond = sd(RelCond), nCond = dplyr::n())
# condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
# 
# readr::write_csv(condN, here::here(out.dir,"RelCondition_mackerel_Mature23_Fall2022.csv"))

# condNSpp <- condN %>%
#   dplyr::add_count(Species) %>%
#   dplyr::filter(n >= 20)

#Mean mackerel condition for line plot (SingleSpecies_ConditionPlot.R):
#MackCondPlot <- condN %>% 
  # dplyr::filter(Species == 'Atlantic mackerel') %>%
#  dplyr::select(MeanCond, YEAR)

#Test for regime shifts in mackerel (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
#Do sensitivity tests for maturity cut-offs between 23-29cm:
# MackCond <- cond.epu %>%  dplyr::filter(Species == 'Atlantic mackerel', LENGTH >23) %>% 
#   dplyr::select(RelCond, YEAR)
# MackRegime <- rpart::rpart(RelCond~YEAR, data=MackCond)
# MackPlot <- rpart.plot::rpart.plot(MackRegime)
#Outputs pruning tree table:
#printcp(MackRegime)

#Pull regime shift years into new data frame to add to plot (use the simplest tree
#within one standard error (xstd) of the best tree (lowest xerror)):
# MackResults <- as.data.frame(MackRegime[["splits"]])
# MackSplit1 <- MackResults$index[1]
# MackSplit2 <- MackResults$index[2]
# MackSplit3 <- MackResults$index[3]


#Removed MAB values in 2017 due to low sampling coverage:
annualCondition <- ButtCondPlot
#%>%
#   dplyr::filter(!(EPU == "MAB" & YEAR == 2017))
#   dplyr::filter(!(YEAR == 2017))

#annualCondition <- MackCondPlot

#change YEAR to continuous numeric for plotting function below:
annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))

speciesNames <- annualCondition
#    dplyr::filter(sexMF == "F") %>%


#See 5 scale colors for viridis:
#scales::show_col(viridis::viridis_pal()(5))
#vir <- viridis::viridis_pal()(5)

#Line plot of condition
p2 <- ggplot(speciesNames, aes(x = YEAR, y = MeanCond)) +
  geom_line()+
  geom_point() +
  labs(title="Butterfish Fall Relative Condition", y = "Relative Condition") +
#  labs(title="Mature Atlantic Mackerel Fall Relative Condition (>23cm)", y = "Relative Condition") +
  # geom_vline(xintercept=MackSplit1, color='red')+
  # geom_vline(xintercept=MackSplit2, color='red')
# +
#     geom_vline(xintercept=MackSplit3, color='red')
   geom_vline(xintercept=ButtSplit1, color='red')+
   geom_vline(xintercept=ButtSplit2, color='red')

ggsave(path= here::here(out.dir),"Butterfish_Fall_ShelfCondition_allsex_2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)



####Automating regime shift plots by species:

# create a character vector of species names
speciesList <- cond.epu %>%
  dplyr::distinct(Species) %>% 
  dplyr::pull()

#Only select Species with names that have sufficient data:
numSpecies <- length(speciesList)
for (i in numSpecies:1) {
  if (!is.na(as.numeric(speciesList[i]))) {
    speciesList <-speciesList[-i]
  }
}

#Summarize annually over all EPUs:
#Filters out numeric Species:
Sppcond <- cond.epu %>% dplyr::filter(!str_detect(Species,"^\\s*[0-9]*\\s*$")) 
SDcond <- Sppcond %>%
  dplyr::group_by(Species, YEAR) %>% 
   dplyr::summarize(MeanCond = mean(RelCond), 
                    sd.cond = sd(RelCond, na.rm = TRUE),
                  nCond = dplyr::n() )
annualcond <- SDcond %>%
  mutate(se.cond = sd.cond / sqrt(nCond),
         lower.ci.cond = MeanCond - qt(1 - (0.05 / 2), nCond - 1) * se.cond,
         upper.ci.cond = MeanCond + qt(1 - (0.05 / 2), nCond - 1) * se.cond)
#%>%
#  mutate(upper.ci = MeanCond+ci, lower.ci = MeanCond-ci)
condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
condNSpp <- condN %>% dplyr::add_count(Species) %>% 
  dplyr::filter(n >= 20)

#loop over species:
for (aspecies in speciesList) {  
  print(aspecies)
  
  #Mean species condition for line plot:
  CondPlot <- condNSpp %>% dplyr::filter(Species == aspecies) %>% dplyr::select(MeanCond, upper.ci.cond, lower.ci.cond, YEAR)
  
  #Test for regime shifts in each species (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
  SppCond <- cond.epu %>% dplyr::filter(Species == aspecies) %>% dplyr::select(RelCond, YEAR)
  Regime <- rpart::rpart(RelCond~YEAR, data=SppCond)
  #Selecting best fit (gives optimal CP value associated with the minimum error)::
  # Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]
  
  SppPlot <- rpart.plot::rpart.plot(Regime)
  
  # Prettier plot of pruned tree (not currently working):
  # library(rpart.plot)
  # library(RColorBrewer)
  
  # ptree<- prune(Regime,
  #               + cp= Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"])
  # fancyRpartPlot(ptree, uniform=TRUE,
  #                  + main="Pruned Classification Tree")
  #plotcp
  
  
  #Outputs pruning tree table:
  saveRDS(Regime[["cptable"]],file = here::here("output","RegimeShifts", paste0(aspecies,"_RelCondition_Regimes_Fall.RDS")))
  # printcp(Regime)
  
  #Single species example:
  # SppCond <- cond.epu %>% dplyr::filter(Species == "American plaice") %>% dplyr::select(RelCond, YEAR)
  # Regime <- rpart::rpart(RelCond~YEAR, data=SppCond)
  # saveRDS(Regime[["cptable"]],file = here::here("output","RegimeShifts", paste0("AmPl_RelCondition_Regimes_Fall.RDS")))
  # readRDS(file = here::here("output","RegimeShifts", paste0("Atlantic sharpnose shark_RelCondition_Regimes_Fall.RDS")))
  
  #Select best pruned tree (outputs the row of the cptable that has the number of splits with the lowest error (xerror)
  # Used rpart::prune
  optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
  optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
  Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
  Regime <- Regime_pruned

  #Pull regime shift years into new data frame to add to plot (use the simplest tree 
  #within one standard error (xstd) of the best tree (lowest xerror)):
  Results <- as.data.frame(Regime[["splits"]])
  SppSplit1 <- Results$index[1]
  SppSplit2 <- Results$index[2]
  SppSplit3 <- Results$index[3]
  SppSplit4 <- Results$index[4]
  SppSplit5 <- Results$index[5]
  
  
  annualCondition <- CondPlot 
  
  #change YEAR to continuous numeric for plotting function below:
  annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))
  
  speciesNames <- annualCondition
  #    dplyr::filter(sexMF == "F") %>%
  
  
  #See 5 scale colors for viridis:
  #scales::show_col(viridis::viridis_pal()(5))
  #vir <- viridis::viridis_pal()(5)
  
  #Line plot of condition
  p2 <- ggplot(speciesNames, aes(x = YEAR, y = MeanCond)) +
    geom_line()+
    geom_errorbar(width=.1, aes(ymin=lower.ci.cond, ymax=upper.ci.cond), colour="black") +
    geom_errorbar(width=.1, aes(ymin=lower.ci.cond, ymax=upper.ci.cond)) +
    geom_point() +
    labs(title= paste0(aspecies, " Relative Condition"), y = "Relative Condition") +
    geom_vline(xintercept=SppSplit1, color='red')+
    geom_vline(xintercept=SppSplit2, color='red')+
    geom_vline(xintercept=SppSplit3, color='red')+
    geom_vline(xintercept=SppSplit4, color='red')+
    geom_vline(xintercept=SppSplit5, color='red')+
    ylim(0.85, 1.21)+
    xlim(1992, 2021)
  
  ggsave(path= here::here("output","RegimeShifts"),paste0(aspecies, "_RelCondition_Regimes_Fall.jpg"), width = 8, height = 3.75, units = "in", dpi = 300)
}

#Automate zooplankton regime shifts from Harvey's data: EcoMon_ZooplanktonData2021_BTSMeanAbundance.csv from gam_calcs_strata.R
#Dataset of zooplankton for regime shift
ZoopSeasonEPU <- ZoopDataEPU %>% dplyr::select(YEAR, EPU,
CopepodSmLgSpringEPU,CopepodSmLgSummmerEPU,CopepodSmLgFallEPU,
CopepodSmLgWinterEPU,TotCopSpringEPU,TotCopSummerEPU,
TotCopFallEPU,TotCopWinterEPU,ZoopAbundSpringEPU, 
ZoopAbundSummerEPU,ZoopAbundFallEPU,ZoopAbundWinterEPU) %>%
  dplyr::distinct()
Zoop1 <- ZoopSeasonEPU %>%
  # key=zoopIndex <- c(CopepodSmLgSpringEPU,CopepodSmLgSummmerEPU,CopepodSmLgFallEPU,
  #       CopepodSmLgWinterEPU,TotCopSpringEPU,TotCopSummerEPU,
  #       TotCopFallEPU,TotCopWinterEPU,ZoopAbundSpringEPU, 
  #       ZoopAbundSummerEPU,ZoopAbundFallEPU,ZoopAbundWinterEPU)
  tidyr::gather(key= 'ZoopName', value = 'ZoopData', -YEAR, -EPU)


#******Need to add EPU to name of zooplankton variable name to automate regime shifts by zooplankton index and EPU
  Zoop <- Zoop1 %>% dplyr::select(YEAR, EPU, ZoopName, ZoopData) %>%
  dplyr::mutate(EPU=paste('ZoopName', EPU, sep="")) %>%
  tidyr::spread(EPU, ZoopName)


#readRDS(file = here::here("output","RegimeShifts", paste0("American plaice_RelCondition_Regimes_Fall.RDS")))

##End automated regime shift plots by species



#FEMALE butterfish condition and regime shift:
# annualCondition <- FemButtCondPlot 
# 
# #change YEAR to continuous numeric for plotting function below:
# annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))
# 
# speciesNames <- annualCondition
# 
# #Line plot of condition
# p2 <- ggplot(speciesNames, aes(x = YEAR, y = MeanCond)) +
#     geom_line()+
#     geom_point() +
#     labs(title="Female Butterfish Relative Condition", y = "Relative Condition") +
#     geom_vline(xintercept=FemButtSplit1, color='red')+
#     geom_vline(xintercept=FemButtSplit2, color='red')
# 
# ggsave(path= here::here(out.dir),"Female_Butterfish_ShelfCondition_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
# 
# #MALE butterfish condition and regime shift:
# annualCondition <- MaleButtCondPlot 
# 
# #change YEAR to continuous numeric for plotting function below:
# annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))
# 
# speciesNames <- annualCondition
# 
# #Line plot of condition
# p2 <- ggplot(speciesNames, aes(x = YEAR, y = MeanCond)) +
#     geom_line()+
#     geom_point() +
#     labs(title="Male Butterfish Relative Condition", y = "Relative Condition") +
#     geom_vline(xintercept=MaleButtSplit1, color='red')+
#     geom_vline(xintercept=MaleButtSplit2, color='red')
# 
# ggsave(path= here::here(out.dir),"Male_Butterfish_ShelfCondition_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgSummerTemp <- AvgTempSummerFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempSummer) %>% group_by(EPU)

#Line plot of summer temp:
p2 <- ggplot(AvgSummerTemp, aes(x = YEAR, y = AvgTempSummer)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Average Summer Bottom Temperature by EPU", y = "Average Summer Bottom Temp") +
  geom_vline(xintercept=SummerSplit1, color='red') 
#+
  # geom_vline(xintercept=SummerSplit2, color='red') +
  # geom_vline(xintercept=SummerSplit3, color='red') +
  # geom_vline(xintercept=SummerSplit4, color='red') +
  # geom_vline(xintercept=SummerSplit5, color='red') 

ggsave(path= here::here(out.dir),"AverageSummerBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgSpringTemp <- AvgTempSpringFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempSpring) %>% group_by(EPU)

#Line plot of spring temp:
p2 <- ggplot(AvgSpringTemp, aes(x = YEAR, y = AvgTempSpring)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Average Spring Bottom Temperature by EPU", y = "Average Spring Bottom Temp") +
  geom_vline(xintercept=SpringSplit1, color='red') 
#+
  # geom_vline(xintercept=SpringSplit2, color='red') +
  # geom_vline(xintercept=SpringSplit3, color='red') +
  # geom_vline(xintercept=SpringSplit4, color='red')

ggsave(path= here::here(out.dir),"AverageSpringBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgFallTemp <- AvgTempFallFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempFall) %>% group_by(EPU)

#Line plot of fall temp:
p2 <- ggplot(AvgFallTemp, aes(x = YEAR, y = AvgTempFall)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Average Fall Bottom Temperature by EPU", y = "Average Fall Bottom Temp") +
  geom_vline(xintercept=FallSplit1, color='red') 

ggsave(path= here::here(out.dir),"AverageFallBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgWinterTemp <- AvgTempWinterFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempWinter) %>% group_by(EPU)

#Line plot of winter temp:
p2 <- ggplot(AvgWinterTemp, aes(x = YEAR, y = AvgTempWinter)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Average Winter Bottom Temperature by EPU", y = "Average Winter Bottom Temp") +
  geom_vline(xintercept=WinterSplit1, color='red') 

ggsave(path= here::here(out.dir),"AverageWinterBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Copepod small to large ratio data by EPU (from gam_calcs_strata.R):
CopepodEPUdata <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, CopepodSmallLarge) %>% group_by(EPU)

#Line plot of coppepod small to large index:
p2 <- ggplot(CopepodEPUdata, aes(x = YEAR, y = CopepodSmallLarge)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Copepod Size Index by EPU", y = "Copepod Size Index") +
  geom_vline(xintercept=CopepodEPUSplit1, color='red') +
  geom_vline(xintercept=CopepodEPUSplit2, color='red')

ggsave(path= here::here(out.dir),"CopepodSmLgEPU_regime2019.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phyoplanktion bloom magnitude data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, RangeMagnitude)

#Line plot of fall phytoplankton mag:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeMagnitude)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title="Fall Phytoplankton Bloom Magnitude", y = "Fall Bloom Magnitude")

ggsave(path= here::here(out.dir),"FallBloom_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phytoplankton bloom duration data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, RangeDuration)

#Line plot of fall phytoplankton duration:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeDuration)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title="Fall Phytoplankton Bloom Duration", y = "Fall Bloom Duration")

ggsave(path= here::here(out.dir),"FallBloom_duration_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#"TotalCopepods","ZooplanktonBiomass","StomachFullness"

#Total Copopods data (from NEFSCZooplankton_v3_6b_v2018.xls from gam_calcs_strata.R):
TotCopData <- TotalCopepods %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, TotalCopepodsMillions) %>% group_by(EPU)

#Line plot of total copepods:
p2 <- ggplot(TotCopData, aes(x = YEAR, y = TotalCopepodsMillions)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green")) +
  geom_point(aes(color = EPU)) +
  labs(title="Total Copepods", y = "Total Copepods Millions") +
  geom_vline(xintercept=TotCopepodsSplit1, color='red') +
  geom_vline(xintercept=TotCopepodsSplit2, color='red')

ggsave(path= here::here(out.dir),"TotalCopepods_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Spring Total copepods from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZoopData from gam_calcs_strata.R:
#******Need to run these regime shifts separately by EPU:
TotalCopSpr <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Spring') %>%
  dplyr::select(YEAR, TotalCopepodStrata) %>%
   dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotCop = sum(TotalCopepodStrata)) %>%
  dplyr::select(YEAR, SumTotCop) %>%
    dplyr::distinct()

#Regime analysis:
CopRegime <- TotalCopSpr %>% dplyr::select(SumTotCop, YEAR)
Regime <- rpart::rpart(SumTotCop~YEAR, data=CopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_spr2022.RDS"))
 printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualCopepods <- CopRegime 

#change YEAR to continuous numeric for plotting function below:
annualCopepods$YEAR <- as.numeric(as.character(annualCopepods$YEAR))

TotCopRegime <- annualCopepods

#Line plot of condition
p2 <- ggplot(TotCopRegime, aes(x = YEAR, y = SumTotCop)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Copepods Spring", y = "Total Copepods (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalCopepods_Regimes_spring.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Summer Total copepods from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZoopData from gam_calcs_strata.R:
TotalCopSummer <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Summer') %>%
  dplyr::select(YEAR, TotalCopepodStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotCop = sum(TotalCopepodStrata)) %>%
  dplyr::select(YEAR, SumTotCop) %>%
  dplyr::distinct()

#Regime analysis:
CopRegime <- TotalCopSummer %>% dplyr::select(SumTotCop, YEAR)
Regime <- rpart::rpart(SumTotCop~YEAR, data=CopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Summer2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualCopepods <- CopRegime 

#change YEAR to continuous numeric for plotting function below:
annualCopepods$YEAR <- as.numeric(as.character(annualCopepods$YEAR))

TotCopRegime <- annualCopepods

#Line plot of condition
p2 <- ggplot(TotCopRegime, aes(x = YEAR, y = SumTotCop)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Copepods Summer", y = "Total Copepods (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalCopepods_Regimes_Summer.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall Total copepods from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZoopData from gam_calcs_strata.R:
TotalCopFall <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Fall') %>%
  dplyr::select(YEAR, TotalCopepodStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotCop = sum(TotalCopepodStrata)) %>%
  dplyr::select(YEAR, SumTotCop) %>%
  dplyr::distinct()

#Regime analysis:
CopRegime <- TotalCopFall %>% dplyr::select(SumTotCop, YEAR)
Regime <- rpart::rpart(SumTotCop~YEAR, data=CopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Fall2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualCopepods <- CopRegime 

#change YEAR to continuous numeric for plotting function below:
annualCopepods$YEAR <- as.numeric(as.character(annualCopepods$YEAR))

TotCopRegime <- annualCopepods

#Line plot of condition
p2 <- ggplot(TotCopRegime, aes(x = YEAR, y = SumTotCop)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Copepods Fall", y = "Total Copepods (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalCopepods_Regimes_Fall.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Winter Total copepods from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZoopData from gam_calcs_strata.R:
TotalCopWinter <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Winter') %>%
  dplyr::select(YEAR, TotalCopepodStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotCop = sum(TotalCopepodStrata)) %>%
  dplyr::select(YEAR, SumTotCop) %>%
  dplyr::distinct()

#Regime analysis:
CopRegime <- TotalCopWinter %>% dplyr::select(SumTotCop, YEAR)
Regime <- rpart::rpart(SumTotCop~YEAR, data=CopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Winter2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualCopepods <- CopRegime 

#change YEAR to continuous numeric for plotting function below:
annualCopepods$YEAR <- as.numeric(as.character(annualCopepods$YEAR))

TotCopRegime <- annualCopepods

#Line plot of condition
p2 <- ggplot(TotCopRegime, aes(x = YEAR, y = SumTotCop)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Copepods Winter", y = "Total Copepods (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalCopepods_Regimes_Winter.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Spring Total zooplankton abundance from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZooSeason from gam_calcs_strata.R:
TotalZoopSpr <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Spring') %>%
  dplyr::select(YEAR, ZooplAbundStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotalZooSpr = sum(ZooplAbundStrata), na.rm = TRUE) %>%
  dplyr::select(YEAR, SumTotalZooSpr) %>%
  dplyr::distinct()

#Regime analysis:
ZooRegime <- TotalZoopSpr %>% dplyr::select(SumTotalZooSpr, YEAR)
Regime <- rpart::rpart(SumTotalZooSpr~YEAR, data=ZooRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Spring2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualZoopl <- ZooRegime 

#change YEAR to continuous numeric for plotting function below:
annualZoopl$YEAR <- as.numeric(as.character(annualZoopl$YEAR))

TotZooRegime <- annualZoopl

#Line plot of condition
p2 <- ggplot(TotZooRegime, aes(x = YEAR, y = SumTotalZooSpr)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Zooplankton Abundance Spring", y = "Total Zooplankton Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalZoopl_Regimes_Spring.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Summer Total zooplankton abundance from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZooSeason from gam_calcs_strata.R:
TotalZoopSummer <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Summer') %>%
  dplyr::select(YEAR, ZooplAbundStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotalZooSummer = sum(ZooplAbundStrata), na.rm = TRUE) %>%
  dplyr::select(YEAR, SumTotalZooSummer) %>%
  dplyr::distinct()

#Regime analysis:
ZooRegime <- TotalZoopSummer %>% dplyr::select(SumTotalZooSummer, YEAR)
Regime <- rpart::rpart(SumTotalZooSummer~YEAR, data=ZooRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Summer2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualZoopl <- ZooRegime 

#change YEAR to continuous numeric for plotting function below:
annualZoopl$YEAR <- as.numeric(as.character(annualZoopl$YEAR))

TotZooRegime <- annualZoopl

#Line plot of condition
p2 <- ggplot(TotZooRegime, aes(x = YEAR, y = SumTotalZooSummer)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Zooplankton Abundance Summer", y = "Total Zooplankton Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalZoopl_Regimes_Summer.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Fall Total zooplankton abundance from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZooSeason from gam_calcs_strata.R:
TotalZoopFall <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Fall') %>%
  dplyr::select(YEAR, ZooplAbundStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotalZooFall = sum(ZooplAbundStrata), na.rm = TRUE) %>%
  dplyr::select(YEAR, SumTotalZooFall) %>%
  dplyr::distinct()

#Regime analysis:
ZooRegime <- TotalZoopFall %>% dplyr::select(SumTotalZooFall, YEAR)
Regime <- rpart::rpart(SumTotalZooFall~YEAR, data=ZooRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Fall2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualZoopl <- ZooRegime 

#change YEAR to continuous numeric for plotting function below:
annualZoopl$YEAR <- as.numeric(as.character(annualZoopl$YEAR))

TotZooRegime <- annualZoopl

#Line plot of condition
p2 <- ggplot(TotZooRegime, aes(x = YEAR, y = SumTotalZooFall)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Zooplankton Abundance Fall", y = "Total Zooplankton Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalZoopl_Regimes_Fall.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Winter Total zooplankton abundance from Harvey: EcoMon_ZooplanktonData_BTSMeanAbundance.csv, as ZooSeason from gam_calcs_strata.R:
TotalZoopWinter <- ZooSeason %>% dplyr::filter(YEAR >= 1992, season1 == 'Winter') %>%
  dplyr::select(YEAR, ZooplAbundStrata) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::mutate(SumTotalZooWinter = sum(ZooplAbundStrata), na.rm = TRUE) %>%
  dplyr::select(YEAR, SumTotalZooWinter) %>%
  dplyr::distinct()

#Regime analysis:
ZooRegime <- TotalZoopWinter %>% dplyr::select(SumTotalZooWinter, YEAR)
Regime <- rpart::rpart(SumTotalZooWinter~YEAR, data=ZooRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Winter2022.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


annualZoopl <- ZooRegime 

#change YEAR to continuous numeric for plotting function below:
annualZoopl$YEAR <- as.numeric(as.character(annualZoopl$YEAR))

TotZooRegime <- annualZoopl

#Line plot of condition
p2 <- ggplot(TotZooRegime, aes(x = YEAR, y = SumTotalZooWinter)) +
  geom_line()+
  geom_point() +
  labs(title= "Total Zooplankton Abundance Winter", y = "Total Zooplankton Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"TotalZoopl_Regimes_Winter.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Regime shifts in surface temp:
Surfdata <- cond.epu %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, SURFTEMP) %>%
  dplyr::filter(!is.na(SURFTEMP)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(AvgSurfTemp = mean(SURFTEMP)) 

#Regime analysis:
SurfRegime <- Surfdata %>% dplyr::select(AvgSurfTemp, YEAR)
Regime <- rpart::rpart(AvgSurfTemp~YEAR, data=SurfRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "SurfaceTemp_Spring.RDS"))
printcp(Regime)


optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
Regime <- Regime_pruned

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
Results <- as.data.frame(Regime[["splits"]])
SppSplit1 <- Results$index[1]
SppSplit2 <- Results$index[2]
SppSplit3 <- Results$index[3]
SppSplit4 <- Results$index[4]
SppSplit5 <- Results$index[5]


#annualZoopl <- SurfRegime 

#change YEAR to continuous numeric for plotting function below:
SurfRegime$YEAR <- as.numeric(as.character(SurfRegime$YEAR))

AnnualSurfRegime <- SurfRegime

#Line plot of condition
p2 <- ggplot(AnnualSurfRegime, aes(x = YEAR, y = AvgSurfTemp)) +
  geom_line()+
  geom_point() +
  labs(title= "Average Spring Surface Temperature", y = "Average Surface Temperature") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"SurfaceTemp_Spring_Regimes.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
