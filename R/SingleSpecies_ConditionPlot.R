library(ggplot2)
library(dplyr)


out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Summarize annually by EPU (use for SOE plots)
annualcondEPU <- cond.epu %>% dplyr::group_by(Species,EPU, YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcondEPU, nCond>=3) %>% ungroup()
condNSppEPU <- condN %>% dplyr::add_count(Species, EPU) %>% 
    dplyr::filter(n >= 20)

# #Summarize annually over all EPUs for butterfish WG:
# annualcond <- cond.epu %>% dplyr::group_by(Species,YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
# condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
# condNSpp <- condN %>% dplyr::add_count(Species) %>% 
#     dplyr::filter(n >= 20)
# 
# #Mean butterfish condition for line plot (SingleSpecies_ConditionPlot.R):
# ButtCondPlot <- condNSpp %>% dplyr::filter(Species == 'Butterfish') %>% dplyr::select(MeanCond, YEAR)
# 
# #Test for regime shifts in butterfish (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
# ButtCond <- cond.epu %>% dplyr::filter(Species == 'Butterfish') %>% dplyr::select(RelCond, YEAR)
# ButtRegime <- rpart::rpart(RelCond~YEAR, data=ButtCond)
# ButtPlot <- rpart.plot::rpart.plot(ButtRegime)
# 
# #Pull regime shift years into new data frame to add to plot (use the simplest tree 
# #within one standard error (xstd) of the best tree (lowest xerror)):
# ButtResults <- as.data.frame(ButtRegime[["splits"]])
# ButtSplit1 <- ButtResults$index[1]
# ButtSplit2 <- ButtResults$index[2]
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
AllSppCond <- condNSpp %>% dplyr::select(MeanCond, YEAR)
AllSppRegime <- rpart::rpart(MeanCond~YEAR, data=AllSppCond)
AllSppPlot <- rpart.plot::rpart.plot(AllSppRegime)

#Pull regime shift years into new data frame to add to plot:
AllSppResults <- as.data.frame(AllSppRegime[["splits"]])
AllSppSplit1 <- AllSppResults$index[1]
AllSppSplit2 <- AllSppResults$index[2]

#Summarize annually over all EPUs for mackerel:
annualcond <- cond.epu %>% dplyr::group_by(Species,YEAR) %>% dplyr::summarize(MeanCond = mean(RelCond), nCond = dplyr::n())
condN <- dplyr::filter(annualcond, nCond>=3) %>% ungroup()
condNSpp <- condN %>% dplyr::add_count(Species) %>% 
    dplyr::filter(n >= 20)

#Mean mackerel condition for line plot (SingleSpecies_ConditionPlot.R):
MackCondPlot <- condNSpp %>% dplyr::filter(Species == 'Atlantic mackerel') %>% dplyr::select(MeanCond, YEAR)

#Test for regime shifts in mackerel (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
MackCond <- cond.epu %>% dplyr::filter(Species == 'Atlantic mackerel') %>% dplyr::select(RelCond, YEAR)
MackRegime <- rpart::rpart(RelCond~YEAR, data=MackCond)
MackPlot <- rpart.plot::rpart.plot(MackRegime)
#Outputs pruning tree table:
#printcp(MackRegime)

#Pull regime shift years into new data frame to add to plot (use the simplest tree 
#within one standard error (xstd) of the best tree (lowest xerror)):
MackResults <- as.data.frame(MackRegime[["splits"]])
MackSplit1 <- MackResults$index[1]
MackSplit2 <- MackResults$index[2]
MackSplit3 <- MackResults$index[3]


#Removed MAB values in 2017 due to low sampling coverage:
#annualCondition <- ButtCondPlot 
#%>% 
 #   dplyr::filter(!(EPU == "MAB" & YEAR == 2017)) 
 #   dplyr::filter(!(YEAR == 2017)) 

annualCondition <- MackCondPlot 

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
    labs(title="Atlantic Mackerel Relative Condition", y = "Relative Condition") +
    geom_vline(xintercept=MackSplit1, color='red')+
    geom_vline(xintercept=MackSplit2, color='red')+
    geom_vline(xintercept=MackSplit3, color='red')

ggsave(path= here::here(out.dir),"AtlMackerel_ShelfCondition_allsex_2022_viridis.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
    geom_vline(xintercept=SummerSplit1, color='red') +
    geom_vline(xintercept=SummerSplit2, color='red') +
    geom_vline(xintercept=SummerSplit3, color='red') +
    geom_vline(xintercept=SummerSplit4, color='red') +
    geom_vline(xintercept=SummerSplit5, color='red') 

ggsave(path= here::here(out.dir),"AverageSummerBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgSpringTemp <- AvgTempSpringFormat %>% dplyr::filter(YEAR >= 1992) %>%
    dplyr::select(YEAR, EPU, AvgTempSpring) %>% group_by(EPU)

#Line plot of summer temp:
p2 <- ggplot(AvgSpringTemp, aes(x = YEAR, y = AvgTempSpring)) +
    geom_line(aes(color = EPU)) + 
    scale_color_manual(values = c("red", "blue", "green", "orange")) +
    geom_point(aes(color = EPU)) +
    labs(title="Average Spring Bottom Temperature by EPU", y = "Average Spring Bottom Temp") +
    geom_vline(xintercept=SpringSplit1, color='red') +
    geom_vline(xintercept=SpringSplit2, color='red') +
    geom_vline(xintercept=SpringSplit3, color='red') +
    geom_vline(xintercept=SpringSplit4, color='red')

ggsave(path= here::here(out.dir),"AverageSpringBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgFallTemp <- AvgTempFallFormat %>% dplyr::filter(YEAR >= 1992) %>%
    dplyr::select(YEAR, EPU, AvgTempFall) %>% group_by(EPU)

#Line plot of summer temp:
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

#Line plot of summer temp:
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

#Line plot of summer temp:
p2 <- ggplot(CopepodEPUdata, aes(x = YEAR, y = CopepodSmallLarge)) +
    geom_line(aes(color = EPU)) + 
    scale_color_manual(values = c("red", "blue", "green", "orange")) +
    geom_point(aes(color = EPU)) +
    labs(title="Copepod Small/Large Ratio by EPU", y = "Copepod Small/Large Ratio") +
    geom_vline(xintercept=CopepodEPUSplit1, color='red') +
    geom_vline(xintercept=CopepodEPUSplit2, color='red')

ggsave(path= here::here(out.dir),"CopepodSmLgEPU_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phyoplanktion bloom magnitude data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
    dplyr::select(YEAR, RangeMagnitude)

#Line plot of summer temp:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeMagnitude)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title="Fall Phytoplankton Bloom Magnitude", y = "Fall Bloom Magnitude")

ggsave(path= here::here(out.dir),"FallBloom_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phyoplanktion bloom duration data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
    dplyr::select(YEAR, RangeDuration)

#Line plot of summer temp:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeDuration)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title="Fall Phytoplankton Bloom Duration", y = "Fall Bloom Duration")

ggsave(path= here::here(out.dir),"FallBloom_duration_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#"TotalCopepods","ZooplanktonBiomass","StomachFullness"

#Total Copopods data (from gam_calcs_strata.R):
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
