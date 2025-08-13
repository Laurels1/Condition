
library(ggplot2)
library(dplyr)
library(tidyr)
library(rpart)

out.dir="output"

#Mean GOM larvacean abundance anomalies from Isabel Honda 
Larvaceans <- readr::read_csv(here::here(data.dir, "GoM_larvaceanAnomaly.csv"))

GOMlarvaceans <- Larvaceans %>% dplyr::filter(year >= 1992)

#Regime analysis:
GOMlarvRegime <- GOMlarvaceans %>% dplyr::select(anomaly_total_mean, year)
Regime <- rpart::rpart(anomaly_total_mean~year, data=GOMlarvRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "GOMlarvaceans_Regimes_2023.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMlarvRegime$year <- as.numeric(as.character(GOMlarvRegime$year))

#Line plot of condition
p2 <- ggplot(GOMlarvRegime, aes(x = year, y = anomaly_total_mean)) +
  geom_line()+
  geom_point() +
  labs(title= "GOM Larvacean Abundance Anomalies", y = "Total Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"GOMlarvaceans_Regimes_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Mean GOM euphausiid abundance anomalies from Isabel Honda
Euph <- readr::read_csv(here::here(data.dir, "GoM_euphausiidAnomaly.csv"))

GOMeuph <- Euph %>% dplyr::filter(year >= 1992)

#Regime analysis for annual GOM euphausiids:
GOMeuphRegime <- GOMeuph %>% dplyr::select(anomaly_total_mean, year)
Regime <- rpart::rpart(anomaly_total_mean~year, data=GOMeuphRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "GOMeuphausiid_Regimes_2023.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMeuphRegime$year <- as.numeric(as.character(GOMeuphRegime$year))

#Line plot of condition
p2 <- ggplot(GOMeuphRegime, aes(x = year, y = anomaly_total_mean)) +
  geom_line()+
  geom_point() +
  labs(title= "GOM Euphausiid Abundance Anomalies", y = "Total Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"GOMeuphausiid_Regimes_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Regime analysis for spring GOM euphausiids (data from Isabel Honda):
GOMeuphRegime <- GOMeuph %>% dplyr::select(anomaly_spring_mean, year)
Regime <- rpart::rpart(anomaly_spring_mean~year, data=GOMeuphRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "SprGOMeuphausiid_Regimes_2023.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMeuphRegime$year <- as.numeric(as.character(GOMeuphRegime$year))

#Line plot of condition
p2 <- ggplot(GOMeuphRegime, aes(x = year, y = anomaly_spring_mean)) +
  geom_line()+
  geom_point() +
  labs(title= "Spring GOM Euphausiid Abundance Anomalies", y = "Spring Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"SprGOMeuphausiid_Regimes_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Regime analysis for spring GOM larvaceans (data from Isabel Honda):
Larv <- readr::read_csv(here::here(data.dir, "GoM_larvaceanAnomaly.csv"))

GOMlarv <- Larv %>% dplyr::filter(year >= 1992)

GOMlarvRegime <- GOMlarv %>% dplyr::select(anomaly_spring_mean, year)
Regime <- rpart::rpart(anomaly_spring_mean~year, data=GOMlarvRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "SprGOMlarvacean_Regimes_2023.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMlarvRegime$year <- as.numeric(as.character(GOMlarvRegime$year))

#Line plot of condition
p2 <- ggplot(GOMlarvRegime, aes(x = year, y = anomaly_spring_mean)) +
  geom_line()+
  geom_point() +
  labs(title= "Spring GOM Larvacean Abundance Anomalies", y = "Spring Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"SprGOMlarvacean_Regimes_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Small to large copepod anomalies (by season, shelf-wide from Ryan Morse on Aug. 1, 2025):
CalfinNEUS <-load(here::here("data","20250801_NES_SLI_Spr.rdata"))
CopSmLgSprNEUS <- SLI.nes.spr

CopSmLgSprNEUSRegime <- CopSmLgSprNEUS %>% dplyr::select(SLI, year) %>%
  dplyr::filter(year>=1992)

Regime <- rpart::rpart(SLI~year, data=CopSmLgSprNEUSRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "SprNEUS_SmLgCop_Regimes_2023.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
CopSmLgSprNEUSRegime$year <- as.numeric(as.character(CopSmLgSprNEUSRegime$year))

#Line plot of condition
p2 <- ggplot(CopSmLgSprNEUSRegime, aes(x = year, y = SLI)) +
  geom_line()+
  geom_point() +
  labs(title= "Spring NEUS Copepod Size Index Anomalies", y = "Spring Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"SprNEUS_SmLgCop_Regimes_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Mean GOM zooplankton abundance anomalies from Ryan Morse (GOM_mean_seasonal_anomalies.csv)
GOMseasonZooAbund <- readr::read_csv(here::here(data.dir, "GOM_mean_seasonal_anomalies.csv"))

GOMsummerZoop <- GOMseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Spring') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, mlucens_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
GOMZoopRegime <- GOMsummerZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=GOMZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "GOMspringZoop_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMZoopRegime$year <- as.numeric(as.character(GOMZoopRegime$year))

#Line plot of condition
p2 <- ggplot(GOMZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "GOM Spring Zooplankton Abundance Anomalies", y = "Total Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

#ggsave(path= here::here("output"),"GOMspringZoop_Regimes_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

GOMseasonZooAbund <- readr::read_csv(here::here(data.dir, "GOM_mean_seasonal_anomalies.csv"))

GOMsummerZoop <- GOMseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Summer') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, mlucens_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
GOMZoopRegime <- GOMsummerZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=GOMZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
#saveRDS(Regime[["cptable"]],file = here::here("output", "GOMsummerZoopAnom_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMZoopRegime$year <- as.numeric(as.character(GOMZoopRegime$year))

#Line plot of condition
p2 <- ggplot(GOMZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "GOM Summer Zooplankton Abundance Anomalies", y = "Total Abundance Anomaly", x = "Year") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"GOMsummerZoopAnomaly_Regimes_2021_Year.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

######GOM zooplankton annual anomolies:
GOMseasonZooAbund <- readr::read_csv(here::here(data.dir, "GOM_mean_seasonal_anomalies.csv"))

GOMsummerZoop <- GOMseasonZooAbund %>% dplyr::filter(year >= 1992) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, mlucens_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
GOMZoopRegime <- GOMsummerZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=GOMZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "GOMAnnualZoopAnom_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GOMZoopRegime$year <- as.numeric(as.character(GOMZoopRegime$year))

#Line plot of condition
p2 <- ggplot(GOMZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "GOM Annual Zooplankton Abundance Anomalies", y = "Total Abundance Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')
#  geom_vline(xintercept=SppSplit3, color='red')+
#  geom_vline(xintercept=SppSplit4, color='red')+
#  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"GOMAnnualZoopAnomaly_Regimes_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Mean GB zooplankton abundance anomalies from Ryan Morse (GBK_mean_seasonal_anomalies.csv)
GBseasonZooAbund <- readr::read_csv(here::here(data.dir, "GBK_mean_seasonal_anomalies.csv"))

GBsummerZoop <- GBseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Summer') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, chaeto_100m3, cham_100m3, para_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
GBZoopRegime <- GBsummerZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=GBZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
#saveRDS(Regime[["cptable"]],file = here::here("output", "GBsummerZoop_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
GBZoopRegime$year <- as.numeric(as.character(GBZoopRegime$year))

#Line plot of condition
p2 <- ggplot(GBZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "GB Summer Zooplankton Abundance Anomalies", y = "Total Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

#ggsave(path= here::here("output"),"GBsummerZoop_Regimes_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Mean MAB zooplankton abundance anomalies from Ryan Morse (MAB_mean_seasonal_anomalies.csv)
MABseasonZooAbund <- readr::read_csv(here::here(data.dir, "MAB_mean_seasonal_anomalies.csv"))

MABsummerZoop <- MABseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Spring') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, tlong_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
MABZoopRegime <- MABsummerZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=MABZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
#saveRDS(Regime[["cptable"]],file = here::here("output", "MABspringZoop_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
MABZoopRegime$year <- as.numeric(as.character(MABZoopRegime$year))

#Line plot of condition
p2 <- ggplot(MABZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "MAB Spring Zooplankton Abundance Anomalies", y = "Total Abundance (millions)") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

#ggsave(path= here::here("output"),"MABspringZoop_Regimes_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#MAB fall Zooplankton anomaly from Ryan Morse:
MABseasonZooAbund <- readr::read_csv(here::here(data.dir, "MAB_mean_seasonal_anomalies.csv"))

MABfallZoop <- MABseasonZooAbund %>% dplyr::filter(year >= 1992, season == 'Fall') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(SumZoop = sum(ctyp_100m3, calfin_100m3, tlong_100m3, pseudo_100m3)) %>%
  dplyr::select(year, SumZoop)

#Regime analysis:
MABZoopRegime <- MABfallZoop %>% dplyr::select(SumZoop, year)
Regime <- rpart::rpart(SumZoop~year, data=MABZoopRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
#saveRDS(Regime[["cptable"]],file = here::here("output", "MABfallZoopAnom_Regimes_2021.RDS"))
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


#change YEAR to continuous numeric for plotting function below:
MABZoopRegime$year <- as.numeric(as.character(MABZoopRegime$year))

#Line plot of condition
p2 <- ggplot(MABZoopRegime, aes(x = year, y = SumZoop)) +
  geom_line()+
  geom_point() +
  labs(title= "MAB Fall Zooplankton Abundance Anomalies", y = "Total Zooplankton Anomaly") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

#ggsave(path= here::here("output"),"MABfallZoopAnom_Regimes_2021.jpg", width = 8, height = 3.75, units = "in", dpi = 300)



#Automate zooplankton regime shifts from Harvey's data: EcoMon_ZooplanktonData2021_BTSMeanAbundance.csv from gam_calcs_strata.R
#Dataset of zooplankton for regime shift
ZoopSeasonEPU <- ZoopDataSeasonEPU %>% dplyr::select(YEAR, EPU, SEASONS,
                                               CopepodSmLgEPU,TotCopEPU,ZoopAbundEPU) %>%
  dplyr::filter(!is.na(EPU)) %>%
  dplyr::filter(!is.na(SEASONS)) %>%
  dplyr::distinct()
Zoop1 <- ZoopSeasonEPU %>%
  # key=zoopIndex <- c(CopepodSmLgSpringEPU,CopepodSmLgSummmerEPU,CopepodSmLgFallEPU,
  #       CopepodSmLgWinterEPU,TotCopSpringEPU,TotCopSummerEPU,
  #       TotCopFallEPU,TotCopWinterEPU,ZoopAbundSpringEPU, 
  #       ZoopAbundSummerEPU,ZoopAbundFallEPU,ZoopAbundWinterEPU)
  tidyr::gather(key= 'ZoopName', value = 'ZoopData', -YEAR, -EPU, -SEASONS)

Zoop <- Zoop1 %>% dplyr::select(YEAR, EPU, SEASONS, ZoopName, ZoopData) %>%
  dplyr::mutate(ZoopIndex=paste(ZoopName, EPU, sep="_")) 

####Automating regime shift plots for zooplankton data by EPU:

# create a character vector of species names
ZoopList <- unique(Zoop) %>%
  dplyr::distinct(ZoopIndex) %>% 
  dplyr::pull()

#Only select Species with names that have sufficient data:
# numZoop <- length(ZoopList)
# for (i in numZoop:1) {
#   if (!is.na(as.numeric(ZoopList[i]))) {
#     ZoopList <-ZoopList[-i]
#   }
# }
# 
# #loop over zooplankton index:
# for (aZoop in ZoopList) {  
#   print(aZoop)
#   
# 
#   #Test for regime shifts in each zooplankton index (same method as in Perretti et al. 2017, although Perretti uses MRT, gives error when method="mrt"):
#   ZoopRegDat <- Zoop %>% dplyr::filter(ZoopIndex == aZoop) %>% dplyr::select(ZoopData, ZoopIndex, YEAR, SEASONS) 
#   Regime <- rpart::rpart(ZoopData~YEAR, data=ZoopRegDat)
#   #Selecting best fit (gives optimal CP value associated with the minimum error)::
#   # Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]
#   
#   ZoopRegimePlot <- rpart.plot::rpart.plot(Regime)
#   
#   # Prettier plot of pruned tree (not currently working):
#   # library(rpart.plot)
#   # library(RColorBrewer)
#   
#   
#   
#   # ptree<- prune(Regime,
#   #               + cp= Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"])
#   # fancyRpartPlot(ptree, uniform=TRUE,
#   #                  + main="Pruned Classification Tree")
#   #plotcp
#   
#   
#   #Outputs pruning tree table:
#   saveRDS(Regime[["cptable"]],file = here::here("output","RegimeShifts", paste0(aZoop,"_Zooplankton_Regimes.RDS")))
#   # printcp(Regime)
#   
#   #Select best pruned tree (outputs the row of the cptable that has the number of splits with the lowest error (xerror)
#   # Used rpart::prune
#   optimal_cp_index <- as.numeric(which.min(Regime$cptable[,"xerror"]))
#   optimal_cp <- Regime$cptable[optimal_cp_index,"CP"]
#   Regime_pruned <- rpart::prune(Regime, cp = optimal_cp)
#   Regime <- Regime_pruned
#   
#   
#   #Pull regime shift years into new data frame to add to plot (use the simplest tree 
#   #within one standard error (xstd) of the best tree (lowest xerror)):
#   Results <- as.data.frame(Regime[["splits"]])
#   SppSplit1 <- Results$index[1]
#   SppSplit2 <- Results$index[2]
#   SppSplit3 <- Results$index[3]
#   SppSplit4 <- Results$index[4]
#   SppSplit5 <- Results$index[5]
#   
#   
#   ZoopDat <- Zoop 
#   
#   #change YEAR to continuous numeric for plotting function below:
#  ZoopDat$YEAR <- as.numeric(as.character(ZoopDat$YEAR))
# 
#  ZoopIndexRegime <- ZoopDat %>% dplyr::filter(YEAR >= 1992) %>%
#    dplyr::select(YEAR, SEASONS, ZoopData) %>% group_by(SEASONS)
#  
# 
#   #Line plot of condition
#   p2 <- ggplot(ZoopDat, aes(x = YEAR, y = ZoopData)) +
#    geom_line(aes(color = SEASONS)) + 
#    scale_color_manual(values = c("red", "blue", "green", "orange")) +
#     # geom_errorbar(width=.1, aes(ymin=lower.ci.cond, ymax=upper.ci.cond), colour="black") +
#     # geom_errorbar(width=.1, aes(ymin=lower.ci.cond, ymax=upper.ci.cond)) +
#     geom_point(aes(color = SEASONS)) +
#     labs(title= paste0(aZoop, " Regime Shifts by Season"), y = "aZoop") +
#     geom_vline(xintercept=SppSplit1, color='red')+
#     geom_vline(xintercept=SppSplit2, color='red')+
#     geom_vline(xintercept=SppSplit3, color='red')+
#     geom_vline(xintercept=SppSplit4, color='red')+
#     geom_vline(xintercept=SppSplit5, color='red')+
#     # ylim(0.85, 1.21)+
#     xlim(1992, 2022)
#   
# #  ggsave(path= here::here("output","RegimeShifts"),paste0(aZoop, "_RegimeShifts.jpg"), width = 8, height = 3.75, units = "in", dpi = 300)
# }


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

#ggsave(path= here::here(out.dir),"AverageSummerBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgSpringTemp <- AvgTempSpringFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempSpring) %>% group_by(EPU)

#Line plot of spring temp:
p2 <- ggplot(AvgSpringTemp, aes(x = YEAR, y = AvgTempSpring)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Spring Bottom Temperature by EPU", y = "Temperature Anomaly") +
  geom_vline(xintercept=SpringSplit1, color='red') +
   geom_vline(xintercept=SpringSplit2, color='red') 
#+
 #  geom_vline(xintercept=SpringSplit3, color='red') +
  # geom_vline(xintercept=SpringSplit4, color='red')

ggsave(path= here::here(out.dir),"AverageSpringBottomTempEPU2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Average bottom temp data by EPU and season (from gam_calcs_strata.R):
AvgFallTemp <- AvgTempFallFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, AvgTempFall) %>% group_by(EPU)

#Line plot of fall temp:
p2 <- ggplot(AvgFallTemp, aes(x = YEAR, y = AvgTempFall)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  geom_point(aes(color = EPU)) +
  labs(title="Fall Bottom Temperature by EPU", y = "Temperature Anomaly", x = "Year") +
  geom_vline(xintercept=FallSplit1, color='red')

ggsave(path= here::here(out.dir),"AvgFallBottomTempEPU2022_Year.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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

#ggsave(path= here::here(out.dir),"AverageWinterBottomTempEPU2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Copepod small to large ratio data by EPU for condition manuscript (from gam_calcs_strata.R):
CopepodEPUdata <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, EPU, CopepodSmallLarge) %>% group_by(EPU)

CopepodEPU <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>% dplyr::select(YEAR, CopepodSmallLarge)
CopepodEPURegime <- rpart::rpart(CopepodSmallLarge~YEAR, data=CopepodEPU)
CopepodEPURegimePlot <- rpart.plot::rpart.plot(CopepodEPURegime)
CopepodEPURegime$cptable

#Pull regime shift years into new data frame to add to plot:
CopepodEPURegimeResults <- as.data.frame(CopepodEPURegime[["splits"]])
CopepodEPUSplit1 <- CopepodEPURegimeResults$index[1]
CopepodEPUSplit2 <- CopepodEPURegimeResults$index[2]

#Line plot of copepod small to large index:
p2 <- ggplot(CopepodEPUdata, aes(x = YEAR, y = CopepodSmallLarge)) +
  geom_line(aes(color = EPU)) + 
  scale_color_manual(values = c("red", "blue", "green")) +
  geom_point(aes(color = EPU)) +
  labs(title="Copepod Size Index by EPU", y = "Copepod Size Index", x = "Year") +
  geom_vline(xintercept=CopepodEPUSplit1, color='red') +
  geom_vline(xintercept=CopepodEPUSplit2, color='red') 
# +
#   geom_vline(xintercept=CopepodEPUSplit3, color='red') +
#   geom_vline(xintercept=CopepodEPUSplit4, color='red') +
#   geom_vline(xintercept=CopepodEPUSplit5, color='red') +
#   geom_vline(xintercept=CopepodEPUSplit6, color='red')

ggsave(path= here::here(out.dir),"CopepodSmLgEPU_regime2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#shelf-wide Copepod small to large ratio data by Shelf (from gam_calcs_strata.R):
CopepodShelfdata <- CalfinFormat %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, CopepodSmallLarge) 

#Line plot of coppepod small to large index:
p2 <- ggplot(CopepodShelfdata, aes(x = YEAR, y = CopepodSmallLarge)) +
  geom_line() + 
  scale_color_manual(values = "red") +
  geom_point() +
  labs(title="Shelf-wide Copepod Size Index", y = "Copepod Size Index") +
  geom_vline(xintercept=CopepodShelfSplit1, color='red') +
  geom_vline(xintercept=CopepodShelfSplit2, color='red') 
# +
#   geom_vline(xintercept=CopepodShelfSplit3, color='red') +
#   geom_vline(xintercept=CopepodShelfSplit4, color='red') +
#   geom_vline(xintercept=CopepodShelfSplit5, color='red') +
#   geom_vline(xintercept=CopepodShelfSplit6, color='red')

ggsave(path= here::here(out.dir),"CopepodSmLgShelf_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Fall phyoplanktion bloom magnitude data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, RangeMagnitude)

#Line plot of fall phytoplankton mag:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeMagnitude)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title="Fall Phytoplankton Bloom Magnitude", y = "Fall Bloom Magnitude")

#ggsave(path= here::here(out.dir),"FallBloom_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phytoplankton bloom duration data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, RangeDuration)

#Line plot of fall phytoplankton duration:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeDuration)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title="Fall Phytoplankton Bloom Duration", y = "Fall Bloom Duration")

#ggsave(path= here::here(out.dir),"FallBloom_duration_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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

#ggsave(path= here::here(out.dir),"TotalCopepods_regime2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_spr2022.RDS"))
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

#ggsave(path= here::here("output"),"TotalCopepods_Regimes_spring.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Summer2022.RDS"))
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

#ggsave(path= here::here("output"),"TotalCopepods_Regimes_Summer.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Fall2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalCopepods_Regimes_Fall.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalCopepds_Regimes_Winter2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalCopepods_Regimes_Winter.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Spring2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalZoopl_Regimes_Spring.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Summer2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalZoopl_Regimes_Summer.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Fall2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalZoopl_Regimes_Fall.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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
#saveRDS(Regime[["cptable"]],file = here::here("output", "TotalZoopl_Regimes_Winter2022.RDS"))
#printcp(Regime)


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

#ggsave(path= here::here("output"),"TotalZoopl_Regimes_Winter.jpg", width = 8, height = 3.75, units = "in", dpi = 300)


#Regime shifts in surface temp (run RelConditionEPU_spring.R first for spring condition)
#Surface Temp for plot in regime shift paper in SingleSpecies_ConditionPlot.R:
Surfdata <- cond.epu %>% dplyr::filter(YEAR >= 1992) %>%
  dplyr::select(YEAR, SURFTEMP) %>%
  dplyr::filter(!is.na(SURFTEMP)) %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarize(AvgSurfTemp = mean(SURFTEMP)) 

#Surface data for mackerel ESP:
#readr::write_csv(Surfdata, here::here(out.dir,"NEFSC_SpringSurfaceTemp2022.csv"))

#Regime analysis:
SurfRegime <- Surfdata %>% dplyr::select(AvgSurfTemp, YEAR)
Regime <- rpart::rpart(AvgSurfTemp~YEAR, data=SurfRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
#saveRDS(Regime[["cptable"]],file = here::here("output", "SurfaceTemp_Spring.RDS"))
#printcp(Regime)


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
  labs(title= "Average Spring Surface Temperature", y = "Temperature °C") +
  geom_vline(xintercept=SppSplit1, color='red')+
  geom_vline(xintercept=SppSplit2, color='red')+
  geom_vline(xintercept=SppSplit3, color='red')+
  geom_vline(xintercept=SppSplit4, color='red')+
  geom_vline(xintercept=SppSplit5, color='red')

ggsave(path= here::here("output"),"SurfaceTemp_Spring_Regimes2023_Year.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
