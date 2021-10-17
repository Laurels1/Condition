library(ggplot2)
library(dplyr)


out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Removed MAB values in 2017 due to low sampling coverage:
annualCondition <- ButtCondPlot 
#%>% 
 #   dplyr::filter(!(EPU == "MAB" & YEAR == 2017)) 
 #   dplyr::filter(!(YEAR == 2017)) 

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
    labs(title="Butterfish Relative Condition", y = "Relative Condition") +
    geom_vline(xintercept=ButtSplit1, color='red')+
    geom_vline(xintercept=ButtSplit2, color='red')

ggsave(path= here::here(out.dir),"Butterfish_ShelfCondition_allsex_2021_viridis.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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

ggsave(path= here::here(out.dir),"AverageSummerBottomTempEPU.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

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

ggsave(path= here::here(out.dir),"CopepodSmLgEPU_regime.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

#Fall phyoplanktion bloom magnitude data (from gam_calcs_strata.R):
FallBloomdata <- Fallbloom %>% dplyr::filter(YEAR >= 1992) %>%
    dplyr::select(YEAR, RangeMagnitude)

#Line plot of summer temp:
p2 <- ggplot(FallBloomdata, aes(x = YEAR, y = RangeMagnitude)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title="Fall Phytoplankton Bloom Magnitude", y = "Fall Bloom Magnitude")

ggsave(path= here::here(out.dir),"FallBloom_regime.jpg", width = 8, height = 3.75, units = "in", dpi = 300)

