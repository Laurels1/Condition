library(ggplot2)
library(dplyr)


out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Removed MAB values in 2017 due to low sampling coverage:
annualCondition <- ButtCond 
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

#Labeling legend title not working:
p2 <- ggplot(speciesNames, aes(x = YEAR, y = MeanCond)) +
    geom_line()+
    geom_point() +
    labs(title="Butterfish Relative Condition", y = "Relative Condition") +
    geom_vline(xintercept=ButtSplit1, color='red')+
    geom_vline(xintercept=ButtSplit2, color='red')

ggsave(path= here::here(out.dir),"Butterfish_ShelfCondition_allsex_2021_viridis.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
