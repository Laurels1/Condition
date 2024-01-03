library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)
library(viridis)

out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Removed MAB values in 2017 due to low sampling coverage:
#For SOE plots:
annualCondition <- rel_condition %>% dplyr::filter(EPU == "GOM")
#annualCondition <- rel_condition %>% dplyr::filter(EPU == "GB")
#annualCondition <- rel_condition %>%
#   dplyr::filter(!(EPU == "MAB" & YEAR == 2017)) %>%
#   dplyr::filter(!(YEAR == 2017))

#change YEAR to continuous numeric for plotting function below:
annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))

speciesNames <- annualCondition %>%
    group_by(Species) %>% 
    mutate(scaleCond = scale(MeanCond, scale = TRUE, center = TRUE))

xs = quantile(speciesNames$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)

speciesNames <- speciesNames %>% 
    mutate(category = cut(scaleCond, breaks = xs, labels = c( "Poor Condition", 
                                                              "Below Average",
                                                              "Neutral",
                                                              "Above Average",
                                                              "Good Condition"), 
                          include.lowest = TRUE))

sortNames <- speciesNames %>% 
    filter(YEAR <= 2014) %>%
    group_by(Species) %>% 
    summarize(total = sum(scaleCond)) %>% 
    arrange(total) %>% 
    mutate(Species = factor(Species, levels = unique(Species))) %>%
    pull(Species)

speciesNames$Species <-   factor(speciesNames$Species, levels = sortNames)


#plot of condition
#See 5 scale colors for viridis:
vir <- viridis::viridis_pal()(5)

#Labeling legend title not working:
p2 <- ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = category)) +
    labs(fill="Quintiles of Condition") +
    geom_tile() +
    coord_equal() +
    theme_bw() +
    scale_fill_manual(values=vir) +
     guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(breaks=round(seq(min(1990), max(speciesNames$YEAR), by = 5))) +
    theme(legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          axis.title = element_blank(), axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) 

ggsave(path= here::here(out.dir),"GOM_Condition_allsex_2023.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
