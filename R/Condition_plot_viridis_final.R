library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)
library(viridis)

out.dir="output"

#Data from RelConditionEPU.R
#No data available for 2020 due to Covid-19
#Removed MAB values in 2017 due to low sampling coverage:
# for use in butterfish plots:
#annualCondition <- condNSpp
#Condition full shelf
annualCondition <- condNshelfSpp
#For SOE plots:
#annualCondition <- condGOM
annualCondition <- condGB
annualCondition <- condMAB %>%
   dplyr::filter(!(EPU == "MAB" & YEAR == 2017)) %>%
   dplyr::filter(!(YEAR == 2017))

#change YEAR to continuous numeric for plotting function below:
annualCondition$YEAR <- as.numeric(as.character(annualCondition$YEAR))

speciesNames <- annualCondition %>%
#    dplyr::filter(sexMF == "F") %>%
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

#Adding regime shift lines:
#Regime analysis:
CondRegime <- speciesNames %>% dplyr::select(MeanCond, YEAR)
Regime <- rpart::rpart(MeanCond~YEAR, data=CondRegime)
#Selecting best fit (gives optimal CP value associated with the minimum error)::
# Regime$cptable[which.min(Regime$cptable[,"xerror"]),"CP"]

SppPlot <- rpart.plot::rpart.plot(Regime)

#Outputs pruning tree table:
saveRDS(Regime[["cptable"]],file = here::here("output", "Cond_Shelf_Regimes_2022.RDS"))
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


AnnualCondRegime <- CondRegime 

#change YEAR to continuous numeric for plotting function below:
AnnualCondRegime$YEAR <- as.numeric(as.character(AnnualCondRegime$YEAR))

ShelfCondRegime <- AnnualCondRegime

#plot of condition with regime lines
#See 5 scale colors for viridis:
#scales::show_col(viridis::viridis_pal()(5))
vir <- viridis::viridis_pal()(5)

#Labeling legend title not working:
p2 <- ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = category)) +
    labs(fill="Quintiles of Condition") +
    geom_tile() +
    coord_equal() +
    theme_bw() +
    scale_fill_manual(values=vir) +
     guides(fill = guide_legend(reverse = TRUE)) +
#      geom_vline(xintercept=SppSplit1, color='red')+
#      geom_vline(xintercept=SppSplit2, color='red')+
    # geom_vline(xintercept=SppSplit3, color='red')+
    #scale_x_discrete works if don't need to pad final year for missing data. Changed Year to numeric above and this works:
    scale_x_continuous(breaks=round(seq(min(1990), max(speciesNames$YEAR), by = 5))) +
    theme(legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          axis.title = element_blank(), axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) #+
#      geom_vline(xintercept=SppSplit1, color='red', size = 1.2)+
#      geom_vline(xintercept=SppSplit2, color='red', size = 1.2)
#         geom_vline(xintercept=SppSplit3, color='red', size = 1.2)

ggsave(path= here::here(out.dir),"Shelf_Condition_allsex_2022.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
