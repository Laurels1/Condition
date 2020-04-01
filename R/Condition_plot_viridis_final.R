<<<<<<< HEAD
library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)

out.dir="output"

annualCondition <- condition$condGOM %>% 
    dplyr::filter(!(EPU == "MAB" & YEAR == 2017))

speciesNames <- annualCondition %>%
    dplyr::filter(sex == "F") %>%
    group_by(Species) %>% 
    mutate(scaleCond = scale(MeanCond, scale = TRUE, center = TRUE))

xs = quantile(speciesNames$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)

speciesNames <- speciesNames %>% 
    mutate(category = cut(scaleCond, breaks = xs, labels = c( "Poor Condition", 
                                                              "Below Average",
                                                              "Neutral",
                                                              "Above Average",
                                                              "Good Condition")))

sortNames <- speciesNames %>% 
    filter(YEAR <= 2014) %>%
    group_by(Species) %>% 
    summarize(total = sum(scaleCond)) %>% 
    arrange(total) %>% 
    mutate(Species = factor(Species, levels = unique(Species))) %>%
    pull(Species)

speciesNames$Species <-   factor(speciesNames$Species, levels = sortNames)

#See 5 scale colors for viridis:
#scales::show_col(viridis::viridis_pal()(5))
vir <- viridis::viridis_pal()(5)

p2 <- ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = category)) +
    geom_tile() +
    coord_equal() +
    theme_bw() +
    scale_fill_manual(values=vir) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(breaks=round(seq(min(1990), max(speciesNames$YEAR), by = 5))) +
    theme(legend.position = "right", legend.box = "vertical", legend.title = element_blank(), 
          axis.title = element_blank(), axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("GOMcondition_2019_viridis_final.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
=======
library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)

out.dir="output"

annualCondition <- condition$condGOM %>% 
    dplyr::filter(!(EPU == "MAB" & YEAR == 2017))

speciesNames <- annualCondition %>%
    dplyr::filter(sex == "F") %>%
    group_by(Species) %>% 
    mutate(scaleCond = scale(MeanCond, scale = TRUE, center = TRUE))

xs = quantile(speciesNames$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)

speciesNames <- speciesNames %>% 
    mutate(category = cut(scaleCond, breaks = xs, labels = c( "Poor Condition", 
                                                              "Below Average",
                                                              "Neutral",
                                                              "Above Average",
                                                              "Good Condition")))

sortNames <- speciesNames %>% 
    filter(YEAR <= 2014) %>%
    group_by(Species) %>% 
    summarize(total = sum(scaleCond)) %>% 
    arrange(total) %>% 
    mutate(Species = factor(Species, levels = unique(Species))) %>%
    pull(Species)

speciesNames$Species <-   factor(speciesNames$Species, levels = sortNames)

#See 5 scale colors for viridis:
#scales::show_col(viridis::viridis_pal()(5))
vir <- viridis::viridis_pal()(5)

p2 <- ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = category)) +
    geom_tile() +
    coord_equal() +
    theme_bw() +
    scale_fill_manual(values=vir) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(breaks=round(seq(min(1990), max(speciesNames$YEAR), by = 5))) +
    theme(legend.position = "right", legend.box = "vertical", legend.title = element_blank(), 
          axis.title = element_blank(), axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("GOMcondition_2019_viridis_final.jpg", width = 8, height = 3.75, units = "in", dpi = 300)
>>>>>>> 6fb5cbeb8056160f4362a9d92ffcebbc31e6ff44
