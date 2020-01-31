library(ggplot2)
library(dplyr)
library(patchwork)
library(forcats)


#annualCondition <- read.csv("https://raw.githubusercontent.com/Laurels1/Condition/master/data/AnnualRelCond2018_GB.csv",
#                            stringsAsFactors = FALSE)

out.dir="output"

annualCondition <- condition$condMAB %>% 
  dplyr::filter(!(EPU == "MAB" & YEAR == 2017))

speciesNames <- annualCondition %>%
  dplyr::filter(sex == "F") %>%
  group_by(Species) %>% 
  mutate(scaleCond = scale(MeanCond, scale = TRUE, center = TRUE))

xs = quantile(speciesNames$scaleCond, seq(0,1, length.out = 6), na.rm = TRUE)

speciesNames <- speciesNames %>% 
  mutate(category = cut(scaleCond, breaks = xs, labels = c( "Poor Condition", 
                                                            "Bad",
                                                            "Neutral",
                                                            "Good",
                                                            "Good Condition")))


sortNames <- speciesNames %>% 
  filter(YEAR <= 2014) %>%
  group_by(Species) %>% 
  summarize(total = sum(scaleCond)) %>% 
  arrange(total) %>% 
  mutate(Species = factor(Species, levels = unique(Species))) %>%
  pull(Species)

speciesNames$Species <-   factor(speciesNames$Species, levels = sortNames)

#jpeg("test.MABcondition_2019.jpg", res = 200, height = 1350, width = 1450)

#p1 <-   ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = category)) +
#  geom_tile() +
#  coord_equal() +
#  scale_fill_brewer(palette = "RdBu", na.value = "black") +
#  theme_bw() +
#  theme(legend.position="bottom")

#ggsave("MABcondition_2019_test.jpg", width = 6, height = 4, units = "in", dpi = 300)

p2 <- ggplot(speciesNames, aes(x = YEAR, y = forcats::fct_rev(Species), fill = scaleCond)) +
  geom_tile() +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw() +
#  scale_fill_discrete(labels = c( "Poor Condition", 
#                                  "Bad",
#                                  "Neutral",
#                                  "Good",
#                                  "Good Condition")) +
  #scale_x_continuous(breaks=round(seq(min(1990), max(speciesNames$YEAR), by = 5))) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
 #       legend.text = element_text(labels = c("-2" = "Poor Condition", 
#                                               "0" = "Neutral",
#                                               "3" = "Good Condition")),
        axis.title = element_blank(), axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot(p2)
ggsave("MABcondition_2019_viridis.jpg", width = 6, height = 3.75, units = "in", dpi = 300)

#p1 + p2
