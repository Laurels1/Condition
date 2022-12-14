library(MARSS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source(here::here("R/helper_functions.r"))


dfa_out <- bind_rows(readRDS(here::here("analysis/condition_dfa_epu.rds")),
                     readRDS(here::here("analysis/condition_dfa_epu_cov.rds")),
                     readRDS(here::here("analysis/condition_dfa_epu2.rds")),
                     readRDS(here::here("analysis/condition_dfa_epu3.rds")))


aicc_mod <- dfa_out %>% 
  select(-mod,
         -data) %>%
  tidyr::unnest(AICc) 

aicc_plot <- ggplot(aicc_mod, aes(x = m, y = AICc, color = covariate, shape = R)) +
  geom_point() +
  geom_line() +
  facet_wrap(~EPU, scales = "free_y", ncol = 1) +
  theme_minimal()

all_mod <- dfa_out %>% 
  select(-mod,
         -data) %>%
  tidyr::unnest(AICc) %>%
  group_by(EPU) %>% 
  arrange(AICc, .by_group = TRUE) %>% 
  slice_min(order_by = AICc)

table_mod <- dfa_out %>%
 select(-mod,
  -data) %>%
   tidyr::unnest(AICc) %>%
    group_by(EPU) %>% 
  arrange(AICc, .by_group = TRUE) %>% 
   slice_min(order_by = AICc)

 best_mod <- dfa_out %>%
  tidyr::unnest(AICc) %>%
   group_by(EPU) %>% 
    arrange(AICc, .by_group = TRUE) %>%
   slice_min(order_by = AICc)
 
 
 plot_tibble <- best_mod %>% 
   mutate(#fitted.ytT = purrr::map(.x = mod, function(x) autoplot.marssMLE(x,
                                                                          # plot.type = "fitted.ytT",
                                                                          # form = "dfa",
                                                                          # rotate = TRUE,
                                                                          # silent = TRUE)),
          dfa_plot = purrr::pmap(.l = list(object = mod, EPU = EPU),
                                 .f = function(object, EPU) dfa_plot(object = object, EPU = EPU))) %>% 
   select(-mod,
          -data)

 
 # plot_tibble$fitted.ytT[[1]]
ggsave(here::here("analysis/figures/gb_condition.png"), plot_tibble$dfa_plot[[1]])
ggsave(here::here("analysis/figures/ss_condition.png"), plot_tibble$dfa_plot[[4]])
ggsave(here::here("analysis/figures/gom_condition.png"), plot_tibble$dfa_plot[[2]])
ggsave(here::here("analysis/figures/mab_condition.png"), plot_tibble$dfa_plot[[3]])



plot_tibble <- readRDS(here::here("analysis/figures/condition_plot_tibble.rds"))

sp_list <- read.csv(here::here("data/species_groupings.csv")) %>% 
  select(common_name = COMNAME,
         # sci_name = SCINAME,
         SVSPP) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(common_name = tolower(common_name))

cond_epu <- readRDS(here::here("data/condSPP_EPU.rds")) %>% 
  filter(!is.na(EPU),
         nCond > 2) %>% 
  left_join(sp_list, by = "SVSPP") %>% 
  mutate(EPU = factor(EPU, levels = c("SS", "GB", "GOM", "MAB")),
         cond =  scale(MeanCond, scale = TRUE, center = TRUE),
         common_name = gsub("[[:space:]]", "_", common_name)) %>% 
  group_by(common_name, SVSPP) %>% 
  tidyr::complete(YEAR = 1992:2021,
                  EPU = c("SS", "GB", "GOM", "MAB")) %>% 
  dplyr::arrange(YEAR) %>%
  ungroup() %>% 
  select(-Species,
         -MeanCond,
         -StdDevCond,
         -nCond,
         -SVSPP)

cust_label <- setNames(c("Georges Bank", "Gulf of Maine", "Mid-Atlantic Bight", "Scotian Shelf"), unique(cond_epu$EPU))


raw_plot <- ggplot(cond_epu, aes(x = YEAR, y = cond, group = common_name, color = common_name)) +
  geom_line(show.legend = FALSE, alpha = 0.6) +
  geom_point(show.legend = FALSE, alpha = 0.1) +
  facet_wrap(~EPU, ncol = 1, labeller = as_labeller(cust_label)) +
  theme_minimal() +
  labs(title = "Condition",
       x = "Year",
       y = expression(z-scored~condition))

ggsave(here::here("analysis/figures/raw_condition.png"), raw_plot)

aicc_mod <- bind_rows(readRDS(here::here("analysis/condition_dfa_epu.rds")),
                      readRDS(here::here("analysis/condition_dfa_epu_cov.rds")),
                      readRDS(here::here("analysis/condition_dfa_epu2.rds"))) %>% 
  select(-mod,
         -data,
         -cov_v) %>%
  tidyr::unnest(AICc) 

head(aicc_mod)

aicc_tt <- aicc_mod %>% 
  group_by(EPU) %>% 
  mutate(min = min(AICc),
         dAICc = AICc-min,
         min_lab = ifelse(dAICc == 0, 1.5, 1)) %>% 
  ungroup() %>% 
  mutate(EPU = factor(EPU, levels = c("SS", "GOM", "GB", "MAB")))

aicc_plot <- ggplot(aicc_tt, aes(x = m, y = dAICc,color = covariate, shape = R)) +
  geom_point(aes(size = min_lab)) +
  geom_line() +
  scale_size(guide = "none") +
  facet_wrap(~EPU, scales = "free_y", ncol = 1, labeller = as_labeller(cust_label)) +
  labs(x = "m (number of latent trends)",
       y = expression(Delta*AICc)) +
  theme_minimal()

aicc_plot

ggsave(here::here("analysis/figures/aicc_condition.png"), aicc_plot)

loadings_tibble <- best_mod %>% 
  mutate(loadings = purrr::pmap(.l = list(object = mod),
                                .f = function(object) loadings_df(object = object))) %>% 
  select(-mod,
         -data,
         -cov_v) %>% 
  unnest(cols = loadings) %>% 
  select(EPU, spp, trend, Z, Zup, Zlow) %>% 
  group_by(EPU, trend) %>%
  mutate(loadings = case_when(Z < 0.2 & Zlow > 0  ~ "positive_weak",
                              Zlow > 0 ~  "positive_sig",
                              # TRUE ~ NA_character_),
                              # negative = case_when(Z > -0.2 & Zup < 0 ~ "negative_weak",
                              Z > -0.2 & Zup < 0 ~ "negative_weak",
                              Zup < 0 ~ "negative_sig",
                              TRUE ~ NA_character_))
         

spp_loadings <- loadings_tibble %>% 
  filter(!is.na(loadings)) %>% 
  select(-Z, -Zup, -Zlow) %>% 
  group_by(EPU, trend, loadings) %>%
  arrange(desc(spp)) %>%
  nest() %>% 
  mutate(spp_list = purrr::map(data, ~paste(.$spp, collapse = ", ")),
         spp_list = ifelse(stringr::str_count(spp_list, ",") > 1,
                           sub(",([^,]*)$", ", and\\1", spp_list),
                           sub(",([^,]*)$", " and\\1", spp_list))) %>% 
  select(-data)

saveRDS(spp_loadings, file = here::here("analysis/condition_spp_loadings.rds"))



 
 