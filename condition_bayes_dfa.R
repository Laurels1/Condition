# remotes::install_github("fate-ewi/bayesdfa")
library(bayesdfa)
library(dplyr)
library(tidyr)
 

sp_list <- read.csv(here::here("data/species_groupings.csv")) %>% 
  select(common_name = COMNAME,
         # sci_name = SCINAME,
         SVSPP) %>% 
  distinct(.keep_all = TRUE) %>% 
  mutate(common_name = tolower(common_name))

cond_epu <- readRDS(here::here("data/condSPP_EPU.rds")) %>% 
  filter(!is.na(EPU),
         EPU %in% c("GOM", "GB", "MAB"),
         nCond > 2) %>% 
  left_join(sp_list, by = "SVSPP") %>% 
  mutate(EPU = factor(EPU, levels = c("SS", "GB", "GOM", "MAB")[2:4]),
         cond =  scale(MeanCond, scale = TRUE, center = TRUE)[,1],
         common_name = gsub("[[:space:]]", "_", common_name)) %>% 
  group_by(common_name, SVSPP) %>% 
  tidyr::complete(YEAR = 1992:2021,
                  EPU = c("SS", "GB", "GOM", "MAB")[2:4]) %>% 
  dplyr::arrange(YEAR) %>%
  ungroup() %>% 
  select(-Species,
         -MeanCond,
         -StdDevCond,
         -nCond,
         -SVSPP)

## First check out the base models without covariates
epu_long <- cond_epu %>% 
  group_by(EPU, common_name) %>%
  mutate(count = sum(!is.na(cond))) %>%
  filter(!all(is.na(cond)),
         count >= 20) %>%
  select(-count,
         time = YEAR,
         obs = cond,
         ts = common_name) %>% 
  ungroup() %>% 
  nest(data = -EPU)# %>%
  # left_join(expand_grid(EPU = c("SS", "GB", "GOM", "MAB")[2:4], 
  #                       m = c(1:4, 8, 10),
  #                       R = c("diagonal and unequal", "diagonal and equal", "unconstrained")[1:2],
  #                       covariate = "none"),
  #           by = "EPU")


options(mc.cores = parallel::detectCores()-2)

bdfa_out <- epu_long %>%
  head(1) %>% 
  mutate(
    mod = purrr::pmap(.l = list(data),
                      .f = function(data) find_dfa_trends(y = data, kmin = 1, kmax = 4,
                                                          iter = 2000,
                                                          thin = 1,
                                                          compare_normal = FALSE,
                                                          convergence_threshold = 1.1,
                                                          data_shape = "long",
                                                          variance = c("equal", "unequal"))))

tt <- find_dfa_trends(y = epu_long$data[[1]] %>% as.data.frame(), kmin = 1, kmax = 4,
                iter = 2000,
                thin = 1,
                compare_normal = FALSE,
                convergence_threshold = 1.1,
                data_shape = "long",
                variance = c("equal", "unequal"))
