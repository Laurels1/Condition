# seasonal_dfa
library(MARSS)
library(tidyr)
# library(bayesdfa)
library(ggplot2)
library(dplyr)
library(future)
# library(rstan)
source("R/helper_functions.r")

# condition_dat <- readRDS(here::here("data/condSPP.rds")) %>%
#   select(year = YEAR,
#          epu = EPU,
#          condition =)

possibly_dfa_mod <- purrr::possibly(dfa_mod, otherwise = NA_character_)

## Load in data
# cond_year <- readRDS(here::here("data/condSPP_Year.rds"))

# githubURL <- "https://github.com/NOAA-EDAB/ecodata/raw/master/data/species_groupings.rda"
# download.file(githubURL,"myfile")
# load("myfile")
# write.csv(x = species_groupings, file = "data/species_groupings.csv", row.names = FALSE)


sp_list <- read.csv("data/species_groupings.csv") %>% 
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

epu_long <- cond_epu %>% 
  group_by(EPU, common_name) %>%
  filter(!all(is.na(cond))) %>%
  ungroup() %>% 
  nest(data = -EPU) %>%
  left_join(expand_grid(EPU = c("SS", "GB", "GOM", "MAB"), 
                        m = 1:2,
                        R = c("diagonal and unequal", "diagonal and equal", "unconstrained" )[1:2],
                        covariate = "none"),
            by = "EPU")


# plan(multisession, workers = parallelly::availableCores() -1)
plan(multisession, workers = 4)
dfa_out_epu <- epu_long %>%
  # group_by(EPU) %>%
  # head(2) %>% 
  mutate(mod = furrr::future_pmap(.l = list(data, m, R, covariate),
                                  .f = function(data, m, R, covariate) dfa_mod(dat = data, m = m, R = R, covariate = covariate,
                                                                               just_testing = TRUE,
                                                                               data_wide = FALSE)),
         AICc = purrr::map(mod, "AICc")) %>%
  arrange(AICc)

plan(sequential)

saveRDS(dfa_out_epu, file = "analysis/condition_dfa_epu.rds")

# epu_wide <- cond_epu %>% 
#   tidyr::pivot_wider(names_from = YEAR, values_from = cond) %>%
#   nest(data = -c(EPU)) %>%
#   mutate(data = purrr::map(data, ~  tibble::column_to_rownames(.data = .x, var = "common_name")),
#          data = purrr::map(data, ~  as.matrix(x = .x)))


dat_time <- as.double(unique(cond_epu$YEAR))
dat_name <- as.character(unique(cond_epu$common_name))


# tt <- cond_epu %>% 
#   nest_by(EPU) %>% 
#   left_join(expand_grid(EPU = c("SS", "GB", "GOM", "MAB"), 
#                         m = 1:2,
#                         R = c("diagonal and unequal", "diagonal and equal", "unconstrained" )[1:2],
#                         covariate = "none"))
#   

# 
# dfa_mat_epu <- epu_wide %>% 
#   left_join(expand_grid(EPU = c("SS", "GB", "GOM", "MAB"), 
#                            m = 1:2,
#                            R = c("diagonal and unequal", "diagonal and equal", "unconstrained" )[1:2],
#                            covariate = "none"))


# plan(multisession, workers = parallelly::availableCores() -1)
dfa_out_epu <- dfa_mat_epu %>%
  # group_by(EPU) %>%
  # mutate(dat = purrr::map(EPU, function(x) epu_wide %>% filter(EPU == .))) %>% 
  # unnest(cols = dat)
  # group_by(epu) %>% 
  head(1) %>%
  mutate(mod = furrr::future_pmap(.l = list(data, m, R, covariate),
                                  # .f = function(data, m, R, covariate) dfa_mod(dat = data, m = m, R = R, covariate = covariate, just_testing = TRUE))
                                  .f = function(data, m, R, covariate) paste0(dat = data, m = m, R = R, covariate = covariate, just_testing = TRUE)),
         AICc = purrr::map(mod, "AICc")) %>%
  arrange(AICc)

plan(sequential)



ggplot(cond_epu, aes(x = YEAR, y = cond, color = common_name)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~EPU, ncol = 1)
# 
# # load(here::here("data/fishcondition.rda"))
# dat <- fishCondition %>% 
#   dplyr::mutate(name = gsub(" ", "_", tolower(Species))) %>% 
#   dplyr::group_by(name, sex, YEAR) %>% 
#   dplyr::summarize(MeanCond = mean(RelCond),
#                    nCond = n()) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::filter(nCond > 2,
#                 sex == "F") %>% 
#   dplyr::select(year = YEAR,
#                 MeanCond,
#                 name) %>% 
#   dplyr::group_by(name) %>% 
#   dplyr::mutate(MeanCond = scale(MeanCond, scale = TRUE, center = TRUE)) %>% 
#   tidyr::complete(year = tidyr::full_seq(year, 1)) %>% 
#   dplyr::arrange(year)
# 
# temp_wide <- dat %>% 
#   tidyr::pivot_wider(names_from = year, values_from = MeanCond) %>%
#   # tidyr::pivot_wider(values_from = MeanCond) %>% 
#   ungroup() %>%
#   dplyr::select(-name) %>%
#   # t() %>% 
#   as.matrix()
# 
# dat_time <- as.double(unique(dat$year))
# dat_name <- as.character(unique(dat$name))
# 
# row.names(temp_wide) <- dat_name



t1 <- ggplot(dat, aes(x = year, y = MeanCond, group = name, color = name)) +
  geom_line(show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Condition",
       subtitle = "n = 24 fish taxa",
       x = "Year",
       y = expression(z-scored~condition))
ggsave(filename = "condition_zscore.png", t1)

dfa_mat <- expand_grid(m = 1:2,#1:nrow(dat),
                       R = c("diagonal and unequal", "diagonal and equal", "unconstrained" )[1:2],
                       covariate = "none")

possibly_dfa_mod <- purrr::possibly(dfa_mod, otherwise = NA_character_)

plan(multisession, workers = parallelly::availableCores() -1)
dfa_out <- dfa_mat %>%
  # head(1) %>%
  mutate(mod = furrr::future_pmap(.l = list(m, R, covariate), 
                                  .f = function(m, R, covariate) possibly_dfa_mod(m = m, R = R, covariate = covariate, just_testing = TRUE)),
         AICc = purrr::map(mod, "AICc")) %>%
  arrange(AICc)

plan(sequential)

dfa_out <- dfa_out_epu
best_mod <- dfa_out %>%
  tidyr::unnest(AICc) %>%
  arrange(AICc) %>%
  head(1) %>%
  pull(mod)


table_mod <- dfa_out %>%
  select(-mod,
         -data) %>%
  tidyr::unnest(AICc) %>%
  arrange(AICc)

best_mod <- best_mod[[1]]

###  make plot of states and CIs
# “model.ytT”, “xtT”, “model.resids”, “state.resids”, “qqplot.model.resids”, “qqplot.state.resids”, “ytT”, “acf.model.resids”
autoplot(best_mod,
         plot.type = "xtT",
         form = "dfa",
         rotate = TRUE) +
  theme_minimal() -> s_plot

ggsave(filename = here::here("dfa_condition.png"), s_plot)

### Plot of observations
autoplot(best_mod,
         plot.type = "model.ytT", #"ytT",
         form = "dfa",
         rotate = TRUE)

### Plot of residuals
autoplot(best_mod,
         plot.type = "acf.model.resids",
         form = "dfa",
         rotate = TRUE)

best_mod <- MARSSparamCIs(best_mod)
# the rotation matrix for the Z
z <- coef(best_mod, type = "Z")
H.inv <- varimax(z)$rotmat

# Get the Z, upZ, lowZ
# Z.up <- coef
z.low <- coef(best_mod, type = "Z", what="par.lowCI")
z.up <- coef(best_mod, type = "Z", what="par.upCI")

z.rot <- z %*% H.inv
z.rot.up <- z.up %*% H.inv
z.rot.low <- z.low %*% H.inv

df <- data.frame(name_code = rep(row.names(temp_wide), table_mod$m[1]),
                 trend = rep(1:table_mod$m[1], each = length(dat_name)),
                 Z = as.vector(z),
                 Zup = as.vector(z.up),
                 Zlow = as.vector(z.low)) %>%
  mutate(epu = gsub("^.*_(.*)",  "\\1", name_code),
         spp = gsub("^(.*)_.*$",  "\\1", name_code))



l_plot <- ggplot(data = df,
       aes(x = name_code,
           ymin = Zlow,
           ymax = Zup,
           y = Z,
           color = epu)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(show.legend = FALSE)+ #, position = position_dodge(width = 0.5)) +
  facet_wrap(~trend) +
  coord_flip() +
  theme_bw()



ggplot(data = df,
       aes(x = spp,
           ymin = Zlow,
           ymax = Zup,
           y = Z,
           color = as.factor(trend))) +
  geom_hline(yintercept = 0, color = "black") +
  geom_pointrange(show.legend = TRUE, position = position_dodge(width = 0.5)) +
  facet_wrap(~epu) +
  labs(color = "State",
       x = "",
       y = "factor loadings") +
  coord_flip() +
  theme_bw() -> l_plot

ggsave(filename = here::here("dfa_condition_loadings.png"), l_plot)
