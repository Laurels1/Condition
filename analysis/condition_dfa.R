# seasonal_dfa
library(MARSS)
library(tidyr)
# library(bayesdfa)
library(patchwork)
library(ggplot2)
library(dplyr)
library(future)
# library(rstan)
source(here::here("R/helper_functions.r"))

# ## Use 75% of the cores on the system but never more than four
# options(parallelly.availableCores.custom = function() {
#   ncores <- max(parallel::detectCores(), 1L, na.rm = TRUE)
#   ncores <- min(as.integer(0.75 * ncores), 4L)
#   max(1L, ncores)
# })
# message(paste("Number of cores available:", availableCores()))

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

## First check out the base models without covariates
epu_long <- cond_epu %>% 
  group_by(EPU, common_name) %>%
  filter(!all(is.na(cond))) %>%
  ungroup() %>% 
  nest(data = -EPU) %>%
  left_join(expand_grid(EPU = c("SS", "GB", "GOM", "MAB"), 
                        m = 1:3,
                        R = c("diagonal and unequal", "diagonal and equal", "unconstrained" ),
                        covariate = "none"),
            by = "EPU")


dfa_out <- epu_long %>%
  mutate(
    mod = purrr::pmap(.l = list(data, m, R, covariate),
                      .f = function(data, m, R, covariate) possibly_dfa_mod(dat = data, m = m, R = R, covariate = covariate,
                                                                            just_testing = TRUE,
                                                                            data_wide = FALSE)),
    AICc = purrr::map(mod, "AICc")) %>%
  arrange(AICc)

saveRDS(dfa_out, file = "analysis/condition_dfa_epu.rds")


# dat_time <- as.double(unique(cond_epu$YEAR))
# dat_name <- as.character(unique(cond_epu$common_name))
# 
# 
# t1 <- ggplot(dat, aes(x = year, y = MeanCond, group = name, color = name)) +
#   geom_line(show.legend = FALSE) +
#   theme_minimal() +
#   labs(title = "Condition",
#        subtitle = "n = 24 fish taxa",
#        x = "Year",
#        y = expression(z-scored~condition))
# ggsave(filename = "condition_zscore.png", t1)

dfa_out <- readRDS(here::here("analysis/condition_dfa_epu.rds"))

# dfa_out <- dfa_out_epu
best_mod <- dfa_out %>%
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

# best_mod <- best_mod[[1]]


## Add covariate matrix
cov_dat <- readRDS(here::here("data/dfa_covariates.rds")) %>% 
  mutate(value = ifelse(is.nan(value), NA, value)) %>% 
  filter(!all(is.na(value)))
           

epu_long_cov <- cond_epu %>% 
  group_by(EPU, common_name) %>%
  filter(!all(is.na(cond))) %>%
  ungroup() %>% 
  nest(data = -EPU) %>%
  left_join(table_mod, by = "EPU") %>% 
  select(-covariate,
         -AICc) %>%
  left_join(expand_grid(covariate = c("amo_2", "nao_2", "copepod_index", "zoop_biomass", "zoop_abundance"),
                        EPU = c("SS", "GB", "GOM", "MAB")),
            by = "EPU") %>% 
  mutate(cov_v = purrr::pmap(.l = list(EPU, covariate), 
                               .f = function(epu, covariate) cov_dat %>% filter(EPU == epu, 
                                                                                cov == covariate) %>% pull(value)),
         len = purrr::map(cov_v, length),
         data = ifelse(len == 0, NA, data)) %>% 
  select(-len)


dfa_out_cov <- epu_long_cov %>%
  mutate(
         mod = purrr::pmap(.l = list(data, m, R, cov_v),
                           .f = function(data, m, R, cov_v) possibly_dfa_mod(dat = data, m = m, R = R, cov_v = cov_v,
                                                                             just_testing = TRUE,
                                                                             data_wide = FALSE)),
         AICc = purrr::map(mod, "AICc")) %>%
  arrange(AICc)

saveRDS(dfa_out_cov, file = "analysis/condition_dfa_epu_cov.rds")


plot_tibble <- best_mod %>% 
  mutate(

         fitted.ytT = purrr::map(.x = mod, function(x) autoplot.marssMLE(x,
                                                                  plot.type = "fitted.ytT",
                                                                  form = "dfa",
                                                                  rotate = TRUE,
                                                                  silent = TRUE)),
    dfa_plot = purrr::pmap(.l = list(object = mod, EPU = EPU),
                           .f = function(object, EPU) dfa_plot(object = object, EPU = EPU))) %>% 
  select(-mod,
         -data)



dfa_plot <- function(object, EPU = NULL) {
  
  marss_obj <- MARSSparamCIs(object)
  
  name_code <- rownames(object$marss$data)
  m <- nrow(object$states.se)
  
  if(!is.null(EPU)){
    title_name <- switch(EPU, 
                         GOM = "Gulf of Maine",
                         MAB = "Mid-Atlantic Bight",
                         SS = "Scotian Shelf",
                         GB = "Georges Bank")
  }
  if(is.null(EPU)){
    title_name = ""
  }
  
  # the rotation matrix for the Z
  z <- coef(marss_obj, type = "Z")
  H.inv <- varimax(z)$rotmat
  
  # Get the Z, upZ, lowZ
  z.low <- coef(marss_obj, type = "Z", what="par.lowCI")
  z.up <- coef(marss_obj, type = "Z", what="par.upCI")
  
  z.rot <- z %*% H.inv
  z.rot.up <- z.up %*% H.inv
  z.rot.low <- z.low %*% H.inv
  
  factor_df <- data.frame(name_code = rep(name_code, m),
                   trend = rep(paste0("Trend ", 1:m), each = length(name_code)),
                   Z = as.vector(z),
                   Zup = as.vector(z.up),
                   Zlow = as.vector(z.low)) %>%
    mutate(spp = gsub("_",  " ", name_code),
           spp = ifelse(grepl("america?|atlant?|acadia?", spp),
                        stringr::str_to_sentence(spp),
                        spp),
           shape_id = ifelse(Zup <0 | Zlow > 0, 19, 1),
           fill_id = ifelse(Zup <0 | Zlow > 0, "black", "grey70"),
           color_id = fill_id
    ) %>% 
    arrange(desc(spp)) %>% 
    mutate(spp = factor(spp, levels = unique(spp)))
  
  
fplot <-  ggplot(data = factor_df,
         aes(x = spp,
             ymin = Zlow,
             ymax = Zup,
             y = Z)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_pointrange(aes(shape = shape_id, fill = fill_id, color = color_id), show.legend = FALSE)+ #, position = position_dodge(width = 0.5)) +
    facet_wrap(~trend) +
    coord_flip() +
    scale_shape_identity() +
    scale_fill_identity() +
    scale_color_identity() +
    labs(title = title_name,
         x = "", y = "factor loadings") +
    theme_minimal()
  
  
  trend_df <- tsSmooth(object, type = "xtT", interval = "confidence", 
                       level = .95) %>% 
    mutate(
      trend = gsub(pattern = "^X", "Trend ", .rownames),
      year = rep(as.numeric(colnames(object$marss$data)), m))
  
  
tplot <- ggplot(data = trend_df,
         aes(x = year,
             ymin = .conf.low,
             ymax = .conf.up,
             y = .estimate)) +
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_line() +
    geom_ribbon(alpha = 0.4, show.legend = FALSE)+ #, position = position_dodge(width = 0.5)) +
    facet_wrap(~ trend, ncol = m) +
    labs(x = "", y = "Estimate") +
    theme_minimal()
  
  
return(fplot/tplot)

} 


mds <- coef(best_mod$mod[[4]], type = "R") %>% 
  dist() %>%        
  abs() %>%
  # MASS::isoMDS() %>%
  # .$points %>%
  cmdscale() %>%
  data.frame()

colnames(mds) <- c("d_1", "d_2")
mds$spp <- rownames(mds)


# Plot MDS
ggplot(data = mds, aes(x = d_1, y = d_2, label = spp)) +
  geom_point() +
  geom_text_repel(min.segment.length = 0, box.padding = 0.3, max.overlaps = Inf)
  



best_mod$mod[[3]]

object <- best_mod$mod[[1]]

object$coef

class(object)

autoplot.marssMLE(best_mod$mod[[1]], plot.type = "xtT", conf.int=TRUE, conf.level=0.95)

head(df)

m <- nrow(object$states.se)

# trend_df <- fitted(object, type = "xtT", interval = "confidence", 
#             level = .95) %>% 

