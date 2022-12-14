## I would leave out the cold pool index for now, since that is on a strata basis. 
## I would start with a temperature index (these are bottom temperature anomalies) and copepod small-large, 
## then possibly phytoplankton bloom magnitude (RangeMagnitude),
## zooplankton anomaly and total copepods.

# Harvey's data for small/large copepods, total copepods and zooplankton abundance. 
# SEASON is indicating fall fish condition,
# but the average temperature anomalies and zooplankton indices have 4 seasons each. 
# Please let me know if you have any questions.


# env_dat <- readRDS(here::here("data/FishCondition_EnvirCov_DFA.rds"))
env_dat <- read.csv(here::here("data/FishCondition_EnvirCov_DFA2022.csv"))%>% 
  filter(!is.na(EPU)) %>% 
  select(YEAR, EPU,
         bt_summer = AvgTempSummer,
         bt_fall   = AvgTempFall,
         bt_winter = AvgTempWinter,
         bt_spring = AvgTempSpring,
         copepod_index_winter = CopepodSmLgWinterEPU,
         copepod_index_spring = CopepodSmLgSpringEPU,
         copepod_index_summer = CopepodSmLgSummmerEPU,
         copepod_index_fall   = CopepodSmLgFallEPU,
         copepod_abundance_winter = TotCopWinterEPU,
         copepod_abundance_spring = TotCopSpringEPU,
         copepod_abundance_summer = TotCopSummerEPU,
         copepod_abundance_fall   = TotCopFallEPU,
         zoop_abundance_winter = ZoopAbundWinterEPU,
         zoop_abundance_spring = ZoopAbundSpringEPU,
         zoop_abundance_summer = ZoopAbundSummerEPU,
         zoop_abundance_fall   = ZoopAbundFallEPU) %>% 
  distinct(.keep_all = TRUE) %>% 
  tidyr::complete(YEAR = 1992:2021,
                  EPU = c("SS", "GB", "GOM", "MAB")) %>% 
  group_by(EPU) %>% 
  mutate_at(vars(-(1:2)), ~ replace_na(., mean(., na.rm = TRUE))) %>% ### This is wrong...
  mutate_at(vars(-(1:2)), ~ scale(., center = TRUE, scale = TRUE)) %>% 
  ungroup() %>% 
  arrange(EPU, YEAR) %>% 
  pivot_longer(cols = !EPU & !YEAR,
               names_to = "cov",
               values_to = "value")

amo_dat <- read.table(url("https://psl.noaa.gov/data/correlation/amon.us.data"), 
                  skip = 1, 
                  nrows = length(1948:2022), 
                  col.names = c("year", month.name)) %>% 
  mutate(
    across(everything(), ~ifelse(. <= -99, NA, .))
  )

nao_dat <- read.table(url("https://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii"),
                      col.names = c("year", "month", "value"))

nao <- nao_dat %>% 
  mutate(year = ifelse(month == 12, year + 1, year)) %>% 
  filter(month %in% c(12, 1, 2, 3)) %>% 
  group_by(year) %>% 
  summarize(nao = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(nao = scale(nao, center = TRUE, scale = TRUE),
         nao_1 = dplyr::lag(nao, n = 1), #c(NA, diff(nao, lag = 1))) %>% 
         nao_2 = dplyr::lag(nao, n = 2)) %>% #c(NA, NA, diff(nao, lag = 2)),
  filter(year %in% 1992:2021) %>% 
  rename(YEAR = year) %>% 
  pivot_longer(!YEAR,
               names_to = "cov",
               values_to = "value")

amo <- amo_dat %>% 
  rowwise() %>% 
  mutate(amo = mean(c_across(2:13), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(amo = scale(amo, center = TRUE, scale = TRUE),
         amo_2 = dplyr::lag(amo, n = 2), #c(NA, NA, diff(amo, difference = 2)),
         amo_1 = dplyr::lag(amo, n = 1)) %>%  #c(NA, diff(nao, difference = 1))) %>% 
  filter(year %in% 1992:2021) %>% 
  select(YEAR = year, amo, amo_1, amo_2) %>% 
  pivot_longer(!YEAR,
               names_to = "cov",
               values_to = "value")


# ggplot()+
#   geom_line(data = amo, aes(x = YEAR, y = amo)) +
#   geom_line(data = amo, aes(x = YEAR, y = amo_2)) +
#   geom_line(data = amo, aes(x = YEAR, y = amo_1))
# ggplot(amo, aes(x = year, y = amo_2))+
#   geom_line()
# 
# ggplot(nao, aes(x = year, y = nao))+
#   geom_line() +
#   geom_point()
# ggplot(nao, aes(x = year, y = nao_2))+
#   geom_line()
# 
cov_long <- bind_rows(amo, nao) %>% 
  left_join(expand.grid(YEAR = 1992:2021,
                        EPU = c("SS", "GB", "GOM", "MAB")),
            by = "YEAR") %>% 
  bind_rows(., env_dat)
 
# cov_long <- cov_dat %>% 
#   group_by(EPU) %>% 
#   # mutate_at(c("bloom_size", "bloom_length", "copepod_index", "zoop_biomass", "zoop_abundance"), ~ scale(., center = TRUE, scale = TRUE)) %>% 
#   pivot_longer(cols = c("bt_fall", "bt_summer", "bloom_size", "bloom_length", "amo", "amo_1", "amo_2", "nao", "nao_1", "nao_2", "copepod_index", "zoop_biomass", "zoop_abundance"),
#                names_to = "cov",
#                values_to = "value")
# 
 ggplot(cov_long, aes(x = YEAR, y = value, group = EPU, color = EPU)) +
  geom_point() +
  geom_line() +
  facet_wrap(~cov)


saveRDS(cov_long, file = here::here("data/dfa_covariates.rds"))

