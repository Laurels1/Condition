## I would leave out the cold pool index for now, since that is on a strata basis. 
## I would start with a temperature index (these are bottom temperature anomalies) and copepod small-large, 
## then possibly phytoplankton bloom magnitude (RangeMagnitude),
## zooplankton anomaly and total copepods.

env_dat <- readRDS(here::here("data/FishCondition_EnvirCov_DFA.rds")) %>% 
  filter(!is.na(EPU)) %>% 
  select(YEAR, EPU,
         # AvgTempWinter,
         # AvgTempSpring,
         bt_summer = AvgTempSummer,
         bt_fall = AvgTempFall,
         copepod_index = CopepodSmallLarge, 
         bloom_size = RangeMagnitude,
         bloom_length = RangeDuration,
         zoop_biomass = ZooplBiomassAnomaly,
         zoop_abundance = TotalCopepodsMillions) %>% 
  distinct(.keep_all = TRUE) %>% 
  tidyr::complete(YEAR = 1992:2021,
                  EPU = c("SS", "GB", "GOM", "MAB")) %>% 
  group_by(EPU) %>% 
  mutate_at(vars(3:9), ~replace_na(., mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  arrange(EPU, YEAR)


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
  rename(YEAR = year)

amo <- amo_dat %>% 
  rowwise() %>% 
  mutate(amo = mean(c_across(2:13), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(amo = scale(amo, center = TRUE, scale = TRUE),
         amo_2 = dplyr::lag(amo, n = 2), #c(NA, NA, diff(amo, difference = 2)),
         amo_1 = dplyr::lag(amo, n = 1)) %>%  #c(NA, diff(nao, difference = 1))) %>% 
  filter(year %in% 1992:2021) %>% 
  select(YEAR = year, amo, amo_1, amo_2)


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
cov_dat <- amo %>% 
  left_join(nao, by = "YEAR") %>% 
  left_join(expand.grid(YEAR = 1992:2021,
                        EPU = c("SS", "GB", "GOM", "MAB")),
            by = "YEAR") %>% 
  left_join(env_dat, by = c("EPU", "YEAR"))
 
cov_long <- cov_dat %>% 
  group_by(EPU) %>% 
  mutate_at(c("bloom_size", "bloom_length", "copepod_index", "zoop_biomass", "zoop_abundance"), ~ scale(., center = TRUE, scale = TRUE)) %>% 
  pivot_longer(cols = c("bt_fall", "bt_summer", "bloom_size", "bloom_length", "amo", "amo_1", "amo_2", "nao", "nao_1", "nao_2", "copepod_index", "zoop_biomass", "zoop_abundance"),
               names_to = "cov",
               values_to = "value")

 # ggplot(cov_long, aes(x = YEAR, y = value, group = EPU, color = EPU)) +
 #  geom_point() +
 #  geom_line() +
 #  facet_wrap(~cov)
 # 

saveRDS(cov_long, file = here::here("data/dfa_covariates.rds"))

