#' Sample code to incorporate in to functions 

library(magrittr)
# used this as a sample to get SVSPP codes
cond <- readRDS(file=here::here("other","condSPP.rds")) %>% 
  dplyr::ungroup()

# connect to the database with your credentials
channel <- dbutils::connect_to_database("server","user")

# Gets itis codes
itis <- dbutils::create_species_lookup(channel,species=unique(cond$SVSPP),speciesType = "SVSPP")

# clean output
codes <- itis$data %>% 
  dplyr::select(SVSPPsv,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>% 
  dplyr::rename(SVSPP=SVSPPsv) %>%
  dplyr::distinct()

# find species names with missing ITIS codes
allSurveySpecies <- survdat::get_species(channel)$data
speciesWithMissingITIS <- allSurveySpecies %>%
  dplyr::filter(SCINAME %in% itis$missing$cfdbs.species_itis_ne) %>% 
  dplyr::select(SCINAME,COMNAME,SVSPP)


# These are the species
# SCINAME                         COMNAME              SVSPP
# <chr>                           <chr>                <dbl>
#   1 TORPEDO NOBILIANA               ATLANTIC TORPEDO        21
# 2 MYLIOBATIS FREMINVILLEI         BULLNOSE RAY            19
# 3 MYOXOCEPHALUS OCTODECEMSPINOSUS LONGHORN SCULPIN       163
# 4 MENTICIRRHUS AMERICANUS         SOUTHERN KINGFISH      652
# 5 MENTICIRRHUS SAXATILIS          NORTHERN KINGFISH      146
# 6 MACROZOARCES AMERICANUS         OCEAN POUT             193
# 7 LEPOPHIDIUM PROFUNDORUM         FAWN CUSK-EEL          194
# 8 GYMNURA ALTAVELA                SPINY BUTTERFLY RAY    375
# 9 GYMNURA MICRURA                 SMOOTH BUTTERFLY RAY   376
# 10 DASYATIS AMERICANA              SOUTHERN STINGRAY       29
# 11 DASYATIS CENTROURA              ROUGHTAIL STINGRAY       4
# 12 DASYATIS SAY                    BLUNTNOSE STINGRAY      18
# 13 ACIPENSER OXYRHYNCHUS           ATLANTIC STURGEON      380
# 14 ZENOPSIS CONCHIFERA             BUCKLER DORY           112