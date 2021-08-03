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


# COMNAME                 SCIENTIFIC_NAME SVSPPsv
# 1  ROUGHTAIL STINGRAY              DASYATIS CENTROURA       4
# 2  BLUNTNOSE STINGRAY                    DASYATIS SAY      18
# 3        BULLNOSE RAY         MYLIOBATIS FREMINVILLEI      19
# 4    LONGHORN SCULPIN MYOXOCEPHALUS OCTODECEMSPINOSUS     163
# 5          OCEAN POUT         MACROZOARCES AMERICANUS     193
# 6 SPINY BUTTERFLY RAY                GYMNURA ALTAVELA     375