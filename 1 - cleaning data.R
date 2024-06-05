#####################
### CLEANING DATA ###
#####################

## Thiago R. Oliveira

#####################

## Load packages
library(tidyverse)
library(haven)

## Read (protected) dataset
data <- 
  read_stata('data/raw/Survey merged_BASE.dta') %>%
  mutate(afterGF = StartDatew2 > "2020-05-25") %>%
  filter(afterGF == F | is.na(afterGF)) %>%                                   # dropping W2 observations measured after George Floyd (n = 82)
  filter(idcorrect != '5c6d8aa9701e050001338a3e' & 
           idcorrect != '5c01eaca55614800012af8af' & 
           idcorrect != '5e7e55707ef97e515c063137' & 
           idcorrect != '5949f3a2833f6d0001623eca') %>%                       # dropping 4 observations (as per Adam's email)
  dplyr::select(idcorrect, 
                agew1, statew1, usbornw1, whitew1, hispw1, blackw1, asianw1, educw1, idpo1w1, partyw1, 
                idblm1w3_v2, votetrumpw1, polideolw1, racebelief1w3_v2, racebelief2w3_v2,
                pj1w1:poinst3w1, pocoop1w1:leoblig3w1, cosciencew1, postop1w1, citstop1w1, victimw1, 
                pj1w2:poinst3w2, pocoop1w2:leoblig3w2, cosciencew2, postop1w2, citstop1w2, victimw2,
                pj1w3:poinst3w3, pocoop1w3:leoblig3w3, cosciencew3, postop1w3, citstop1w3, victimw3) %>%
  mutate_at(vars(pj1w1:cosciencew3), as.numeric) %>%
  mutate(statew1 = case_when(
    statew1 == "New York" | statew1 == "New Yoek" | statew1 == "new york" | statew1 == "New york" | statew1 == "New YorK" | statew1 == "NEW YORK" | statew1 == "New York City" | statew1 == "New. York" |
      statew1 == "ny" | statew1 == "Ny" | statew1 == "NY" | statew1 == "NYC" | statew1 == " New York" ~ 'NY',
    statew1 == "arizona" | statew1 == "Arizona" | statew1 == "ARIZONA" | statew1 == "az" | statew1 == "Az" | statew1 == "AZ" ~ 'AZ',
    statew1 == "mi" | statew1 == "Mi" | statew1 == "MI" | statew1 == "michigan" | statew1 == "Michigan" | statew1 == "MICHIGAN" ~ 'MI',
    statew1 == "texas" | statew1 == "Texas" | statew1 == "TEXAS" | statew1 == "Texas (yeehaw)" | statew1 == "Texxas" | statew1 == "tx" | statew1 == "Tx" | statew1 == "TX" ~ 'TX',
    TRUE ~ NA_character_
  ) %>% factor(levels = c('AZ', 'MI', 'NY', 'TX')))

data.long <-
  data %>%
  dplyr::select(idcorrect,
                pj1w1:cosciencew3, postop1w1, citstop1w1, victimw1, postop1w2, citstop1w2, victimw2, postop1w3, citstop1w3, victimw3) %>%
  pivot_longer(cols = c(pj1w1:cosciencew3, postop1w1, citstop1w1, victimw1, postop1w2, citstop1w2, victimw2, postop1w3, citstop1w3, victimw3),
               names_pattern = "(.*)(..)$",
               names_to = c("variable", "wave"),
               values_to = 'value') %>%
  pivot_wider(id_cols = c(idcorrect, wave),
              names_from = variable,
              values_from = value) %>%
  mutate_at(vars(pj1:coscience), as.numeric) %>%
  mutate_at(vars(postop1, citstop1, victim), list(~case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    TRUE ~ NA_real_
  ))) %>%
  left_join(data %>% dplyr::select(idcorrect, statew1, partyw1, polideolw1))

## Save datafiles
saveRDS(data, file = "data/data.rds")
saveRDS(data.long, file = "data/data_long.rds")