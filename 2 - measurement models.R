##########################
### MEASUREMENT MODELS ###
##########################

## Thiago R. Oliveira

##########################

## Load packages
library(tidyverse)
library(haven)
library(lavaan)

## Load clean datasets
data <- readRDS("data/data.rds")
data.long <- readRDS("data/data_long.rds")

##################
### CFA MODELS ###
##################

## cfa: procedural justice
cfa.pj <- 
  'pj =~ pj1 + pj2 + pj3' %>%
  cfa(data.long, missing = "ML")
pj.scores <- lavPredict(cfa.pj)

## cfa: distributive justice
cfa.dj <- 
  'dj =~ dj1 + dj2 + dj3 + dj4 + dj5' %>%
  cfa(data.long, missing = "ML")
dj.scores <- lavPredict(cfa.dj)

## cfa: bounded authority
cfa.bound <- 
  'bound =~ bound1 + bound2 + bound3 + bound4 + bound5 + bound6 + bound7' %>%
  cfa(data.long, missing = "ML")
bound.scores <- lavPredict(cfa.bound)

## cfa: normative alignment with the police
cfa.pona <- 
  'pona =~ pona1 + pona2 + pona3' %>%
  cfa(data.long, missing = "ML")
pona.scores <- lavPredict(cfa.pona)

## cfa: duty to obey the police
cfa.pooblig <- 
  'pooblig =~ pooblig1 + pooblig2 + pooblig3' %>%
  cfa(data.long, missing = "ML")
pooblig.scores <- lavPredict(cfa.pooblig)

## cfa: normative alignment with the law
cfa.lena <- 
  'lena =~ lena1 + lena2 + lena3' %>%
  cfa(data.long, missing = "ML")
lena.scores <- lavPredict(cfa.lena)

## cfa: duty to obey the law
cfa.leoblig <- 
  'leoblig =~ leoblig1 + leoblig2 + leoblig3' %>%
  cfa(data.long, missing = "ML")
leoblig.scores <- lavPredict(cfa.leoblig)

## cfa: identification with police
cfa.idpol <-
  'idpol =~ idpo1 + idpo2 + idpo3' %>%
  cfa(data.long, missing = 'ML')
idpol.scores <- lavPredict(cfa.idpol)

## cfa: identification with healthcare workers
cfa.idhealth <-
  'idhealth =~ idhealth1 + idhealth2 + idhealth3' %>%
  cfa(data.long, missing = 'ML')
idhealth.scores <- lavPredict(cfa.idhealth)

## cfa: collective efficacy
cfa.colleff <-
  'colleff =~ colleff1 + colleff2 + colleff3 + colleff4' %>%
  cfa(data.long, missing = 'ML')
colleff.scores <- lavPredict(cfa.colleff)

############################
### data with CFA scores ###
############################

## attach factor scores to dataset
scores <-
  data.long %>%
  dplyr::select(idcorrect, wave, statew1, partyw1, polideolw1,
                coscience, postop1:victim) %>%
  bind_cols(pj.scores) %>%
  bind_cols(dj.scores) %>%
  bind_cols(bound.scores) %>%
  mutate(boundpos = -bound) %>%
  bind_cols(pona.scores) %>%
  bind_cols(pooblig.scores) %>%
  bind_cols(pofear.scores) %>%
  bind_cols(lena.scores) %>%
  bind_cols(leoblig.scores) %>%
  bind_cols(idpol.scores) %>%
  bind_cols(idhealth.scores) %>%
  bind_cols(colleff.scores) %>%
  mutate(sciencez = coscience - mean(coscience, na.rm = T))

## calculate change scores and create respondent-change long dataset
data.scores <-
  scores %>%
  pivot_wider(
    id_cols = c(idcorrect),
    names_from = wave,
    values_from = coscience:colleff
  ) %>%
  mutate(
    changepj0 = pj_w2 - pj_w1,
    changedj0 = dj_w2 - dj_w1,
    changebound0 = bound_w2 - bound_w1,
    changeboundpos0 = boundpos_w2 - boundpos_w1,
    changepona0 = pona_w2 - pona_w1,
    changepooblig0 = pooblig_w2 - pooblig_w1,
    changepofear0 = pofear_w2 - pofear_w1,
    changelena0 = lena_w2 - lena_w1,
    changeleoblig0 = leoblig_w2 - leoblig_w1,
    changeidpol0 = idpol_w2 - idpol_w1,
    changeidhealth0 = idhealth_w2 - idhealth_w1,
    changecolleff0 = colleff_w2 - colleff_w1,
    changecoscience0 = coscience_w2 - coscience_w1,
    changepostop0 = postop1_w2 - postop1_w1,
    changecitstop0 = citstop1_w2 - citstop1_w1,
    changevictim0 = victim_w2 - victim_w1,
    #
    changepj1 = pj_w3 - pj_w2,
    changedj1 = dj_w3 - dj_w2,
    changebound1 = bound_w3 - bound_w2,
    changeboundpos1 = boundpos_w3 - boundpos_w2,
    changepona1 = pona_w3 - pona_w2,
    changepooblig1 = pooblig_w3 - pooblig_w2,
    changepofear1 = pofear_w3 - pofear_w2,
    changelena1 = lena_w3 - lena_w2,
    changeleoblig1 = leoblig_w3 - leoblig_w2,
    changeidpol1 = idpol_w3 - idpol_w2,
    changeidhealth1 = idhealth_w3 - idhealth_w2,
    changecolleff1 = colleff_w3 - colleff_w1,
    changecoscience1 = coscience_w3 - coscience_w2,
    changepostop1 = postop1_w3 - postop1_w2,
    changecitstop1 = citstop1_w3 - citstop1_w2,
    changevictim1 = victim_w3 - victim_w2
  ) %>%
  dplyr::select(idcorrect, changepj0:changevictim1) %>%
  pivot_longer(
    cols = changepj0:changevictim1,
    names_pattern = "(.*)(.)$",
    names_to = c("variable", "treat"),
    values_to = 'value'
  ) %>%
  pivot_wider(
    id_cols = c(idcorrect, treat),
    names_from = variable,
    values_from = value
  ) %>%
  left_join(scores %>% 
              pivot_wider(
                id_cols = c(idcorrect),
                names_from = wave,
                values_from = coscience:colleff
              ) %>%
              dplyr::select(idcorrect, idpol_w1)
  ) %>% 
  left_join(data %>%
              dplyr::select(idcorrect, 
                            agew1, statew1, usbornw1, whitew1, hispw1, blackw1, asianw1, educw1, partyw1, votetrumpw1, polideolw1)
            , by = "idcorrect") %>%
  mutate(republicans = partyw1 == 1, 
         liberals = case_when(
           polideolw1 == 1 ~ TRUE,
           polideolw1 == 2 ~ TRUE,
           TRUE ~ FALSE
         ),
         votetrump_supporters = case_when(
           votetrumpw1 == 6 ~ TRUE,
           votetrumpw1 == 7 ~ TRUE,
           TRUE ~ FALSE)
         )

## Save respondent-changes long dataset
saveRDS(scores, file = "data/scores.rds")
saveRDS(data.scores, file = "data/data_scores.rds")
