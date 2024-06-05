#########################
### REGRESSION MODELS ###
#########################

## Thiago R. Oliveira

#########################

## Load packages
library(tidyverse)
library(haven)
library(estimatr)
library(texreg)

## Load respondent-changes long dataset
data.scores <- readRDS("data/data_scores.rds")


## estimate regression models and produce tables in a word document
m.control.pj <- lm_robust(changepj ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.dj <- lm_robust(changedj ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.boundpos <- lm_robust(changeboundpos ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.pona <- lm_robust(changepona ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.pooblig <- lm_robust(changepooblig ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.lena <- lm_robust(changelena ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.leoblig <- lm_robust(changeleoblig ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Proc. Just.' = m.control.pj,
  'Distr. Just.' = m.control.dj,
  'Resp. Bounded Auth.' = m.control.boundpos,
  'Pol. Norm. Align.' = m.control.pona,
  'Pol. Duty Obey' = m.control.pooblig,
  'Law Norm. Align.' = m.control.lena,
  'Law Duty Obey' = m.control.leoblig
) %>%
  wordreg(file = "tables/results_tests.docx", 
          ci.force = F, omit.coef = "idcorrect",
          custom.coef.names = c('Intercept', 'Treatment', 'Change: police stop', 'Change: cit-init contact', 'Change: victimization'),
          custom.gof.rows = list("Fixed effects" = rep("Yes", 7),
                                 "Clustered S.E." = rep("Yes", 7),
                                 "Num. obs." = c(1418, 1418, 1419, 1416, 1418, 1419, 1415),
                                 "Num. respondents" = c(769, 769, 769, 769, 769, 769, 767)))

## smell test
m.control.idpol <- lm_robust(changeidpol ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idhealth <- lm_robust(changeidhealth ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.colleff <- lm_robust(changecolleff ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.coscience <- lm_robust(changecoscience ~ treat + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Trust in science' = m.control.coscience,
  'Collective Efficacy' = m.control.colleff,
  'Id. healthcare' = m.control.idhealth,
  'Id. police' = m.control.idpol
) %>% 
  wordreg(file = "tables/results_smell.docx", 
          #screenreg(
          ci.force = F, omit.coef = "idcorrect",
          custom.coef.names = c('Intercept', 'Treatment', 'Change: police stop', 'Change: cit-init contact', 'Change: victimization'),
          #custom.gof.names = c(NA, NA, "n", "rmse", "clust")
          custom.gof.rows = list("Fixed effects" = rep("Yes", 4),
                                 "Clustered S.E." = rep("Yes", 4),
                                 "Num. obs." = c(1412, 1419, 1418, 1398),
                                 "Num. respondents" = c(768, 769, 768, 762 )))

#####
# interacting with trump vote
m.control.votetrump_supporters.pj <- lm_robust(changepj ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.dj <- lm_robust(changedj ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.boundpos <- lm_robust(changeboundpos ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.pona <- lm_robust(changepona ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.pooblig <- lm_robust(changepooblig ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.lena <- lm_robust(changelena ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_supporters.leoblig <- lm_robust(changeleoblig ~ treat + votetrump_supporters + treat*votetrump_supporters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Proc. Just.' = m.control.votetrump_supporters.pj,
  'Distr. Just.' = m.control.votetrump_supporters.dj,
  'Resp. Bounded Auth.' = m.control.votetrump_supporters.boundpos,
  'Pol. Norm. Align.' = m.control.votetrump_supporters.pona,
  'Pol. Duty Obey' = m.control.votetrump_supporters.pooblig,
  'Law Norm. Align.' = m.control.votetrump_supporters.lena,
  'Law Duty Obey' = m.control.votetrump_supporters.leoblig
) %>%
  wordreg(file = "tables/results_tests_votetrump_ref_dummmy6-7.docx", 
          #screenreg(
          ci.force = F, omit.coef = "idcorrect",
          custom.coef.names = c('Intercept', 'Treatment', 'Trump supporters',
                                'Change: police stop', 'Change: cit-init contact', 'Change: victimization',
                                'Treatment X Trump supporters'),
          #custom.gof.names = c(NA, NA, "n", "rmse", "clust")
          custom.gof.rows = list("Fixed effects" = rep("Yes", 7),
                                 "Clustered S.E." = rep("Yes", 7),
                                 "Num. obs." = c(1418, 1418, 1419, 1416, 1418, 1419, 1415),
                                 "Num. respondents" = c(769, 769, 769, 769, 769, 769, 767)))

## reversing the ref group
data.scores <- data.scores %>% mutate(votetrump_haters = factor(votetrump_supporters, levels = c('TRUE', 'FALSE')))

m.control.votetrump_nonsupporters.pj <- lm_robust(changepj ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.dj <- lm_robust(changedj ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.boundpos <- lm_robust(changeboundpos ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.pona <- lm_robust(changepona ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.pooblig <- lm_robust(changepooblig ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.lena <- lm_robust(changelena ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.votetrump_nonsupporters.leoblig <- lm_robust(changeleoblig ~ treat + votetrump_haters + treat*votetrump_haters + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Proc. Just.' = m.control.votetrump_nonsupporters.pj,
  'Distr. Just.' = m.control.votetrump_nonsupporters.dj,
  'Resp. Bounded Auth.' = m.control.votetrump_nonsupporters.boundpos,
  'Pol. Norm. Align.' = m.control.votetrump_nonsupporters.pona,
  'Pol. Duty Obey' = m.control.votetrump_nonsupporters.pooblig,
  'Law Norm. Align.' = m.control.votetrump_nonsupporters.lena,
  'Law Duty Obey' = m.control.votetrump_nonsupporters.leoblig
) %>%
  screenreg(
    ci.force = F, omit.coef = "idcorrect",
    custom.coef.names = c('Intercept', 'Treatment', 'Trump supporters',
                          'Change: police stop', 'Change: cit-init contact', 'Change: victimization',
                          'Treatment X Trump supporters'),
    #custom.gof.names = c(NA, NA, "n", "rmse", "clust")
    custom.gof.rows = list("Fixed effects" = rep("Yes", 7),
                           "Clustered S.E." = rep("Yes", 7),
                           "Num. obs." = c(1418, 1418, 1419, 1416, 1418, 1419, 1415),
                           "Num. respondents" = c(769, 769, 769, 769, 769, 769, 767)))


######### Testing other potential moderators #########
# interacting with political ideology
m.control.ideology.pj <- lm_robust(changepj ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.dj <- lm_robust(changedj ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.boundpos <- lm_robust(changeboundpos ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.pona <- lm_robust(changepona ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.pooblig <- lm_robust(changepooblig ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.lena <- lm_robust(changelena ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.ideology.leoblig <- lm_robust(changeleoblig ~ treat + liberals + treat*liberals + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Proc. Just.' = m.control.ideology.pj,
  'Distr. Just.' = m.control.ideology.dj,
  'Resp. Bounded Auth.' = m.control.ideology.boundpos,
  'Pol. Norm. Align.' = m.control.ideology.pona,
  'Pol. Duty Obey' = m.control.ideology.pooblig,
  'Law Norm. Align.' = m.control.ideology.lena,
  'Law Duty Obey' = m.control.ideology.leoblig
) %>%
  wordreg(file = "tables/results_tests_politicalideology.docx", 
          #screenreg(
          ci.force = F, omit.coef = "idcorrect",
          custom.coef.names = c('Intercept', 'Treatment', 'Liberals', 
                                'Change: police stop', 'Change: cit-init contact', 'Change: victimization',
                                'Treatment X Liberals'),
          #custom.gof.names = c(NA, NA, "n", "rmse", "clust")
          custom.gof.rows = list("Fixed effects" = rep("Yes", 7),
                                 "Clustered S.E." = rep("Yes", 7),
                                 "Num. obs." = c(1418, 1418, 1419, 1416, 1418, 1419, 1415),
                                 "Num. respondents" = c(769, 769, 769, 769, 769, 769, 767)))

# interacting with IDPOL AT W1
m.control.idpol.pj <- lm_robust(changepj ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.dj <- lm_robust(changedj ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.boundpos <- lm_robust(changeboundpos ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.pona <- lm_robust(changepona ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.pooblig <- lm_robust(changepooblig ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.lena <- lm_robust(changelena ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
m.control.idpol.leoblig <- lm_robust(changeleoblig ~ treat + idpol_w1 + treat*idpol_w1 + changepostop + changecitstop + changevictim + factor(idcorrect), clusters = idcorrect, se_type = 'CR0', data.scores)
list(
  'Proc. Just.' = m.control.idpol.pj,
  'Distr. Just.' = m.control.idpol.dj,
  'Resp. Bounded Auth.' = m.control.idpol.boundpos,
  'Pol. Norm. Align.' = m.control.idpol.pona,
  'Pol. Duty Obey' = m.control.idpol.pooblig,
  'Law Norm. Align.' = m.control.idpol.lena,
  'Law Duty Obey' = m.control.idpol.leoblig
) %>%
  wordreg(file = "tables/results_tests_idpol.docx", 
          #screenreg(
          ci.force = F, omit.coef = "idcorrect",
          custom.coef.names = c('Intercept', 'Treatment', 'Identification with Police', 
                                'Change: police stop', 'Change: cit-init contact', 'Change: victimization',
                                'Treatment X Identification with Police'),
          #custom.gof.names = c(NA, NA, "n", "rmse", "clust")
          custom.gof.rows = list("Fixed effects" = rep("Yes", 7),
                                 "Clustered S.E." = rep("Yes", 7),
                                 "Num. obs." = c(1397, 1397, 1398, 1397, 1397, 1398, 1394),
                                 "Num. respondents" = c(757, 757, 757, 757, 757, 757, 755)))


save.image("data/regression models.RData")
