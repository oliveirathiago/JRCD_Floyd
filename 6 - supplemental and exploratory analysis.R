library(tidyverse)
library(haven)
library(broom)
options(scipen=999)

data <- readRDS("data/data.rds")

table4_data <-
  data %>%
  mutate(votetrump_supporters = case_when(
    votetrumpw1 == 6 ~ TRUE,
    votetrumpw1 == 7 ~ TRUE,
    TRUE ~ FALSE),
    racialized_beliefs = (racebelief1w3_v2 + racebelief2w3_v2) / 2,
    AZ = statew1 == "AZ",
    MI = statew1 == "MI",
    NY = statew1 == "NY",
    TX = statew1 == "TX",
    less_than_HS = educw1 == 1,
    HS = educw1 == 2,
    Bachelors = educw1 == 3,
    Postgrad = educw1 == 4
    ) %>%
  mutate(across(c(whitew1:asianw1, AZ:Postgrad), as.numeric))
  

group_by(votetrump_supporters) %>%
  summarise(racialized_beliefs = mean(racialized_beliefs, na.rm = T),
            id_police = mean(idpo1w1, na.rm = T),
            id_blm = mean(idblm1w3_v2, na.rm = T),
            AZ = mean(statew1 == "AZ", na.rm = T),
            MI = mean(statew1 == "MI", na.rm = T),
            NY = mean(statew1 == "NY", na.rm = T),
            TX = mean(statew1 == "TX", na.rm = T),
            White = mean(whitew1, na.rm = T),
            Hispanic = mean(hispw1, na.rm = T),
            Black = mean(blackw1, na.rm = T),
            Asian = mean(asianw1, na.rm = T),
            less_than_HS = mean(educw1 == 1, na.rm = T),
            HS = mean(educw1 == 2, na.rm = T),
            Bachelors = mean(educw1 == 3, na.rm = T),
            Postgrad = mean(educw1 == 4, na.rm = T)
  )


compute_stat <- function(data, outcome_var, group_var) {
  # t-test assuming normality
  t_test <- t.test(data[[outcome_var]] ~ data[[group_var]])
  
  # Return results
  return(list(
    mean_group1 = t_test$estimate[2],
    mean_group0 = t_test$estimate[1],
    t = t_test$statistic,
    p = t_test$p.value,
    se = t_test$stderr,
    diff = diff(t_test$estimate)
  ))
}

vars <- c("racialized_beliefs", "idpo1w1", "idblm1w3_v2", "AZ", "MI", "NY", "TX", "less_than_HS", "HS", "Bachelors", "Postgrad")

table4 <- lapply(vars, function(outcome_var) {
  compute_stat(data = table4_data, outcome_var, "votetrump_supporters")
}) %>%
  bind_rows(.id = "Outcome") %>%
  mutate(cilow = diff - 1.96 * se,
         ciupp = diff + 1.96 * se,
         vars = c("Racialized beliefs about crime", "Identification with police (wave 1)", "Identification with BLM (wave 3)",
                  "Arizona", "Michigan", "New York", "Texas", "Less than high school", "High school or equivalent", 
                  "Bachelor's degree", "Masters, professional, or doctoral degree")) %>%
  mutate(across(c(mean_group1:ciupp), ~round(. ,2))) %>%
  mutate(p = case_when(
    p == 0 ~ "< 0.001",
    TRUE ~ as.character(p)
  )) %>%
  mutate(ci = paste("[", cilow, "; ", ciupp, "]", sep = "")) %>%
  dplyr::select(vars, `Intended Trump Voters ` = mean_group1, `Non-Trump Voters` = mean_group0, `t-statistic` = t, 
                `Confidence interval of mean the difference` = ci, `p-value` = p)

write.csv(table4, file = 'tables/tabl4.csv')