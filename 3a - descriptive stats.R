#########################
### DESCRIPTIVE STATS ###
#########################

## Thiago R. Oliveira

#########################

## Load packages
library(tidyverse)
library(haven)

## Load clean dataset
data <- readRDS("data/data.rds")

demos <-
  data %>%
  drop_na(statew1) %>%
  mutate(age = as.numeric(agew1),
         usborn = usbornw1 == 1,
         less_HS = educw1 == 1,
         high_school = educw1 == 2,
         bsc = educw1 == 3,
         postgrad = educw1 == 4,
         republican = partyw1 == 1,
         democrat = partyw1 == 2,
         independent = partyw1 == 3,
         liberals = case_when(
           polideolw1 == 1 ~ TRUE,
           polideolw1 == 2 ~ TRUE,
           TRUE ~ FALSE
         ),
         votetrump_dummy = votetrumpw1 != 1) %>%
  mutate(age = case_when(
    age > 100 ~ NA_real_,
    TRUE ~ age
  ))

demo_state <-
  demos %>%
  group_by(statew1) %>%
  summarise('Age' = mean(age, na.rm = T),
            'US born' = mean(usborn, na.rm = T),
            'White' = mean(whitew1, na.rm = T),
            'Black' = mean(blackw1, na.rm = T),
            'Hispanic' = mean(hispw1, na.rm = T),
            'Asian' = mean(asianw1, na.rm = T),
            'Less than high school' = mean(less_HS, na.rm = T),
            'High school or equivalent' = mean(high_school, na.rm = T),
            'Bachelors degree' = mean(bsc, na.rm = T),
            'Masters or above' = mean(postgrad, na.rm = T),
            'Republicans' = mean(republican, na.rm = T),
            'Democrats' = mean(democrat, na.rm = T),
            'Independent' = mean(independent, na.rm = T),
            'Trump supporters' = mean(votetrump_dummy, na.rm = T)) %>%
  add_row(statew1 = 'Full sample',
          Age = mean(demos$age, na.rm = T),
          'US born' = mean(demos$usborn, na.rm = T),
          'White' = mean(demos$whitew1, na.rm = T),
          'Black' = mean(demos$blackw1, na.rm = T),
          'Hispanic' = mean(demos$hispw1, na.rm = T),
          'Asian' = mean(demos$asianw1, na.rm = T),
          'Less than high school' = mean(demos$less_HS, na.rm = T),
          'High school or equivalent' = mean(demos$high_school, na.rm = T),
          'Bachelors degree' = mean(demos$bsc, na.rm = T),
          'Masters or above' = mean(demos$postgrad, na.rm = T),
          'Republicans' = mean(demos$republican, na.rm = T),
          'Democrats' = mean(demos$democrat, na.rm = T),
          'Independent' = mean(demos$independent, na.rm = T),
          'Trump supporters' = mean(demos$votetrump_dummy, na.rm = T))

# Create Table 1
write.csv(demo_state, file = 'tables/demo_state.csv')
