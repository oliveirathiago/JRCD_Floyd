#########################
### TIME SERIES PLOTS ###
#########################

## Thiago R. Oliveira

#########################

## Load packages
library(tidyverse)

## Looad clean data with factor scores
scores <- readRDS("data/scores.rds")

# Prepare data for plot
time.series <-
  scores %>%
  group_by(wave) %>%
  summarise('Procedural Justice' = mean(pj, na.rm = T),
            'Distributive Justice' = mean(dj, na.rm = T),
            'Bounded Authority' = mean(bound, na.rm = T),
            'Respect for Authority Boundaries' = mean(boundpos, na.rm = T),
            'Normative Alignment: Police' = mean(pona, na.rm = T),
            'Duty to Obey: Police' = mean(pooblig, na.rm = T),
            'Normative Alignment: Law' = mean(lena, na.rm = T),
            'Duty to Obey: Law' = mean(leoblig, na.rm = T),
            'Collective Efficacy' = mean(colleff, na.rm = T),
            'Trust in Science 1' = mean(coscience, na.rm = T),
            'Trust in Science (z-standardized)' = mean(sciencez, na.rm = T),
            'Identification with Healthcare Workers' = mean(idhealth, na.rm = T),
            'Identification with Police' = mean(idpol, na.rm = T)) %>%
  pivot_longer(cols = 'Procedural Justice':'Identification with Police') %>%
  mutate(wave = case_when(
    wave == 'w1' ~ 'T1',
    wave == 'w2' ~ 'T2',
    wave == 'w3' ~ 'T3',
    TRUE ~ wave
  ))

# Prepare data by state
time.series_state <-
  scores %>%
  drop_na(statew1) %>%
  group_by(wave, statew1) %>%
  summarise('Procedural Justice' = mean(pj, na.rm = T),
            'Distributive Justice' = mean(dj, na.rm = T),
            'Bounded Authority' = mean(bound, na.rm = T),
            'Respect for Authority Boundaries' = mean(boundpos, na.rm = T),
            'Normative Alignment: Police' = mean(pona, na.rm = T),
            'Duty to Obey: Police' = mean(pooblig, na.rm = T),
            'Normative Alignment: Law' = mean(lena, na.rm = T),
            'Duty to Obey: Law' = mean(leoblig, na.rm = T),
            'Collective Efficacy' = mean(colleff, na.rm = T),
            'Trust in Science 1' = mean(coscience, na.rm = T),
            'Trust in Science (z-standardized)' = mean(sciencez, na.rm = T),
            'Identification with Healthcare Workers' = mean(idhealth, na.rm = T),
            'Identification with Police' = mean(idpol, na.rm = T)) %>%
  pivot_longer(cols = 'Procedural Justice':'Identification with Police') %>%
  mutate(wave = case_when(
    wave == 'w1' ~ 'T1',
    wave == 'w2' ~ 'T2',
    wave == 'w3' ~ 'T3',
    TRUE ~ wave
  ))

# Plot Figure 1a
time.series.plot_police <- ggplot(time.series %>% filter(name == 'Procedural Justice' | name == 'Distributive Justice' | name == 'Respect for Authority Boundaries' |
                                                           name == "Normative Alignment: Police" | name == "Normative Alignment: Law" |
                                                           # name == "Identification with Police") %>%
                                                           name == "Duty to Obey: Police" | name == "Duty to Obey: Law") %>%
                                    mutate(name = factor(name, levels = c('Procedural Justice', 'Distributive Justice', 'Respect for Authority Boundaries',
                                                                          "Normative Alignment: Police", "Normative Alignment: Law",
                                                                          #"Identification with Police")),
                                                                          "Duty to Obey: Police", "Duty to Obey: Law"
                                    )),
                                    wave = case_when(
                                      wave == "T1" ~ "Wave 1",
                                      wave == "T2" ~ "Wave 2",
                                      wave == "T3" ~ "Wave 3",
                                    )),
                                  aes(y = value, x = wave, shape = name, linetype = name, colour = name, group = name)) + 
  geom_point(size = 3) + geom_line(size = 1.1) + 
  ylim(-.25, .2) +
  ylab("Average scores") + xlab("") +
  geom_vline(xintercept = 2.1, size = 2, linetype = 'dashed', colour = "dimgray") +
  annotate("text", x = 1.7, y = .18, label = "George Floyd's murder", angle = 0, size = 5.5, color="#3C3C3C") +
  #facet_grid(. ~ name) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 11)) +
  guides(colour = 'none') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        legend.position = "bottom",
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  scale_colour_grey(start = 0.1, end = 0.2)

# Plot Figure 1b
time.series.plot_pseudo <- ggplot(time.series %>% 
                                    filter(name == 'Collective Efficacy' | name == 'Identification with Healthcare Workers'
                                           | name == "Trust in Science (z-standardized)") %>%
                                    mutate(name = factor(name, levels = c('Collective Efficacy', 'Identification with Healthcare Workers'
                                                                          , "Trust in Science (z-standardized)" )),
                                           wave = case_when(
                                             wave == "T1" ~ "Wave 1",
                                             wave == "T2" ~ "Wave 2",
                                             wave == "T3" ~ "Wave 3",
                                           )),
                                  aes(y = value, x = wave, shape = name, linetype = name, colour = name, group = name)) + 
  geom_point(size = 3) + geom_line(size = 1.1) + 
  ylim(-.25, .2) +
  ylab("Average scores") + xlab("") +
  geom_vline(xintercept = 2.1, size = 2, linetype = 'dashed', colour = "dimgray") +
  annotate("text", x = 1.7, y = .18, label = "George Floyd's murder", angle = 0, size = 5.5, color="#3C3C3C") +
  #facet_grid(. ~ name) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 11)) +
  guides(colour = 'none') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        legend.position = "bottom",
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  scale_colour_grey(start = 0.1, end = 0.2)

# Plot Figure 1c
time.series.plot_police_bystate <- ggplot(time.series_state %>% filter(name == 'Procedural Justice' | name == 'Distributive Justice' | name == 'Respect for Authority Boundaries' |
                                                                         name == "Normative Alignment: Police" | name == "Normative Alignment: Law" |
                                                                         # name == "Identification with Police") %>%
                                                                         name == "Duty to Obey: Police" | name == "Duty to Obey: Law") %>%
                                            mutate(name = factor(name, levels = c('Procedural Justice', 'Distributive Justice', 'Respect for Authority Boundaries',
                                                                                  "Normative Alignment: Police", "Normative Alignment: Law",
                                                                                  #"Identification with Police")),
                                                                                  "Duty to Obey: Police", "Duty to Obey: Law"
                                            )),
                                            wave = case_when(
                                              wave == "T1" ~ "Wave 1",
                                              wave == "T2" ~ "Wave 2",
                                              wave == "T3" ~ "Wave 3",
                                            )),
                                          aes(y = value, x = wave, shape = name, linetype = name, colour = name, group = name)) + 
  geom_point(size = 3) + geom_line(size = 1.1) + 
  ylim(-.4, .4) +
  ylab("Average scores") + xlab("") +
  geom_vline(xintercept = 2.1, size = 2, linetype = 'dashed', colour = "dimgray") +
  annotate("text", x = 1.7, y = .18, angle = 0, size = 5.5, color="#3C3C3C"
           ,   label = ""
           #,  label = "George Floyd's murder"
  ) +
  #facet_grid(. ~ name) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 11)) +
  guides(colour = 'none') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        legend.position = "bottom",
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  scale_colour_grey(start = 0.1, end = 0.2) + 
  facet_wrap(~ statew1)

# Plot Figure 1d
time.series.plot_pseudo_bystate <- ggplot(time.series_state %>% 
                                            filter(name == 'Collective Efficacy' | name == 'Identification with Healthcare Workers'
                                                   | name == "Trust in Science (z-standardized)") %>%
                                            mutate(name = factor(name, levels = c('Collective Efficacy', 'Identification with Healthcare Workers'
                                                                                  , "Trust in Science (z-standardized)" )),
                                                   wave = case_when(
                                                     wave == "T1" ~ "Wave 1",
                                                     wave == "T2" ~ "Wave 2",
                                                     wave == "T3" ~ "Wave 3",
                                                   )),
                                          aes(y = value, x = wave, shape = name, linetype = name, colour = name, group = name)) + 
  geom_point(size = 3) + geom_line(size = 1.1) + 
  ylim(-.4, .4) +
  ylab("Average scores") + xlab("") +
  geom_vline(xintercept = 2.1, size = 2, linetype = 'dashed', colour = "dimgray") +
  annotate("text", x = 1.7, y = .18, angle = 0, size = 5.5, color="#3C3C3C"
           ,   label = ""
           #,  label = "George Floyd's murder"
  ) +
  #facet_grid(. ~ name) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 11)) +
  guides(colour = 'none') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_blank(),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        legend.position = "bottom",
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  scale_colour_grey(start = 0.1, end = 0.2) + 
  facet_wrap(~ statew1)

pdf('plots/timeseries_police.pdf', width = 10.5)
time.series.plot_police
dev.off()

pdf('plots/timeseries_pseudo.pdf', width = 10.5)
time.series.plot_pseudo
dev.off()

pdf('plots/timeseries_police_bystate.pdf', width = 10.5)
time.series.plot_police_bystate
dev.off()

pdf('plots/timeseries_pseudo_bystate.pdf', width = 10.5)
time.series.plot_pseudo_bystate
dev.off()