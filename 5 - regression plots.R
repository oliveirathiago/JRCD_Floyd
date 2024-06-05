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

## Load image with regression models estimated
load("data/regression models.RData")

## Prepare data for main plot (Figures 2a and 2b)
data.plot <-
  tibble(
    model = c('pj', 'dj', 'boundpos', 'pona', 'pooblig', 'lena', 'leoblig', 'idhealth', 'colleff', 'coscience'),
    coef = c(coef(m.control.pj)['treat1'], coef(m.control.dj)['treat1'], coef(m.control.boundpos)['treat1'], coef(m.control.pona)['treat1'], coef(m.control.pooblig)['treat1'], 
             coef(m.control.lena)['treat1'], coef(m.control.leoblig)['treat1'], coef(m.control.idhealth)['treat1'], coef(m.control.colleff)['treat1'], coef(m.control.coscience)['treat1']),
    cilow = c(confint(m.control.pj)['treat1', 1], confint(m.control.dj)['treat1', 1], confint(m.control.boundpos)['treat1', 1], confint(m.control.pona)['treat1', 1], 
              confint(m.control.pooblig)['treat1', 1], confint(m.control.lena)['treat1', 1], confint(m.control.leoblig)['treat1', 1], 
              confint(m.control.idhealth)['treat1', 1], confint(m.control.colleff)['treat1', 1], confint(m.control.coscience)['treat1', 1]),
    ciupp = c(confint(m.control.pj)['treat1', 2], confint(m.control.dj)['treat1', 2], confint(m.control.boundpos)['treat1', 2], confint(m.control.pona)['treat1', 2], 
              confint(m.control.pooblig)['treat1', 2], confint(m.control.lena)['treat1', 2], confint(m.control.leoblig)['treat1', 2], 
              confint(m.control.idhealth)['treat1', 2], confint(m.control.colleff)['treat1', 2], confint(m.control.coscience)['treat1', 2])
  )

## Plot: Figure 2a
plot.results <- ggplot(data.plot %>% filter(model != 'idhealth' & model != 'colleff' & model != 'coscience'), aes(y = coef, x = model)) +
  geom_errorbar(aes(ymin = cilow, ymax = ciupp), width = .1, position = position_dodge(), lwd = .75, show.legend = T) + 
  ylim(-.5,.5) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') + 
  coord_flip() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2) + 
  scale_x_discrete(limits = c('leoblig', 'lena', 'pooblig', 'pona', 'boundpos', 'dj', 'pj'),
                   breaks = c('leoblig', 'lena', 'pooblig', 'pona', 'boundpos', 'dj', 'pj'),
                   labels = c('Duty to obey the law', 'Normative alignment with the law', 'Duty to obey the police', 'Normative alignment with the police',
                              'Respect for authority boundaries', 'Perceived distributive justice', 'Perceived procedural justice'))

## Plot: Figure 2b
plot.placebo <- ggplot(data.plot %>% filter(model == 'idhealth' | model == 'colleff' | model == 'coscience'), aes(y = coef, x = model)) +
  geom_errorbar(aes(ymin = cilow, ymax = ciupp), width = .1, position = position_dodge(), lwd = .75, show.legend = T) + 
  ylim(-.5,.5) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') + 
  coord_flip() + 
  ylab("") + xlab("") +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 12),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2) + 
  scale_x_discrete(limits = c('idhealth', 'coscience', 'colleff'),
                   breaks = c('idhealth', 'coscience', 'colleff'),
                   labels = c('Identification with healthcare workers',
                              'Trust in science', 'Collective efficacy'))

pdf('plots/Figure2a.pdf')
plot.results
dev.off()

pdf('plots/Figure2b.pdf')
plot.placebo
dev.off()

#################################
### Interaction plots ###

# List of models
models_list_supporters67 <- list(m.control.votetrump_supporters.pj, m.control.votetrump_supporters.dj, m.control.votetrump_supporters.boundpos,
                           m.control.votetrump_supporters.pona, m.control.votetrump_supporters.pooblig,
                           m.control.votetrump_supporters.lena, m.control.votetrump_supporters.leoblig)
models_list_nonsupporters15 <- list(m.control.votetrump_nonsupporters.pj, m.control.votetrump_nonsupporters.dj, m.control.votetrump_nonsupporters.boundpos,
                               m.control.votetrump_nonsupporters.pona, m.control.votetrump_nonsupporters.pooblig,
                               m.control.votetrump_nonsupporters.lena, m.control.votetrump_nonsupporters.leoblig)
model_names <- c('Perceived procedural justice', 'Perceived distributive justice', 'Respect for authority boundaries', 
                 'Normative alignment with the police', 'Duty to obey the police', 'Normative alignment with the law', 
                 'Duty to obey the law')


# Function to extract coefficients for X1 and its interactions
extract_coefficients <- function(model, model_name) {
  tidy_model <- tidy(model)
  coefficients_treat <- tidy_model %>%
    filter(term == "treat1")
  coefficients_treat$model <- model_name
  return(coefficients_treat)
}

# Extract coefficients from all models
coefficients_list_supporters67 <- Map(extract_coefficients, models_list_supporters67, model_names)
coefficients_list_nonsupporters15 <- Map(extract_coefficients, models_list_nonsupporters15, model_names)

# Combine coefficients from all models
all_coefficients_supporters67 <- do.call(rbind, coefficients_list_supporters67) %>%
  mutate(trump = "Non-supporters")
all_coefficients_nonsupporters15 <- do.call(rbind, coefficients_list_nonsupporters15) %>%
  mutate(trump = "Supporters")

# Data for plot in Figure 2c
data_plot_trump_new <-
  all_coefficients_supporters67 %>%
  add_row(all_coefficients_nonsupporters15)

# Plot Figure 2c
plot.trump <- ggplot(data_plot_trump_new, 
                     aes(y = estimate, x = model, group = trump, colour = trump)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .3, position = position_dodge(), lwd = .75, show.legend = T) + 
  ylim(-.5,.5) +
  geom_hline(yintercept = 0, size = .75, color = 'darkgray') + 
  coord_flip() + 
  ylab("") + xlab("") +
  labs(colour = "") + 
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 11),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 12),
        legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 1,margin = unit(c(0,0,0,0), "mm")),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  scale_color_manual(values = c("blue", "red"), labels = c("non-Trump supporters", "Trump supporters")) + 
  theme(aspect.ratio = 2) + 
  scale_x_discrete(limits = c('Duty to obey the law', 'Normative alignment with the law', 'Duty to obey the police', 'Normative alignment with the police',
                              'Respect for authority boundaries', 'Perceived distributive justice', 'Perceived procedural justice'),
                   breaks = c('Duty to obey the law', 'Normative alignment with the law', 'Duty to obey the police', 'Normative alignment with the police',
                              'Respect for authority boundaries', 'Perceived distributive justice', 'Perceived procedural justice'),
                   labels = c('Duty to obey the law', 'Normative alignment with the law', 'Duty to obey the police', 'Normative alignment with the police',
                              'Respect for authority boundaries', 'Perceived distributive justice', 'Perceived procedural justice'))

pdf('plots/Figure2c.pdf')
plot.trump
dev.off()  