#### Load libraries

library(tidyverse)
library(here)
library(knitr)
library(srvyr)
library(tidycensus)
library(jtools)

#### Load dataset

trips <- here("data",
              "NHTS",
              "trippub.csv") |>
  read_csv(show_col_types = FALSE)

people <- here("data",
               "NHTS",
               "perpub.csv") |>
  read_csv(show_col_types = FALSE)


#### Filter sample

treat_trips <- trips |>
  filter(WHYTO == "13" |
           WHYFROM == "13")

weekend_treat_trips <- treat_trips |>
  filter(TDWKND == "01")


#### Generate outcome variable

short_weekend_treat_trips <- weekend_treat_trips |>
  filter(TRPMILES < 1.5)

nrow(short_weekend_treat_trips)

short_weekend_treat_trips <- short_weekend_treat_trips |>
  mutate(walk = TRPTRANS == "01")

short_weekend_treat_trips |>
  mutate(Mode = factor(ifelse(walk, "Walk", "Other mode"),
                       levels = c("Walk", "Other mode"))) |>
  group_by(Mode) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

short_weekend_treat_trips |>
  as_survey_design(weights = WTTRDFIN) |>
  mutate(Mode = factor(ifelse(walk, "Walk", "Other mode"),
                       levels = c("Walk", "Other mode"))) |>
  group_by(Mode) |>
  survey_tally(vartype = "ci") |>
  mutate(`Estimated percent of trips` = 
           paste0(round(100*n/sum(n)),"%"),
         `Lower estimate (95% confidence)` = 
           paste0(round(100*n_low/sum(n)),"%"),
         `Upper estimate (95% confidence)` = 
           paste0(round(100*n_upp/sum(n)),"%")) |>
  select(Mode,
         `Estimated percent of trips`,
         `Lower estimate (95% confidence)`,
         `Upper estimate (95% confidence)`) |>
  kable()


#### Choose predictor variables: distance, age, gender, home ownership, race

#### Distance


ggplot(short_weekend_treat_trips) +
  geom_histogram(aes(x = TRPMILES),
                 color = "olivedrab",
                 fill = "olivedrab2",
                 bins = 40) +
  scale_x_continuous(name = "Trip distance (miles)") +
  scale_y_continuous(name = "Number of trips in sample") +
  theme_minimal()

sample_trips <- short_weekend_treat_trips |>
  filter(TRPMILES >=0)

ggplot(sample_trips) +
  geom_histogram(aes(x = TRPMILES),
                 color = "olivedrab",
                 fill = "olivedrab2",
                 binwidth = 0.1) +
  scale_x_continuous(name = "Trip distance (miles)",
                     breaks = seq(0, 1.5, by=0.1)) +
  scale_y_continuous(name = "Number of trips in sample") +
  theme_minimal()


#### Age

ggplot(sample_trips) +
  geom_histogram(aes(x = R_AGE),
                 color = "olivedrab",
                 fill = "olivedrab2",
                 binwidth = 1) +
  scale_x_continuous(name = "Traveler's age (years)",
                     breaks = seq(0, 100, by = 10),
                     limits = c(0, 100)) +
  scale_y_continuous(name = "Number of trips in sample") +
  theme_minimal()

##### Gender

sample_trips |>
  group_by(R_SEX) |>
  summarise(`Number of trips` = n())

sample_trips <- sample_trips |>
  filter(R_SEX != "-7") |>
  mutate(female = R_SEX == "02")

sample_trips |>
  group_by(female) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()


##### Home ownership 

sample_trips <- sample_trips |>
  filter(HOMEOWN != "-7") |>
  mutate(owner = HOMEOWN == "01")

sample_trips |>
  group_by(owner) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()

##### Race 

race_data <- people |>
  select(HOUSEID, PERSONID, R_RACE)

sample_trips <- sample_trips |>
  left_join(race_data) |>
  mutate(white = R_RACE == "01")

sample_trips |>
  group_by(white) |>
  summarise(`Number of trips` = n()) |>
  mutate(`Percent of trips` = 
           paste0(round(100*`Number of trips`/sum(`Number of trips`)), "%")) |>
  kable()



##### Estimate model 


model <- glm(walk ~ 
               TRPMILES +
               R_AGE +
               female +
               owner +
               white,
             data = sample_trips,
             family = "binomial")

coeff_labels <- c("Trip distance (miles)" = "TRPMILES",
                  "Age (years)" = "R_AGE",
                  "Female" = "femaleTRUE",
                  "Owner" = "ownerTRUE",
                  "White" = "whiteTRUE")

export_summs(model, 
             robust = "HC3", 
             coefs = coeff_labels,
             error_format = "(p = {p.value})",
             error_pos = "right")

export_summs(model, 
             robust = "HC3", 
             coefs = coeff_labels,
             error_format = "(p = {p.value})",
             error_pos = "same",
             to.file = "Word",
             file.name = here("P5",
                              "model-result.docx"))

#### Visualize results

effect_plot(model, pred = "TRPMILES", interval = TRUE) +
  scale_x_continuous(name = "Trip distance (miles)",
                     breaks = seq(0, 1.5, by  =0.1)) +
  scale_y_continuous(name = "Probabilitity of walking",
                     breaks = breaks <- seq(0, 0.8, by = 0.1),
                     labels = paste0(breaks*100, "%"))


effect_plot(model = model, pred = "owner", interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0.11, 0.23, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Is the person a home owner?"),
                   labels = c("No", "Yes"))


effect_plot(model = model, pred = "white", interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0.11, 0.23, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Is the person White?"),
                   labels = c("No", "Yes"))


effect_plot(model = model, pred = "female", interval = TRUE) +
  scale_y_continuous(name = "Probability of walking for a particular trip",
                     breaks = breaks <- seq(0.11, 0.23, by=0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = paste0("Is the person female?"),
                   labels = c("No", "Yes"))

