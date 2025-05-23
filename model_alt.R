#########################
## Load Libraries and functions

library(tidyverse)
library(here)
library(mlogit)
library(knitr)
library(caret)

devtools::install_version("dfidx", version = "0.0-5", repos = "http://cran.us.r-project.org")

here("code",
     "mlogit_helpers.R") |>
  source()

'%!in%' <- function(x,y)!('%in%'(x,y))

#####################
## Load in data

hh_data <- here("data",
                "NHTS",
                "hhpub.csv") |>
  read_csv(show_col_types = FALSE)

person_data <- here("data",
                    "NHTS",
                    "perpub.csv") |>
  read_csv(show_col_types = FALSE)

hh_data <- hh_data |>
  select(WRKCOUNT,
         PRICE,
         HBHTNRNT,
         HBRESDN,
         DRVRCNT,
         HHVEHCNT,
         HHSIZE,
         NUMADLT,
         HHFAMINC,
         HBPPOPDN,
         HOUSEID)

glimpse(hh_data)

person_data <- person_data |>
  select(HOUSEID,
         R_AGE,
         WORKER,
         DRIVER)

###########################
## Construct variables

hh_data <- hh_data |>
  mutate(veh_avail = case_when(HHVEHCNT == 0 ~ "Zero",
                               DRVRCNT > HHVEHCNT ~ "Insuff.",
                               TRUE ~ "Suff."))
hh_data <- hh_data |>
  mutate(n_child = HHSIZE - NUMADLT)

n_seniors <- person_data |>
  mutate(is_senior = R_AGE > 64) |>
  group_by(HOUSEID) |>
  summarise(n_seniors = sum(is_senior))

hh_data <- hh_data |>
  left_join(n_seniors)

hh_data <- hh_data |>
  mutate(three_drivers = DRVRCNT > 2)

hh_data <- hh_data |>
  mutate(n_extra_drivers = ifelse(three_drivers, DRVRCNT - 2, 0))

################################
## Construct alternative variables 

hh_data <- hh_data |>
  mutate(HHFAMINC = as.numeric(HHFAMINC)) |>
  filter(HHFAMINC > 0) |>
  mutate(income = case_when(HHFAMINC < 4 ~ "low",
                            HHFAMINC < 5 & HHSIZE > 1 ~ "low",
                            HHFAMINC < 6 & HHSIZE > 3 ~ "low",
                            HHFAMINC < 7 & HHSIZE > 5 ~ "low",
                            HHFAMINC < 8 & HHSIZE > 7 ~ "low",
                            HHFAMINC > 8 ~ "high",
                            TRUE ~ "medium")) |>
  mutate(income = factor(income, levels = c("medium", "low", "high")))

hh_data <- hh_data |>
           mutate(HBHTNRNT = as.numeric(HBHTNRNT)) |>
           filter(HBHTNRNT > 0) |>
           mutate(renter_occupied = case_when(HBHTNRNT < 60 ~ "minority",
                                              HBHTNRNT > 50 ~ "majority")) |>
           mutate(renter_occupied = factor(renter_occupied, levels = c ("minority", "majority")))

hh_data <- hh_data |>
  mutate(HBRESDN = as.numeric(HBRESDN)) |>
  filter(HBRESDN > -10) |>
  mutate(housing_units = case_when(HBRESDN < 300 ~ "low",
                                   HBRESDN < 3000 ~ "medium",
                                   HBRESDN < 30000 ~ "high")) |>
  mutate(housing_units = factor(housing_units, levels = c ("low", "medium", "high")))
         
#############################
## Construct variables

non_work_driver <- person_data |>
  mutate(non_work_driver = WORKER == "02" & DRIVER == "01") |>
  group_by(HOUSEID) |>
  summarise(non_work_driver = max(non_work_driver))

hh_data <- hh_data |>
  left_join(non_work_driver)
hh_data <- hh_data |>
  filter(HBPPOPDN > 0) |>
  mutate(density = case_when(HBPPOPDN < 7000 ~ "Low",
                             HBPPOPDN < 10000 ~ "High",
                             TRUE ~ "Medium"))
##############################
## Train data

hh_data <- hh_data |>
  select(HOUSEID,
         renter_occupied,
         housing_units,
         PRICE,
         veh_avail,
         WRKCOUNT,
         n_child,
         n_seniors,
         n_extra_drivers,
         three_drivers,
         non_work_driver,
         income,
         density)

set.seed(3105271)

hh_data_train_ids <- sample(hh_data$HOUSEID, 
                            size = ceiling(nrow(hh_data)/2))

hh_data_train <- hh_data |>
  filter(HOUSEID %in% hh_data_train_ids)

hh_data_test <- hh_data |>
  filter(HOUSEID %!in% hh_data_train_ids)

veh_dfidx_train <- fn_make_dfidx(hh_data_train,
                                 "HOUSEID",
                                 "veh_avail")

veh_dfidx_test <- fn_make_dfidx(hh_data_test,
                                "HOUSEID",
                                "veh_avail")
#############################
## Run model 

model_alt <- mlogit(choice ~ 0 | 
                      WRKCOUNT +
                      renter_occupied +
                      housing_units +
                      PRICE +
                      n_child +
                      n_seniors +
                      n_extra_drivers +
                      three_drivers + 
                      non_work_driver +
                      income +
                      density | 0,
                    veh_dfidx_train,
                    reflevel = "Suff.")

summary(model_alt)

predicts_test <- predict(model_alt, veh_dfidx_test) |>
  as.data.frame() |>
  rownames_to_column("HOUSEID") |>
  mutate(HOUSEID = as.numeric(HOUSEID)) |>
  left_join(hh_data_test)

head(predicts_test) |>
  kable()

predicts_test <- predicts_test |>
  mutate(most_likely = case_when((Suff. > Insuff.) & (Suff. > Zero) ~ "Suff.",
                                 (Zero > Insuff.) & (Zero > Suff.) ~ "Zero",
                                 TRUE ~ "Insuff.")) 
predicts_test <- predicts_test |>
  mutate(most_likely = factor(most_likely, 
                              levels = c("Suff.", "Insuff.", "Zero"))) |>
  mutate(veh_avail = factor(veh_avail,
                            levels = c("Suff.", "Insuff.", "Zero"))) |>
  mutate(correct = veh_avail == most_likely)

confusionMatrix(data = predicts_test$most_likely,
                reference = predicts_test$veh_avail)

