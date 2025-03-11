####################Load libraries

library(tidyverse)
library(here)
library(knitr)
library(srvyr)
library(mlogit)
library(caret)
library(pscl)

here("code",
     "mlogit_helpers.R") |>
  source()



#####################Load datasets

hh_data <- here("data",
                "NHTS",
                "hhpub.csv") |>
  read_csv(show_col_types = FALSE)

person_data <- here("data",
                    "NHTS",
                    "perpub.csv") |>
  read_csv(show_col_types = FALSE) 

trip_data <- here("data",
                  "NHTS",
                  "trippub.csv") |>
  read_csv(show_col_types = FALSE)  |>
  select(HOUSEID,
         PERSONID,
         TDTRPNUM,
         TRIPPURP,
         WHYFROM,
         WHYTO,
         TRPTRANS, 
         R_AGE,
         EDUC,
         WTTRDFIN)

###################Construct trip purpose 

shopping_count <- trip_data %>%
  mutate(is_shopping = (WHYTO == "11"))%>%
  group_by(HOUSEID) %>%
  summarise(n_shopping = sum(is_shopping),
            n_trips = n()) %>%
  mutate(n_non_shopping = n_trips - n_shopping)

trip_data <- trip_data %>%
  mutate(is_shopping = (WHYTO == "11"))


#########################Select and construct predictor variables

household <- hh_data |>
  select(HOUSEID, HH_RACE, WALK, YOUNGCHILD, HHFAMINC, HTPPOPDN, HHSIZE) |>
  filter(HH_RACE != "-7" & HH_RACE != "-8") |>
  filter(as.numeric(WALK) > 0)|>
  mutate(regularwalk = WALK == "01" | WALK == "02") |>
  mutate(HHFAMINC = as.numeric(HHFAMINC)) |>
  filter(HHFAMINC > 0) |>
  mutate(income = case_when(HHFAMINC < 4 ~ "low",
                            HHFAMINC < 5 & HHSIZE > 1 ~ "low",
                            HHFAMINC < 6 & HHSIZE > 3 ~ "low",
                            HHFAMINC < 7 & HHSIZE > 5 ~ "low",
                            HHFAMINC < 8 & HHSIZE > 7 ~ "low",
                            HHFAMINC > 8 ~ "high",
                            TRUE ~ "middle")) |>
  mutate(income = factor(income, levels = c("low", "middle", "high")))|>
  mutate(HTPPOPDN = as.numeric(HTPPOPDN))|>
  filter(HTPPOPDN > 0) |>
  left_join(shopping_count) |>
  mutate(dens_cat = case_when(HTPPOPDN < 700 ~ "low",
                              HTPPOPDN > 5000 ~ "high",
                              TRUE ~ "medium"))
  


############### Summarize trip purpose 

trip_data |>
  group_by(is_shopping) |>
  summarise(`Number of trips (unweighted)` = n()) |>
  mutate(`Percent of trips (unweighted)` = 
           100 * `Number of trips (unweighted)`/
           sum(`Number of trips (unweighted)`)) |>
  arrange(desc(`Percent of trips (unweighted)`)) |>
  kable(format.args = list(big.mark = ","), digits = 0)

################ Count trips with purpose to get goods

goods_trips <- trip_data |>
  filter(is_shopping == "TRUE")|>
  group_by(HOUSEID)|>
  summarise(goods_trips = n())

household <- household |>
  left_join(goods_trips)|>
  replace_na(list(goods_trips = 0))
 
ggplot(household) +
  geom_histogram(aes(x=goods_trips),
                 binwidth=1,
                 color = "lightblue",
                 fill = "darkgreen")+
  scale_x_continuous(name="Number of goods trips",
                     breaks = seq(0,22,by=1))+
  scale_y_continuous(name="Number of household in sample")+
  theme_minimal()

###### Distribution 

household |>
  summarise(`Average count of goods trips` = mean(goods_trips),
            `Standard deviation` = sd(goods_trips)) |>
  kable(digits = 3)

####### Zero Inflated regression

goods_zero <- zeroinfl(goods_trips ~ 
                          YOUNGCHILD + 
                          HH_RACE +  
                          income +
                          dens_cat +
                          regularwalk |
                         YOUNGCHILD + 
                         HH_RACE +  
                         income +
                         dens_cat +
                         regularwalk,
                        data = household,    
                        dist = "negbin")  

summary(goods_zero)

goods_zero <- tibble(observed = goods_zero$model$goods_trips, 
                      predicted = goods_zero$fitted.values)

ggplot(goods_zero) +   
  geom_jitter(aes(x = observed,                  
                  y = predicted),               
              color = "darkblue",               
              alpha = 0.5,               
              size = 0.5) +   
  scale_x_continuous(name = "Number of observed trips per household")
  scale_y_continuous(name = "Number of predicted trips per household")
        
  theme_minimal()


