options(java.parameters = '-Xmx4G')

library(tidyverse)
library(here)
library(knitr)
library(tigris)
library(stringr)
library(maptiles)
library(tidyterra)
library(r5r)
library(sf)
library(leaflet)

 

here("code",
     "grvty_balancing.R") |>
  source()

all_cbsas <- core_based_statistical_areas(progress_bar = FALSE,
                                          year = 2024) |>
  select(NAMELSAD) |>
  mutate(type = ifelse(!is.na(str_match(NAMELSAD, "Metro")), "Metro", "Micro")) |>
  mutate(type = as.character(type))

table(all_cbsas$type) |>
  kable()

missoula <- all_cbsas |>
  filter(NAMELSAD == "Missoula, MT Metro Area") |>
  st_transform("WGS84")


base_map <- get_tiles(missoula,
                      provider = "CartoDB.Positron",
                      zoom = 9,
                      crop = TRUE)

ggplot(missoula) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(fill = NA,
          color = "tomato") +
  theme_void()

#### you want main and not aux

read_csv("C:/Users/ariel/OneDrive - Harvard University/forecasting-transportation/examples-ariellaamit/p4/mt_od_aux_JT01_2021.csv.gz")

state <- "mt"
year <- "2021"

missoula_counties_5_digit <- c("30063")
missoula_counties_3_digit <- substr(missoula_counties_5_digit, 3, 5)

url <- paste0("https://lehd.ces.census.gov/data/lodes/LODES8/",
              state,
              "/od/",
              state,
              "_od_main_JT00_",
              year,
              ".csv.gz")

pa_data <- read_csv(url) |>
  mutate(w_county = substr(w_geocode, 1, 5),
         h_county = substr(h_geocode, 1, 5)) |>
  filter(h_county %in% missoula_counties_5_digit &
           w_county %in% missoula_counties_5_digit) |>
  mutate(w_geocode = as.character(w_geocode),
         h_geocode = as.character(h_geocode))

head(pa_data) |>
  kable()

total_prod <- pa_data |>
  group_by(h_geocode) |>
  summarise(goods_p = sum(SI01),
            trade_p = sum(SI02),
            serve_p = sum(SI03),
            total_p = sum(S000)) |>
  rename(geocode = h_geocode)

total_attr <- pa_data |>
  group_by(w_geocode) |>
  summarize(goods_a = sum(SI01),
            trade_a = sum(SI02),
            serve_a = sum(SI03),
            total_a = sum(S000)) |>
  rename(geocode = w_geocode)

trip_gen <- full_join(total_prod,
                      total_attr) |>
  replace_na(list(goods_p = 0, 
                  goods_a = 0,
                  trade_p = 0,
                  trade_a = 0,
                  serve_p = 0,
                  serve_a = 0,
                  total_p = 0,
                  total_a = 0))

### load spatial data

msa_blocks <- st_read("C:/Users/ariel/OneDrive - Harvard University/forecasting-transportation/examples-ariellaamit/p4/data/tl_2022_30_tabblock20/tl_2022_30_tabblock20.shp")

msa_blocks_filtered <- msa_blocks %>%
  filter(substr(COUNTYFP20, 1, 3) %in% missoula_counties_3_digit)

ggplot(msa_blocks_filtered) +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(fill = NA,
          color = "tomato") +
  theme_void()

trip_gen_locs <- msa_blocks_filtered |>
  rename(geocode = GEOID20) |>
  right_join(trip_gen) |>
  select(geocode, 
         goods_p, 
         trade_p, 
         serve_p,
         total_p,
         goods_a, 
         trade_a,
         serve_a,
         total_a) |>
  st_transform("WGS84")

leaflet(trip_gen_locs) |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(weight = 2,
              color = "tomato",
              fillColor = "tomato",
              fillOpacity = 0.1,
              highlightOptions = highlightOptions(weight = 3,
                                                  fillOpacity = 0.5),
              label = trip_gen_locs$geocode)

### load the network

missoula_network <- here("P4",
                          "network") |>
  setup_r5()

street_vis <- street_network_to_sf(missoula_network)

street_lines <- street_vis$edges
street_pts <- street_vis$vertices

st_write(street_lines,
         here("data",
              "street-lines_1.shp"))

st_write(street_pts,
         here("data",
              "street-pts_1.shp"))

stop_r5()

street_lines <- here("P4",
                     "data",
                     "street-lines.shp") |>
  st_read()

street_pts <- here("P4",
                   "data",
                   "street-pts.shp") |>
  st_read()

base_map <- get_tiles(street_lines,
                      provider = "CartoDB.Positron",
                      zoom = 8,
                      crop = TRUE)

trip_gen_loc_ids <- trip_gen_locs |>
  st_point_on_surface() |>
  st_nearest_feature(street_pts)

trip_gen_pts <- street_pts[trip_gen_loc_ids,] |>
  mutate(id = trip_gen_locs$geocode) |>
  select(id)

ggplot() +
  geom_spatraster_rgb(data = base_map) +
  geom_sf(data = trip_gen_locs,
          color = "tomato",
          fill = "tomato") +
  geom_sf(data = street_lines,
          color =  "orange") +
  theme_void()

##### skim 


missoula_core <- here("C:/Users/ariel/OneDrive - Harvard University/forecasting-transportation/examples-ariellaamit/p4/network1") |>
  setup_r5()

skim1 <- travel_time_matrix(missoula_core,
                            origins = trip_gen_pts,
                            destinations = trip_gen_pts,
                            mode = "CAR",
                            max_trip_duration = 600)

stop_r5()

write_csv(skim1, file = here("P4",
                             "data",
                             "missoula-skim.csv"))

skim1 <- read_csv(here("P4",
                       "data",
                       "missoula-skim.csv"),
                  col_types = "ccn")

head(skim1) |>   
  kable()

nrow(skim1)

#### Find missing points

skim_interzonal <- skim1 |>
  filter(from_id != to_id)

unique_skim_zones <- unique(c(skim_interzonal$from_id, skim_interzonal$to_id))

missing_from_skim <- trip_gen_locs |>
  filter(geocode %!in% unique_skim_zones)

missing_pts <- trip_gen_pts  |>
  filter(id %!in% unique_skim_zones)

missing_zones_trip_gen <- trip_gen |>
  filter(geocode %!in% unique_skim_zones)


### Find missing rows

matching_rows <- skim1 |> 
  filter(from_id == 300630014011051
         | to_id == 	
           300630014011051) 

### Remove missing points

skim1 <- skim1 |> 
  filter(!(from_id %in% missing_zones_trip_gen | to_id %in% missing_zones_trip_gen))

skim1 <- skim1 |> 
  filter(!(from_id %in% missing_zones_trip_gen$geocode | 
             to_id %in% missing_zones_trip_gen$geocode))

pa_data <- pa_data |> 
  filter(!(w_geocode %in% missing_zones_trip_gen$geocode | 
             h_geocode %in% missing_zones_trip_gen$geocode))

###apply a gravity model

flow_tt <- pa_data |>
  rename(from_id = h_geocode,
         to_id = w_geocode) |>
  right_join(skim1) |>
  rename(flow_total = S000,
         flow_goods = SI01,
         flow_trade = SI02,
         flow_serve = SI03) |>
  replace_na(list(flow_total = 0,
                  flow_goods = 0,
                  flow_trade = 0,
                  flow_serve = 0))

avg_tts <- tibble(`Worker sector` = c("Goods", "Trade", "Service", "Total"),
                  `Average travel time (observed)` = c(
                    sum(flow_tt$flow_goods * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_goods),
                    sum(flow_tt$flow_trade * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_trade),
                    sum(flow_tt$flow_serve * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_serve),
                    sum(flow_tt$flow_total * flow_tt$travel_time_p50) / 
                      sum(flow_tt$flow_total)))

kable(avg_tts, digits = 1)

betas <- 1/avg_tts$`Average travel time (observed)`
names(betas) <- c("Goods", "Trade", "Service", "Total")

initial_betas <- tibble(`Worker sector` = names(betas),
                        `Initial β value` = betas)

kable(initial_betas, digits = 3)

### visualize travel time sensitivity

friction <- tibble(`Travel time (min)` = seq(0, 30, by=1)) |>
  mutate(Goods = exp(-1 * betas["Goods"] * `Travel time (min)`),
         Trade = exp(-1 * betas["Trade"] * `Travel time (min)`),
         Service = exp(-1 * betas["Service"] * `Travel time (min)`),
         `All industries` = exp(-1 * betas["Total"] * `Travel time (min)`)) |>
  pivot_longer(cols = -`Travel time (min)`,
               names_to = "Industry") |>
  rename(`Destination attractiveness` = value)

ggplot(friction) +
  geom_line(aes(x = `Travel time (min)`,
                y = `Destination attractiveness`,
                linetype = Industry)) +
  scale_x_continuous(breaks = seq(0, 30, by=5)) +
  scale_y_continuous(breaks = seq(0, 1.1, by=0.1)) +
  theme_minimal()

### calculate friction factors

flow_tt <- flow_tt |>
  mutate(friction_goods = exp(-1 * betas["Goods"] * travel_time_p50),
         friction_trade = exp(-1 * betas["Trade"] * travel_time_p50),
         friction_serve = exp(-1 * betas["Service"] * travel_time_p50),
         friction_total = exp(-1 * betas["Total"] * travel_time_p50))

###estimate initial trip matrix

flow_goods_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "goods_p",
                                  zone_d = "goods_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_goods",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_trade_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "trade_p",
                                  zone_d = "trade_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_trade",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_serve_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "serve_p",
                                  zone_d = "serve_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_serve",
                                  tolerance = 0.001,
                                  max_iter = 100)

flow_total_est <- grvty_balancing(od_zones = trip_gen,
                                  friction = flow_tt,
                                  zone_id = "geocode",
                                  zone_o = "total_p",
                                  zone_d = "total_a",
                                  friction_o_id = "from_id",
                                  friction_d_id = "to_id",
                                  friction_factor = "friction_total",
                                  tolerance = 0.001,
                                  max_iter = 100)

write_csv(flow_goods_est$flows,
          file = here("P4",
                      "data",
                      "init-goods-flow.csv"))

write_csv(flow_trade_est$flows,
          file = here("P4",
                      "data",
                      "init-trade-flow.csv"))

write_csv(flow_serve_est$flows,
          file = here("P4",
                      "data",
                      "init-serve-flow.csv"))

write_csv(flow_total_est$flows,
          file = here("P4",
                      "data",
                      "init-total-flow.csv"))

###Evaluate model fit

flow_goods <- here("P4",
                   "data",
                   "init-goods-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         goods_flow_est = flow)

flow_trade <- here("P4",
                   "data",
                   "init-trade-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         trade_flow_est = flow)

flow_serve <- here("P4",
                   "data",
                   "init-serve-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         serve_flow_est = flow)

flow_total <- here("P4",
                   "data",
                   "init-total-flow.csv") |>
  read_csv(col_types = "ccn") |>
  rename(from_id = o_id,
         to_id = d_id,
         total_flow_est = flow)

flow_tt <- flow_tt |>
  left_join(flow_goods) |>
  left_join(flow_trade) |>
  left_join(flow_serve) |> 
  left_join(flow_total)

avg_tts <- avg_tts |>
  mutate(`Average travel time (estimated)` = c(
    sum(flow_tt$goods_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$goods_flow_est),
    sum(flow_tt$trade_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$trade_flow_est),
    sum(flow_tt$serve_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$serve_flow_est),
    sum(flow_tt$total_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$total_flow_est)))

avg_tts |>
  kable(digits = 1)

avg_tts <- avg_tts |>
  mutate(rmse = c((mean((flow_tt$flow_goods - flow_tt$goods_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_trade - flow_tt$trade_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_serve - flow_tt$serve_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_total - flow_tt$total_flow_est)^2))^0.5))

kable(avg_tts, digits = 2)

###Visualize comparison

plot_flows <- function(flow_df,
                       obs_col_name,
                       est_col_name) {
  
  summary <- flow_df |>
    rename(obs = all_of(obs_col_name),
           est = all_of(est_col_name)) |>
    group_by(obs, est) |>
    summarize(n = n()) 
  
  max_scale <- max(summary$obs, summary$est)
  my_interval <- ceiling(max_scale / 10)
  dot_size <- floor(70 / max_scale)
  
  max_n_exp = round(log10(max(summary$n)))
  
  ggplot(summary) +
    geom_point(aes(x = obs,
                   y = est,
                   color = n),
               size = dot_size) +
    scale_x_continuous(name = "Observed flow", 
                       limits = c(0, max_scale),
                       breaks = seq(0, max_scale, by=my_interval)) +
    scale_y_continuous(name = "Estimated flow", 
                       limits = c(0, max_scale),
                       breaks = seq(0, max_scale, by=my_interval)) +
    scale_color_viridis_c(transform = "log",
                          breaks = my_breaks <- c(10^seq(-1, 
                                                         max_n_exp, 
                                                         by=1)),
                          labels = formatC(my_breaks, format = "d", 
                                           big.mark = ","),
                          direction = -1,
                          name = "Number of P-A pairs") +
    theme_minimal()
  
  
}

plot_flows(flow_tt, 
           obs_col_name = "flow_goods",
           est_col_name = "goods_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_trade",
           est_col_name = "trade_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_serve",
           est_col_name = "serve_flow_est")

plot_flows(flow_tt, 
           obs_col_name = "flow_total",
           est_col_name = "total_flow_est")



#####calibrate gravity model

flow_tt <- flow_tt |>
  select(-goods_flow_est,
         -trade_flow_est,
         -serve_flow_est,
         -total_flow_est)

## calibrate goods beta
calibrated_flows_goods <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_goods",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_goods <- calibrated_flows_goods$beta

goods_flow_est <- calibrated_flows_goods$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         goods_flow_est = flow_est) |>
  select(from_id, to_id, goods_flow_est)

flow_tt <- flow_tt |>
  left_join(goods_flow_est)

####calibrate trade beta

calibrated_flows_trade <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_trade",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_trade <- calibrated_flows_trade$beta

trade_flow_est <- calibrated_flows_trade$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         trade_flow_est = flow_est) |>
  select(from_id, to_id, trade_flow_est)

flow_tt <- flow_tt |>
  left_join(trade_flow_est)

### calibrate service beta
calibrated_flows_serve <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_serve",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_serve <- calibrated_flows_serve$beta

serve_flow_est <- calibrated_flows_serve$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         serve_flow_est = flow_est) |>
  select(from_id, to_id, serve_flow_est)

flow_tt <- flow_tt |>
  left_join(serve_flow_est)

## calibrate total beta
calibrated_flows_total <- grvty_calibrate(obs_flow_tt = flow_tt,
                                          o_id_col = "from_id",
                                          d_id_col = "to_id",
                                          obs_flow_col = "flow_total",
                                          tt_col = "travel_time_p50",
                                          tolerance_balancing = 0.0001,
                                          max_iter_balancing = 30,
                                          tolerance_calibration = 0.2,
                                          max_iter_calibration = 30)

beta_total <- calibrated_flows_total$beta

total_flow_est <- calibrated_flows_total$flows |>
  rename(from_id = o_id,
         to_id = d_id,
         total_flow_est = flow_est) |>
  select(from_id, to_id, total_flow_est)

flow_tt <- flow_tt |>
  left_join(total_flow_est)

betas_table <- tibble(Industry = c("Goods", 
                                   "Trade",
                                   "Service",
                                   "Total"),
                      beta_initial = betas,
                      beta_calibrated = c(beta_goods,
                                          beta_trade,
                                          beta_serve,
                                          beta_total))
write_csv(flow_tt,
          here("P4",
               "data",
               "calib-flows.csv"))

write_csv(betas_table,
          here("P4",
               "data",
               "calib-betas.csv"))

#####evaluate model fit

flow_tt <- here("P4",
                "data",
                "calib-flows.csv") |>
  read_csv()

avg_tts <- avg_tts |>
  select(-rmse) |>
  mutate(`Average travel time (estimated)` = c(
    sum(flow_tt$goods_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$goods_flow_est),
    sum(flow_tt$trade_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$trade_flow_est),
    sum(flow_tt$serve_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$serve_flow_est),
    sum(flow_tt$total_flow_est * flow_tt$travel_time_p50) / 
      sum(flow_tt$total_flow_est)))

avg_tts |>
  kable(digits = 1)

avg_tts <- avg_tts |>
  mutate(rmse = c((mean((flow_tt$flow_goods - flow_tt$goods_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_trade - flow_tt$trade_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_serve - flow_tt$serve_flow_est)^2))^0.5,
                  (mean((flow_tt$flow_total - flow_tt$total_flow_est)^2))^0.5))

kable(avg_tts, digits = 2)

plot_flows(flow_tt,
           obs_col_name = "flow_goods",
           est_col_name = "goods_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_trade",
           est_col_name = "trade_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_serve",
           est_col_name = "serve_flow_est")

plot_flows(flow_tt,
           obs_col_name = "flow_total",
           est_col_name = "total_flow_est")

### interpret parameters

betas_table <- here("P4",
                    "data",
                    "calib-betas.csv") |> 
  read_csv()

friction <- tibble(`Travel time (min)` = seq(1, 60, by=1)) |>
  mutate(Goods = exp(-1 * betas_table$beta_calibrated[1] * `Travel time (min)`),
         Trade = exp(-1 * betas_table$beta_calibrated[2] * `Travel time (min)`),
         Service = exp(-1 * betas_table$beta_calibrated[3] * `Travel time (min)`),
         `All industries` = 
           exp(-1 * betas_table$beta_calibrated[4] * `Travel time (min)`)) |>
  pivot_longer(cols = -`Travel time (min)`,
               names_to = "Sector") |>
  rename(`Destination attractiveness` = value) |>
  filter(`Destination attractiveness` < 2)

ggplot(friction) +
  geom_line(aes(x = `Travel time (min)`,
                y = `Destination attractiveness`,
                linetype = Sector)) +
  scale_x_continuous(breaks = seq(0, 60, by=5)) +
  scale_y_continuous(breaks = seq(0, 2, by=0.1),
                     limits = c(0, 1.5)) +
  theme_minimal()

##gravity balancing fx from github

grvty_balancing <- function(od_zones,
                            friction,
                            zone_id,
                            zone_o,
                            zone_d,
                            friction_o_id,
                            friction_d_id,
                            friction_factor,
                            tolerance,
                            max_iter) {
  
  # for quick tests
  # od_zones <- trip_gen
  # friction <- skim
  # zone_id <- "GEOID"
  # zone_o <- "hbo_trip_prod"
  # zone_d <- "hbo_bal_attr"
  # friction_o_id <- "from_GEOID"
  # friction_d_id <- "to_GEOID"
  # friction_factor <- "F_HBO"
  # tolerance <- 0.01
  # max_iter <- 100
  
  # rename and select columns
  wip_friction <- friction |>
    dplyr::rename(o_id = tidyselect::all_of(friction_o_id),
                  d_id = tidyselect::all_of(friction_d_id),
                  factor = tidyselect::all_of(friction_factor)) |>
    dplyr::select(o_id, d_id, factor)
  
  wip_zones <- od_zones |>
    dplyr::rename(id = tidyselect::all_of(zone_id),
                  origin = tidyselect::all_of(zone_o),
                  destin = tidyselect::all_of(zone_d)) |>
    dplyr::mutate(origin = origin,
                  destin = destin) |>
    dplyr::select(id, origin, destin)
  
  # get minimum non-zero value for friction factor
  min_factor <- min(wip_friction$factor[wip_friction$factor != 0])
  
  # replace zero values for friction factors
  if(sum(wip_friction$factor == 0) > 0) {
    warning("\nReplacing friction factors of zero with the lowest non-zero friction factor.\n")
    wip_friction <- wip_friction |>
      # set all zero friction values equal to the smallest non-zero value
      dplyr::mutate(factor = ifelse(factor == 0, min_factor, factor))
  }
  
  # warn and remove friction rows where the friction factor is missing or undefined
  if(sum(is.na(wip_friction$factor)) > 0 |
     sum(is.infinite(wip_friction$factor)) > 0) {
    warning("\nIgnoring origin-destination pairs with missing or undefined friction factors.\n")
    wip_friction <- wip_friction |>
      dplyr::filter(!is.na(wip_friction$factor) &
                      !is.infinite(wip_friction$factor))
  }
  
  # Check that no zones are repeated in the zones table
  if(length(wip_zones$id) > length(unique(wip_zones$id))) {
    warning("\nDuplicated zone IDs in zones table. Aggregating origins and destinations by zone ID.\n")
    wip_zones <- wip_zones |>
      dplyr::group_by(id) |>
      dplyr::summarise(origin = sum(origin),
                       destin = sum(destin))
  }
  
  # Check that no OD pairs are repeated in the friction table
  wip_friction$combined_id <- paste0(as.character(wip_friction$o_id),
                                     as.character(wip_friction$d_id))
  if(length(wip_friction$combined_id) > length(unique(wip_friction$combined_id))) {
    warning("\nAverageing friction factors across duplicated origin-destination pairs in friction table.\n")
    wip_friction <- wip_friction |>
      dplyr::group_by(combined_id) |>
      dplyr::summarise(factor = mean(factor))
  }
  
  # Check that all the zones in the skim are in the zone table.
  # If they are missing from the zone table, remove them from the skim.
  unique_friction_ids <- unique(c(wip_friction$o_id, wip_friction$d_id))
  missing_from_zones <- unique_friction_ids[!unique_friction_ids %in% wip_zones$id]
  if(length(missing_from_zones > 0)) {
    missing_from_zones_warning = paste0("\nRemoving ",
                                        length(missing_from_zones),
                                        " zones from the friction data frame that are missing",
                                        " from the origin-destination table.\n")
    warning(missing_from_zones_warning)
    wip_friction <- wip_friction |>
      dplyr::filter(o_id %in% wip_zones$id,
                    d_id %in% wip_zones$id)
  }
  
  # Check that all zones in the zone table are in the skim.
  # If they are missing from the skim, remove them from the zone table
  missing_from_friction <- wip_zones$id[!wip_zones$id %in% unique_friction_ids]
  if(length(missing_from_friction > 0)) {
    missing_from_friction_warning = paste0("Removing ",
                                           length(missing_from_friction),
                                           " zones from the origin-destination data",
                                           " frame that are missing",
                                           " from the friction factor table.")
    warning(missing_from_friction_warning)
    wip_zones <- wip_zones |>
      dplyr::filter(id %in% unique_friction_ids)
  }
  
  # Replace missing origins and destinations with zeros
  if(sum(is.na(wip_zones$origin)) > 0) {
    warning("\nReplacing missing orgin values with zeros.\n")
    wip_zones <- wip_zones |>
      tidyr::replace_na(list(origin = 0))
  }
  if(sum(is.na(wip_zones$destin)) > 0) {
    warning("\nReplacing missing destination values with zeros.\n")
    wip_zones <- wip_zones |>
      tidyr::replace_na(list(destin = 0))
  }
  
  # Check whether origin and destination totals are consistent
  if(sum(wip_zones$origin) != sum(wip_zones$destin)) {
    warning(paste0("\nTotal number of origins does not equal total number of destinations.\n",
                   "Rescaling destinations for consistency with total origins.\n"))
    wip_zones$destin = wip_zones$destin *
      (sum(wip_zones$origin)/sum(wip_zones$destin))
  }
  
  # scale up so all values are greater than 10^-100
  if (min_factor < 10^-100) {
    wip_friction <- wip_friction |>
      dplyr:: mutate(factor = factor * (10^-100 / min_factor))
  }
  
  # Add productions and attractions to trip matrix
  origins <- wip_zones |>
    dplyr::select(id, origin)
  
  destinations <- wip_zones |>
    dplyr::select(id, destin)
  
  flows <- wip_friction |>
    dplyr::left_join(origins, by = c("o_id" = "id")) |>
    dplyr::left_join(destinations, by = c("d_id" = "id")) |>
    dplyr::rename(friction = factor)
  
  # first iteration
  message("\nBalancing iteration 1")
  flows <- flows |>
    dplyr::mutate(B_factor = 1)
  
  flows <- flows |>
    dplyr::group_by(o_id) |>
    dplyr::mutate(A_factor = 1/sum(B_factor * destin * friction)) |>
    dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) |>
    dplyr::ungroup()
  
  balance_check_o <- flows |>
    dplyr::group_by(o_id) |>
    dplyr::summarize(target = mean(origin),
                     value = sum(flow)) |>
    dplyr::ungroup() |>
    dplyr::mutate(diff = (value - target) / target) |>
    tidyr::replace_na(list(diff = 0)) |>
    dplyr::summarize(max_o_diff = max(abs(diff)))
  
  balance_check_d <- flows |>
    dplyr::group_by(d_id) |>
    dplyr::summarize(target = mean(destin),
                     value = sum(flow)) |>
    dplyr::ungroup() |>
    dplyr::mutate(diff = (value - target) / target) |>
    tidyr::replace_na(list(diff = 0)) |>
    dplyr::summarize(max_d_diff = max(abs(diff)))
  
  balance_check <- tibble::tibble(iteration = 1,
                                  max_o_diff = round(balance_check_o$max_o_diff[1],4),
                                  max_d_diff = round(balance_check_d$max_d_diff[1],4))
  
  # Loop for the rest of the iterations
  done <- FALSE
  i <- 2
  while (!done) {
    message(paste0("\nBalancing iteration ", i))
    flows <- flows |>
      dplyr::group_by(d_id) |>
      dplyr::mutate(B_factor = 1 / sum(A_factor * origin * friction)) |>
      dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) |>
      dplyr::ungroup()
    
    balance_check_o <- flows |>
      dplyr::group_by(o_id) |>
      dplyr::summarize(target = mean(origin),
                       value = sum(flow)) |>
      dplyr::ungroup() |>
      dplyr::mutate(diff = (value - target) / target) |>
      tidyr::replace_na(list(diff = 0)) |>
      dplyr::summarize(max_o_diff = max(abs(diff)))
    
    balance_check_d <- flows |>
      dplyr::group_by(d_id) |>
      dplyr::summarize(target = mean(destin),
                       value = sum(flow)) |>
      dplyr::ungroup() |>
      dplyr::mutate(diff = (value - target) / target) |>
      tidyr::replace_na(list(diff = 0)) |>
      dplyr::summarize(max_d_diff = max(abs(diff)))
    
    next_balance_check <- tibble::tibble(iteration = i,
                                         max_o_diff =
                                           round(balance_check_o$max_o_diff[1],4),
                                         max_d_diff =
                                           round(balance_check_d$max_d_diff[1],4))
    
    balance_check <- rbind(balance_check, next_balance_check)
    
    i <- i + 1
    
    message(paste0("\nBalancing iteration ", i))
    flows <- flows |>
      dplyr::group_by(o_id) |>
      dplyr::mutate(A_factor = 1 / sum(B_factor * destin * friction)) |>
      dplyr::mutate(flow = A_factor * origin * B_factor * destin * friction) |>
      dplyr::ungroup()
    
    balance_check_o <- flows |>
      dplyr::group_by(o_id) |>
      dplyr::summarize(target = mean(origin),
                       value = sum(flow)) |>
      dplyr::ungroup() |>
      dplyr::mutate(diff = (value - target) / target) |>
      tidyr::replace_na(list(diff = 0)) |>
      dplyr::summarize(max_o_diff = max(abs(diff)))
    
    balance_check_d <- flows |>
      dplyr::group_by(d_id) |>
      dplyr::summarize(target = mean(destin),
                       value = sum(flow)) |>
      dplyr::ungroup() |>
      dplyr::mutate(diff = (value - target) / target) |>
      tidyr::replace_na(list(diff = 0)) |>
      dplyr::summarize(max_d_diff = max(abs(diff)))
    
    next_balance_check <- tibble::tibble(iteration = i,
                                         max_o_diff =
                                           round(balance_check_o$max_o_diff[1],4),
                                         max_d_diff =
                                           round(balance_check_d$max_d_diff[1],4))
    
    balance_check <- rbind(balance_check, next_balance_check)
    
    i <- i + 1
    done = (next_balance_check$max_o_diff < tolerance &
              next_balance_check$max_d_diff < tolerance) |
      i > max_iter
    
  }
  
  flows <- flows |>
    dplyr::mutate(flow = round(flow)) |>
    dplyr::select(o_id, d_id, flow)
  
  list(flows = flows, convergence = balance_check)
}

grvty_calibrate <- function(obs_flow_tt,
                            o_id_col,
                            d_id_col,
                            obs_flow_col,
                            tt_col,
                            tolerance_balancing,
                            max_iter_balancing,
                            tolerance_calibration,
                            max_iter_calibration) {
  
  # rename and select columns
  wip_flows <- obs_flow_tt |>
    dplyr::rename(o_id = tidyselect::all_of(o_id_col),
                  d_id = tidyselect::all_of(d_id_col),
                  flow = tidyselect::all_of(obs_flow_col),
                  tt = tidyselect::all_of(tt_col)) |>
    dplyr::select(o_id, d_id, flow, tt)
  
  # calculate average observed travel time
  mean_tt_obs <- sum(wip_flows$flow * wip_flows$tt) / 
    sum(wip_flows$flow)
  
  # calculate zone total origins and destinations
  zone_total_o <- wip_flows |>
    group_by(o_id) |>
    summarise(o = sum(flow)) |>
    rename(id = o_id)
  
  zone_total_d <- wip_flows |>
    group_by(d_id) |>
    summarize(d = sum(flow)) |>
    rename(id = d_id)
  
  zone_totals <- full_join(zone_total_o,
                           zone_total_d,
                           by = join_by(id)) |>
    replace_na(list(o = 0, d = 0))
  
  # balance origins and destinations if necessary
  if (sum(zone_totals$o) != sum(zone_totals$d)) {
    warning(paste0("Total number of origins does not equal total number of destinations.\n",
                   "Rescaling destinations for consistency with total origins.\n"))
    zone_totals$d = zone_totals$d *
      (sum(zone_totals$o)/sum(zone_totals$d))
  }
  
  # Calculate initial beta value
  m <- 1
  message(paste0("\nCalibration iteration ", m))
  
  beta_1 <- 1/mean_tt_obs
  
  # calculate initial friction factors
  wip_flows <- wip_flows |>
    mutate(friction = exp(-1 * beta_1 * tt))
  
  # initial flow estimates
  flows_result <- grvty_balancing(od_zones = zone_totals,
                                  friction = wip_flows,
                                  zone_id = "id",
                                  zone_o = "o",
                                  zone_d = "d",
                                  friction_o_id = "o_id",
                                  friction_d_id = "d_id",
                                  friction_factor = "friction",
                                  tolerance = tolerance_balancing,
                                  max_iter = max_iter_balancing)
  
  convergence <- flows_result$convergence
  
  if(nrow(convergence) >= max_iter_balancing) {
    warning(paste0("\nCalibration iteration ",
                   m,
                   ": Gravity model not balanced to required tolerance within maximum iterations.\n"))
  }
  
  flow_est <- flows_result$flows |>
    rename(flow_est = flow)
  
  flow_check <- wip_flows |>
    full_join(flow_est) |>
    replace_na(list(flow = 0,
                    flow_est = 0)) |>
    filter(!is.na(tt))
  
  # Check if average travel times are within tolerance
  mean_tt_est_1 <- sum(flow_check$flow_est * flow_check$tt) / 
    sum(flow_check$flow_est)
  
  if(abs(mean_tt_obs - mean_tt_est_1) < tolerance_calibration) {
    return(list(flows = flow_check, beta = beta_1, iterations = 1))
  }
  
  # move to iteration 2
  m <- 2
  message(paste0("\nCalibration iteration ", m))
  
  beta_2 <- beta_1 * mean_tt_est_1 / mean_tt_obs
  
  # calculate new friction factors
  wip_flows <- wip_flows |>
    mutate(friction = exp(-1 * beta_2 * tt)) 
  
  # recalculate trip matrix
  flow_result <- grvty_balancing(od_zones = zone_totals,
                                 friction = wip_flows,
                                 zone_id = "id",
                                 zone_o = "o",
                                 zone_d = "d",
                                 friction_o_id = "o_id",
                                 friction_d_id = "d_id",
                                 friction_factor = "friction",
                                 tolerance = tolerance_balancing,
                                 max_iter = max_iter_balancing)
  
  convergence <- flows_result$convergence
  
  if(nrow(convergence) >= max_iter_balancing) {
    warning(paste0("Calibration iteration ",
                   m,
                   ": Gravity model not balanced to required tolerance within maximum iterations.\n"))
  }
  
  flow_est <- flow_result$flows |>
    rename(flow_est = flow)
  
  flow_check <- wip_flows |>
    full_join(flow_est, by = join_by(o_id, d_id)) |>
    replace_na(list(flow = 0,
                    flow_est = 0)) |>
    filter(!is.na(tt))
  
  # Check if average travel times are within tolerance
  mean_tt_est_2 <- sum(flow_check$flow_est * flow_check$tt) / 
    sum(flow_check$flow_est)
  
  if(abs(mean_tt_obs - mean_tt_est_2) < tolerance_calibration) {
    return(list(flows = flow_check, beta = beta_2, iterations = 2))
  }
  
  # move to iteration 3
  m <- 3
  betas <- c(beta_1, beta_2)
  mean_tt_ests <- c(mean_tt_est_1, mean_tt_est_2)
  done <- FALSE
  
  while (!done) {
    
    betas[m] <- ((mean_tt_obs - mean_tt_ests[m-2])*betas[m-1] - (mean_tt_obs - mean_tt_ests[m-1])*betas[m-2]) /
      (mean_tt_ests[m-1] - mean_tt_ests[m-2])
    
    # calculate new friction factors
    wip_flows <- wip_flows |>
      mutate(friction = exp(-1 * betas[m] * tt))
    
    flow_result <- grvty_balancing(od_zones = zone_totals,
                                   friction = wip_flows,
                                   zone_id = "id",
                                   zone_o = "o",
                                   zone_d = "d",
                                   friction_o_id = "o_id",
                                   friction_d_id = "d_id",
                                   friction_factor = "friction",
                                   tolerance = tolerance_balancing,
                                   max_iter = max_iter_balancing)
    
    convergence <- flows_result$convergence
    
    if(nrow(convergence) >= max_iter_balancing) {
      warning(paste0("Calibration iteration ",
                     m,
                     ": Gravity model not balanced to required tolerance within maximum iterations.\n"))
    }
    
    flow_est <- flow_result$flows |>
      rename(flow_est = flow)
    
    flow_check <- wip_flows |>
      full_join(flow_est, by = join_by(o_id, d_id)) |>
      replace_na(list(flow = 0,
                      flow_est = 0)) |>
      filter(!is.na(tt))
    
    # Check if average travel times are within tolerance
    mean_tt_ests[m] <- sum(flow_check$flow_est * flow_check$tt) / 
      sum(flow_check$flow_est)
    
    if(abs(mean_tt_obs - mean_tt_ests[m]) < tolerance_calibration) {
      return(list(flows = flow_check, beta = betas[m], iterations = m))
      done = TRUE
    }
    if(m >= max_iter_calibration) {
      return(list(flows = flow_check, beta = betas[m], iterations = m))
      done = TRUE
    }
    m <- m+1
  }
  
}


#####Compare betas

kable(betas_table,digits=3)
