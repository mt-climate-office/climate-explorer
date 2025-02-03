library(magrittr)

setwd("~/git/report-builder/app/app")

blm <- sf::read_sf("https://data.climate.umt.edu/mt-normals/fgb/explorer/blm.fgb")
counties <- sf::read_sf("https://data.climate.umt.edu/mt-normals/fgb/explorer/counties.fgb")
hucs <- sf::read_sf("https://data.climate.umt.edu/mt-normals/fgb/explorer/hucs.fgb")
tribes <- sf::read_sf("https://data.climate.umt.edu/mt-normals/fgb/explorer/tribes.fgb")

"http://fcfc-mesonet-staging.cfc.umt.edu/blm_api/data/historical/county/30107/pr/"
"http://fcfc-mesonet-staging.cfc.umt.edu/blm_api/data/future/county/30027/tas/"

change_units <- function(value, variable) {
  switch(
    variable, 
    "pr" = value / 25.4,
    "penman" = value /25.4,
    "sfcWind" = value * 2.237,
    "vs" = value * 2.237,
    "tas" = (value - 273.15) * 1.8 + 32,
    "tasmin" = (value - 273.15) * 1.8 + 32,
    "tasmax" = (value - 273.15) * 1.8 + 32,
    "tmmn" = (value - 273.15) * 1.8 + 32,
    "tmmx" = (value - 273.15) * 1.8 + 32, 
    "etr" = (value / 25.4)*365,
    "pet" = (value / 25.4)*365,
    "vpd" = value * 10,
    value
  )
}

tidy_id <- function(shp) {
  shp %>% 
    tidyr::separate(id, c("loc_type", "db_id", "name"), sep = "_")
}

make_historical_dat <- function(dat, variable, period="Annual") {
  name <- dat$name[[1]]
  
  if (period != "Annual") {
    dat <- dat %>% 
      dplyr::filter(
        lubridate::month(date) == which(tolower(month.abb) == tolower(period))
      )
  } else {
    dat <- dat %>% 
      dplyr::mutate(date = lubridate::floor_date(date, "year")) %>%
      dplyr::group_by(date) %>% 
      dplyr::summarise(
        value = dplyr::if_else(
          dplyr::first(variable %in% c("pr", "pet", "etr")), 
          sum(value), 
          mean(value)
        ) 
      ) 
  }
  
  p_value <- lm(dat$value ~ dat$date) %>% 
    summary() %>% 
    purrr::pluck("coefficients") %>% 
    # as.numeric() %>%
    purrr::pluck(-1)
  
}


make_monthly_dat <- function(dat, us_units=TRUE, difference=FALSE, size=14) {
  
  titles <- build_titles(dat$location[[1]], dat$variable[[1]], us_units, monthly = T)
  
  if (difference) {
    
    avg <-  dat %>% 
      dplyr::select(-upper, -lower, -grp) %>% 
      dplyr::distinct() %>% 
      dplyr::filter(scenario == "Historical Emissions") %>% 
      dplyr::select(month, avg=value)
    
    dat %<>%
      dplyr::filter(scenario != "Historical Emissions") %>% 
      dplyr::left_join(avg, by="month") %>% 
      dplyr::mutate(
        upper = upper - avg, 
        lower = lower - avg, 
        value = value - avg
      )
  }
}


make_timeseries_dat <- function(dat, us_units=TRUE, difference=FALSE, size=14) {
  
  if (difference) {
    avg <- dat %>% 
      dplyr::filter(year <= 2020, year >=1991) %>%
      dplyr::pull(value) %>% 
      mean()
    
    dat %<>% 
      dplyr::mutate(
        upper = upper - avg,
        lower = lower - avg,
        value = value - avg
      )
  } 
}


dat <- dplyr::bind_rows(
  hucs, tribes, counties, blm
) %>% 
  tidy_id() %>%
  sf::st_drop_geometry() 

future_df <-  tidyr::crossing(
    endpoint = "future",
    variable = c("penman", "tas", "tasmax", "tasmin", "sfcWind", "pr", 
                 "above90", "freeze-free", "gdd", "con-wet", "con-dry",
                 "dry-days", "wet-days"),
    diff = c(TRUE, FALSE),
    table_type = c("monthly", "timeseries")
  )

historical_df <- tibble::tibble(
    variable = c("erc", "etr", "pet", "pr", "rmax", "rmin", "tmmn", "tmmx", "vpd", "vs", 
                 "afg", "bgr", "pfg", "shr", "tre", "evi", "ndvi", "et_m16", "pet_m16", "gpp", 
                 "afgnpp", "pfgnpp", "shrnpp", "trenpp")
  )

get_historical_df <- function(variable, loc_type, db_id) {
  out_name <- glue::glue(
    "./data/for_js_plots/historical/{variable}_{loc_type}_{db_id}.csv"
  )
  
  if (file.exists(out_name)) {
    print(glue::glue("Reading {out_name} from file."))
    readr::read_csv(out_name, show_col_types = FALSE)
  } else {
    print(glue::glue("Creating {out_name} from API."))
    readr::read_csv(
      glue::glue("http://fcfc-mesonet-staging.cfc.umt.edu/blm_api/data/historical/{loc_type}/{db_id}/{variable}/"),
      show_col_types = FALSE
    ) %>%
      readr::write_csv(out_name)
  }
}

get_future_df <- function(variable, diff, table_type, loc_type, db_id) {
  out_name <- glue::glue(
    "./data/for_js_plots/future/{variable}_{loc_type}_{db_id}_{table_type}_{diff}.csv"
  )
  
  if (file.exists(out_name)) {
    print(glue::glue("Reading {out_name} from file."))
    readr::read_csv(out_name, show_col_types = FALSE)
  } else {
    print(glue::glue("Creating {out_name} from API."))
    glue::glue(
      "http://fcfc-mesonet-staging.cfc.umt.edu/blm_api/data/future/{loc_type}/{db_id}/{variable}/"
    ) %>%
      httr::GET(
        query = list(
          diff = diff,
          table_type = table_type
        )
      ) %>%
      httr::content(show_col_type=FALSE) %>%
      readr::write_csv(out_name)
  }
}

dat %>%
  tidyr::crossing(future_df) %>%
  dplyr::select(-name, -endpoint) %>%
  purrr::pmap(get_future_df)

dat %>%
  tidyr::crossing(historical_df) %>%
  dplyr::select(-name, -endpoint) %>%
  purrr::pmap(get_historical_df)





