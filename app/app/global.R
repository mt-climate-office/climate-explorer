library(magrittr)

counties <- sf::read_sf("./data/counties.shp") %>% 
  dplyr::select(id=cnty_fp, name=cnty_nm) %>%
  dplyr::mutate(id = glue::glue("county_{id}_{name}"))

hucs <- sf::read_sf("./data/mt_hucs.geojson") %>% 
  dplyr::mutate(id = glue::glue("huc_{id}_{name} Watershed"))

tribes <- sf::read_sf("./data/tribes.geojson")


blm <- sf::read_sf("./data/blm.geojson") %>%
  dplyr::mutate(id = glue::glue("blm_{id}_{name} District"))

rasters <- list.files("./data", pattern = ".tif", full.names = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(base = basename(f) %>% 
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(base, c("variable", "scenario", "period"), sep = "_")

legend_info <- readr::read_csv("./data/legend.csv", show_col_types = FALSE)
legend_gridmet <- readr::read_csv("./data/gridmet_legend.csv", show_col_types = FALSE)

API_URL <- ifelse(
  Sys.getenv("IN_DOCKER") == "",
  "http://fcfc-mesonet-staging.cfc.umt.edu/blm_api/",
  "http://blm_api/"
)

pals <- function(x) {
  switch(
    x, 
    "penman" = viridisLite::viridis,
    "pr" = viridisLite::viridis,
    "tas" = viridisLite::inferno,
    "tasmin" = viridisLite::inferno,
    "tasmax" = viridisLite::inferno,
    "sfcWind" = viridisLite::cividis,
    "above90" = viridisLite::magma,
    "con-dry" = viridisLite::cividis,
    "con-wet" = viridisLite::mako,
    "dry-days" =  viridisLite::cividis,
    "freeze-free" =  viridisLite::mako,
    "gdd" =  viridisLite::viridis,
    "wet-days" =  viridisLite::mako
  )
}

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

legend_title <- function(variable, units=TRUE) {
  switch(
    variable,
    "pr" = ifelse(units, "Precipitation [in]", "Precipitation"),
    "penman" = ifelse(units, "Reference ET [in]", "Reference ET"),
    "sfcWind" = ifelse(units, "Wind Speed [mph]", "Wind Speed"),
    "tas" = ifelse(units, "Temperature [degF]", "Temperature"),
    "tasmin" = ifelse(units, "Min. Temperature [degF]", "Min. Temperature"),
    "tasmax" = ifelse(units, "Max. Temperature [degF]", "Max. Temperature"),
    "tmmx" = ifelse(units, "Max. Temperature [degF]", "Max. Temperature"),
    "tmmn" = ifelse(units, "Min. Temperature [degF]", "Min. Temperature"),
    "etr" = ifelse(units, "Reference ET [in]", "Reference ET"),
    "pet" = ifelse(units, "Reference ET [in]", "Reference ET"),
    "vs" = ifelse(units, "Wind Speed [mph]", "Wind Speed"),
    "vpd" = ifelse(units, "Vapor Pressure Deficit [mbar]", "Vapor Pressure Deficit"),
    "rmax" = ifelse(units, "Max. Relative Humidiy [%]", "Max. Relative Humidiy"),
    "rmin" = ifelse(units, "Min. Relative Humidity [%]", "Min. Relative Humidity"),
    "sph" = ifelse(units, "Specific Humidity [%]", "Specific Humidity"),
    "erc" = ifelse(units, "Energy Release","Energy Release"),
    "above90" = "Days Above 90°F",
    "con-dry" = "Consecutive Dry Days",
    "con-wet" = "Consecutive Wet Days",
    "dry-days" = "Dry Days",
    "freeze-free" = "Freeze-Free Days",
    "gdd" = "Growing Degree Days",
    "wet-days" = "Wet Days"
  )
}

text_units <- function(variable) {
  switch(
    variable,
    "pr" = "inches",
    "penman" =  "inches",
    "sfcWind" = "miles per hour",
    "tas" = "degF",
    "tasmin" = "degF",
    "tasmax" = "degF",
    "tmmx" = "degF",
    "tmmn" = "degF",
    "etr" =  "inches",
    "pet" =  "inches",
    "vs" = "miles per hour",
    "vpd" = "milibars",
    "rmax" = "percent",
    "rmin" = "percent",
    "sph" = "percent",
    "erc" = "",
    "above90" = "days",
    "con-dry" = "days",
    "con-wet" = "days",
    "dry-days" = "days",
    "freeze-free" = "days",
    "gdd" = "days",
    "wet-days" = "days"
  )
}

handle_raster_plotting_logic <- function(input) {
  
  filt_val = ifelse(input$map_type, "diff", "raw")
  vals <- legend_info %>% 
    dplyr::filter(variable == input$variable,
                  type == filt_val)
  
  if (input$map_type) {
    pal <- RColorBrewer::brewer.pal(10, "RdBu") %>% 
      rev() %>% 
      colorRampPalette()
    mx <- ceiling(vals$mx)
    mn <- mx * -1
    breaks = seq(mn, mx, length.out=10)
    labels <- seq(mn, mx, length.out=10) %>%
      round(2)
  } else {
    mn <- floor(vals$mn)
    mx <- ceiling(vals$mx)
    pal <- pals(input$variable)
    breaks = seq(mn, mx, length.out=10)
    labels <- breaks %>%
      change_units(input$variable) 
    round_val = ifelse(
      max(labels) - min(labels) < 10, 2, 0
    )
    labels <- round(labels, round_val)
  }
  return(list(
    "mn" = mn,
    "mx" = mx,
    "pal" = pal, 
    "breaks" = breaks,
    "labels" = labels,
    "variable" = input$variable, 
    "reference" = input$reference,
    "type" = input$map_type
  ))
}

placeholder_graph <- function() {
  
  tibble::tibble(x=1, y=1, txt="Click a county to plot data!") %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_text(aes(label=txt), size=10) + 
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}


historical_colors  = list(
  'pr'='YlGnBu',
  'pet'='OrRd',
  'etr'='OrRd',
  'tmmn'='Blues',
  'tmmx'='Reds',
  'rmax'='PuBuGn',
  'rmin'='PuBuGn',
  'th'='PuRd',
  'erc'='PuRd',
  'vpd'='OrRd',
  'vs'='RdPu',
  'sph'='Oranges',
  'srad'='YlOrRd',
  "afg"='greens',
  "bgr"='greens',
  "pfg"='greens',
  "shr"='greens',
  "tre"='greens',
  "evi"='greens',
  "ndvi"='greens',
  "et_m16"='OrRd',
  "pet_m16"='OrRd',
  "gpp"='PuRd',
  "afgnpp"='OrRd',
  "pfgnpp"='OrRd',
  "shrnpp"='OrRd',
  "trenpp='OrRd'"
)

gridmet_legend <- function(input) {

  vals <- dplyr::filter(
    legend_gridmet, 
    variable == input$historical_variable,
    time == tolower(input$historical_period)
  )
  mn <- floor(vals$mn)
  mx <- ceiling(vals$mx)
  pal <- RColorBrewer::brewer.pal(9, historical_colors[[input$historical_variable]]) %>% 
    rev() %>% 
    colorRampPalette()
  
  breaks = seq(mn, mx, length.out=10)
  labels <- breaks %>% 
    change_units(input$historical_variable) 
  
  round_val = ifelse(
    max(labels) - min(labels) < 10, 2, 0
  )
  labels <- round(labels, round_val)
  
  return(list(
    "mn" = mn,
    "mx" = mx,
    "pal" = pal, 
    "breaks" = breaks,
    "labels" = labels
  ))
}

get_layers <- function(scenario, variable, reference, type) {
  if (type) {
    out <- glue::glue("https://data.climate.umt.edu/mca/cmip/derived/difference/{variable}_{scenario}_{reference}.tif")
  } else {
    out <- glue::glue("https://data.climate.umt.edu/mca/cmip/derived/{variable}_{scenario}_{reference}.tif")
  }
  return(out)
}

scenario_switch <- function(x) {
  switch(
    x, 
    "ssp126" = "SSP1-2.6", 
    "ssp245" = "SSP2-4.5", 
    "ssp370" = "SSP3-7.0", 
    "ssp585" = "SSP5-8.5"
  )
}

add_layers <- function(leaf, scenario, info, with_legend = FALSE) {

  leaf %>%
    leafem::addGeotiff(
      url = get_layers(scenario, info$variable, info$reference, info$type),
      autozoom = FALSE,
      colorOptions = leafem::colorOptions(
        palette = info$pal,
        breaks = seq(info$mn, info$mx, length.out=500),
        domain = c(info$mn, info$mx)
      ),
      group = scenario_switch(scenario),
      layerId = scenario_switch(scenario)
    ) %>%
    setView(lng = -107.5, lat = 47, zoom = 7) %>%
    addLegend(
        position="bottomleft",
        layerId="colorLegend",
        colors = info$pal(10),
        labels = info$labels,
        title = legend_title(info$variable)
    ) 
}
