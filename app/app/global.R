library(magrittr)

counties <- sf::read_sf("./data/counties.shp") %>% 
  dplyr::select(id=cnty_fp, name=cnty_nm) %>%
  dplyr::mutate(id = glue::glue("county_{id}"))

hucs <- sf::read_sf("./data/mt_hucs.geojson") %>% 
  dplyr::mutate(id = glue::glue("huc_{id}"))

rasters <- list.files("./data", pattern = ".tif", full.names = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(base = basename(f) %>% 
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(base, c("variable", "scenario", "period"))

legend_info <- readr::read_csv("./data/legend.csv")

pals <- function(x) {
  switch(
    x, 
    "penman" = viridisLite::viridis,
    "pr" = viridisLite::viridis,
    "tas" = viridisLite::inferno,
    "tasmin" = viridisLite::inferno,
    "tasmax" = viridisLite::inferno,
    "sfcWind" = viridisLite::cividis,
  )
}

change_units <- function(value, variable) {
  
  switch(
    variable, 
    "pr" = value / 25.4,
    "penman" = value /25.4,
    "sfcWind" = value * 2.237,
    (value - 273.15) * 1.8 + 32
  )
}

legend_title <- function(variable) {
  switch(
    variable,
    "pr" = "Precipitation [in]",
    "penman" = "Reference ET [in]",
    "sfcWind" = "Wind Speed [mph]",
    "Temperature [degF]"
  )
}

handle_raster_plotting_logic <- function(input) {
  
  if (input$map_type) {
    r <- glue::glue(
      "https://data.climate.umt.edu/mca/cmip/derived/difference/{input$variable}_{input$scenario}_{input$reference}.tif"
    )
  } else {
    r <- glue::glue(
      "https://data.climate.umt.edu/mca/cmip/derived/{input$variable}_{input$scenario}_{input$reference}.tif"
    )
  }

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
    labels <- round(seq(mn, mx, length.out=10)) %>%
      change_units(input$variable) %>% 
      round()
  }
  return(list(
    "r" = r,
    "mn" = mn,
    "mx" = mx,
    "pal" = pal, 
    "breaks" = breaks,
    "labels" = labels
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
