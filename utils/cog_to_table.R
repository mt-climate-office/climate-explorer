library(magrittr)


reassign_name_as_time <- function(x) {
  names(x) <- terra::time(x)
  return(x)
}

build_table <- function(path, shp) {

  out <- list.files(path, full.names = T, include.dirs = FALSE) %>% 
    grep(".json", ., value = T, invert = T) %>% 
    grep("/derived", ., value = T, invert = T) %>% 
    tibble::tibble(f = .) %>% 
    dplyr::mutate(base = basename(f) %>% tools::file_path_sans_ext()) %>% 
    tidyr::separate(base, c("model", "scenario", "drop", "variable"), sep = "_") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(r = list(terra::rast(f))) %>% 
    dplyr::mutate(r = list(reassign_name_as_time(r))) %>% 
    dplyr::select(-c(f, drop)) 
  
  out %<>%
    dplyr::mutate(r = list(normals::spat_summary(r, shp, attr_id = "county_name", name_to = "date", fun = "mean"))) %>%
    tidyr::unnest(cols=r) %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(-geometry) %>% 
    dplyr::mutate(date = lubridate::as_date(date))
  
    tidyr::pivot_wider(names_from = scenario, values_from = r) %>% 
    tidyr::pivot_longer(dplyr::starts_with("ssp"), names_to = "scenario") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(r = list(c(historical, value))) %>% 
    dplyr::select(-historical, -value)
  
  summarized <- dat %>% 
    dplyr::filter(model != "MIROC6", variable != "huss") %>%
    dplyr::rename(c(sub_region = county_name)) %>% 
    dplyr::mutate(region_type = "county") %>% 
    dplyr::select(region_type, sub_region, model, scenario, variable, date, value) 

  return(summarized)  
}

path = '~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly/'
shp <- urbnmapr::get_urbn_map(map = "counties", sf = TRUE) %>% 
  sf::st_transform(crs = sf::st_crs(4326)) %>% 
  dplyr::filter(state_name == "Montana") %>% 
  dplyr::select(county_name)