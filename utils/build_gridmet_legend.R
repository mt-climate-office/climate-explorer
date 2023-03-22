tidyr::crossing(
  variable = c("erc", "etr", "pet", "pr", "rmax", "rmin", "sph", "srad", "tmmn", "tmmx", "vpd", "vs"),
  time = c("annual", tolower(month.abb))
) %>% 
  dplyr::mutate(
    r = glue::glue("https://data.climate.umt.edu/mt-normals/cog/{variable}/{time}_mean.tif")
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(terra::rast(r))) %>%
  dplyr::mutate(
    mn = terra::global(r, fun="min", na.rm = T) %>% as.numeric(),
    mx = terra::global(r, fun="max", na.rm = T) %>% as.numeric()
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-r) %>% 
  readr::write_csv("./data/gridmet_legend.csv")
