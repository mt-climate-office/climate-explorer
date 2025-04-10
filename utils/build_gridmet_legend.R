dat <- tidyr::crossing(
  variable = c(# "erc", "etr", "pet", "pr", "rmax", "rmin", "sph", "srad", "tmmn", "tmmx", "vpd", "vs",
               "afg", "bgr", "evi", "ltr", "pet_m16", "pfgnpp", "shrnpp", "trenpp", "afgnpp", "et_m16",
               "gpp", "ndvi", "pfg", "shr", "tre"),
  time = c("annual", tolower(month.abb))
) %>% 
  dplyr::filter(
    !((variable %in% c("afg", "bgr", "ltr", "pfgnpp", "shrnpp", "trenpp", "afgnpp", "pfg", "shr", "tre")) & (time %in% tolower(month.abb)))
  ) %>%
  dplyr::mutate(
    r = glue::glue("~/data/blm_project/gee_data/{variable}/{time}_mean.tif")
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(terra::rast(r))) %>%
  dplyr::mutate(
    mn = terra::global(r, fun="min", na.rm = T) %>% as.numeric(),
    mx = terra::global(r, fun="max", na.rm = T) %>% as.numeric()
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-r)

readr::read_csv("./app/app/data/gridmet_legend.csv") %>% 
  dplyr::bind_rows(dat) %>% 
  readr::write_csv("./app/app/data/historical_legend.csv")
  # readr::write_csv("./app/app/data/historical_legend.csv")
