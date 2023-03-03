library(magrittr)

bounds <- sf::read_sf("~/data/cmip6/conus_huc2.fgb")

readr::read_csv("~/data/cmip6/gddp-cmip6-files.csv") %>% 
  dplyr::transmute(
    f = fileURL, 
    name = basename(f) %>% 
      tools::file_path_sans_ext()
  ) %>% 
  tidyr::separate(
    name, 
    c("variable", "drop1", "model", "time", "drop2", "drop3", "year"), 
    sep = "_"
  ) %>% 
  dplyr::select(-dplyr::starts_with("drop")) %>% 
  dplyr::filter(
    model %in% c("ACCESS-CM2", ADSFASDFSDA Look up other models )
  ) 
  
  
parse_raster <- function(f, out_dir) {
  
  name = file.path(out_dir, basename(f))
  
  terra::rast(f) %>% 
    terra::crop(bounds) %>% 
    terra::writeCDF(name)
  
}
