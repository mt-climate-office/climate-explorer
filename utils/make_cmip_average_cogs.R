library(magrittr)

data_dir = "~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly"
files <- list.files(data_dir, full.names = T, pattern = ".tif") %>% 
  stringr::str_subset(".json", negate = TRUE)

subset_mean <- function(r, start_year, end_year, func) {
  
  years <- terra::time(r) %>% 
    stringr::str_sub(end=4) %>% 
    as.numeric() %>% 
    {which(. %in% start_year:end_year)}

  terra::subset(r, years) %>%
    terra::tapp(index = "years", fun = func) %>% 
    terra::app("mean")
}


out_dir = "./data"

tibble::tibble(f = files) %>% 
  dplyr::mutate(name = basename(f) %>% 
                  tools::file_path_sans_ext()) %>% 
  tidyr::separate(name, c("model", "time", "drop1", "variable"), sep = "_") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(terra::rast(f))) %>% 
  dplyr::select(-c(drop1, f)) %>%
  tidyr::pivot_wider(
    names_from = time, 
    values_from = r
  ) %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("ssp")
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    r = list(c(historical, value))
  ) %>% 
  dplyr::select(
    model, variable, name, r
  ) %>% 
  dplyr::filter(
    model != "MIROC6",
    variable != "huss",
  ) %>% 
  dplyr::mutate(f = ifelse(variable %in% c("hargreaves", "penman", "pr"), "sum", "mean")) %>%
  dplyr::mutate(
    reference = list(subset_mean(r, 1991, 2020, f)), 
    mid = list(subset_mean(r, 2040, 2069, f)),
    end = list(subset_mean(r, 2070, 2099, f))
  ) %>% 
  dplyr::group_by(
      variable, scenario=name
  ) %>% 
  dplyr::summarise(
    reference = list(terra::app(terra::rast(reference), "mean")),
    mid = list(terra::app(terra::rast(mid), "mean")),
    end = list(terra::app(terra::rast(end), "mean"))
  ) %>% 
  tidyr::pivot_longer(
    cols = c(reference, mid, end)
  )  %>% 
  dplyr::mutate(
    out_name = file.path(out_dir, glue::glue("{variable}_{scenario}_{name}.tif"))
  ) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    out = list(normals::write_as_cog(value, out_name))
  )
