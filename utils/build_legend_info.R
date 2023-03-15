list.files("./app/app/data", pattern = ".tif", full.names = T, recursive = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(name = basename(f) %>% 
                  tools::file_path_sans_ext(),
                type = dirname(f) %>% 
                  basename() %>% 
                  {ifelse(. == "difference", "diff", "raw")}
                ) %>% 
  tidyr::separate(name, c("variable", "scenario", "period"), sep = "_") %>%
  dplyr::mutate(
    mn = terra::rast(f) %>% terra::global("min") %>% magrittr::extract2(1),
    mx = terra::rast(f) %>% terra::global("max") %>% magrittr::extract2(1)
  ) %>% 
  dplyr::group_by(variable, type) %>% 
  dplyr::summarize(mn = min(mn), mx = max(mx)) %>% 
  dplyr::filter(
    variable %in% c("penman", "pr", "sfcWind", "tas", "tasmax", "tasmin")
  ) %>% 
  readr::write_csv("./app/app/data/legend.csv")
