list.files("./app/app/data", pattern = ".tif", full.names = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(name = basename(f) %>% 
                  tools::file_path_sans_ext()) %>% 
  tidyr::separate(name, c("variable", "scenario", "period"), sep = "_") %>%
  dplyr::mutate(
    mn = terra::rast(f) %>% terra::global("min") %>% magrittr::extract2(1),
    mx = terra::rast(f) %>% terra::global("max") %>% magrittr::extract2(1)
  ) %>% 
  dplyr::group_by(variable) %>% 
  dplyr::summarize(mn = min(mn), mx = max(mx)) %>% 
  dplyr::filter(
    variable %in% c("penman", "pr", "sfcWind", "tas", "tasmax", "tasmin")
  ) %>% 
  dplyr::mutate(
    mn = dplyr::case_when(
      variable == "penman" ~ mn / 25.4,
      variable == "pr" ~ mn / 25.4,
      variable == "sfcWind" ~ mn * 2.237,
      TRUE ~ (mn - 273.15) * 1.8 + 32
    ),
    mx = dplyr::case_when(
      variable == "penman" ~ mx / 25.4,
      variable == "pr" ~ mx / 25.4,
      variable == "sfcWind" ~ mx * 2.237,
      TRUE ~ (mx - 273.15) * 1.8 + 32
    )
  )
