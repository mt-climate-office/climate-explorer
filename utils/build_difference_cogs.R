library(magrittr)

out_dir = './app/app/data/difference'

list.files("./app/app/data", full.names = T, pattern = ".tif") %>% 
  tibble::tibble(
    f = .,
    base = basename(.) %>% 
      tools::file_path_sans_ext()
  ) %>% 
  tidyr::separate(base, c("variable", "scenario", "period"), sep = "_") %>% 
  dplyr::filter(variable %in% c("penman", "pr", "sfcWind", "tas", "tasmax", "tasmin")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(r = list(terra::rast(f))) %>% 
  dplyr::mutate(
    r = dplyr::case_when(
      variable == "penman" ~ list(r / 25.4),
      variable == "pr" ~ list(r / 25.4),
      variable == "sfcWind" ~ list(r * 2.237),
      TRUE ~ list((r - 273.15) * 1.8 + 32)
    )
  ) %>% 
  dplyr::select(-f) %>% 
  tidyr::pivot_wider(
    names_from = period, 
    values_from = r
  ) %>% 
  tidyr::pivot_longer(-c(variable, scenario, reference)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(difference = list(value - reference)) %>% 
  dplyr::transmute(
    out_name = file.path(out_dir, glue::glue("{variable}_{scenario}_{name}.tif")),
    r = list(difference)
  ) %>% 
  dplyr::summarise(
    out = list(normals::write_as_cog(r, out_name))
  )
