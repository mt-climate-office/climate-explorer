library(magrittr)

cog_to_nc <- function(path, ) {

  dat <- 
    list.files(path, full.names = T, pattern = "pr.tif") %>% 
    grep(".json", ., value = T, invert = T) %>% 
    tibble::tibble(f = .) %>% 
    dplyr::mutate(base = basename(f) %>% tools::file_path_sans_ext()) %>% 
    tidyr::separate(base, c("model", "scenario", "drop", "variable"), sep = "_") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(r = list(terra::rast(f)))
  
  
  dat %>% 
    dplyr::select(-c(f, drop)) %>% 
    tidyr::pivot_wider(names_from = scenario, values_from = r) %>% 
    tidyr::pivot_longer(dplyr::starts_with("ssp"), names_to = "scenario") %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(r = list(c(historical, value))) %>% 
    dplyr::select(-historical, -value) -> a

    
}

path = '~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly/'
