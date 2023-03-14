library(magrittr)

cluster <- multidplyr::new_cluster(10)
multidplyr::cluster_library(cluster, c("magrittr", "terra"))

out_dir = "~/MCO_onedrive/General/nexgddp_cmip6_montana/data-derived/nexgddp_cmip6/monthly/"

list.files("~/data/cmip6/daily/", full.names = T) %>% 
  stringr::str_subset(".aux.json", negate = T) %>% 
  tibble::tibble(f = .) %>%
  dplyr::mutate(
    out_dir = out_dir,
    fun = ifelse(stringr::str_detect(f, "pr|hargreaves|penman"), "sum", "mean")
  ) %>% 
  dplyr::rowwise() %>% 
  multidplyr::partition(cluster) %>% 
  dplyr::summarise(
    f = f,
    r = list(
      terra::rast(f) %>% 
        terra::tapp("yearmonths", fun) %>% 
        terra::writeRaster(file.path(out_dir, basename(f)), overwrite = T)
    )
  ) %>% 
  dplyr::collect()


