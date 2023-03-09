library(magrittr)

dotenv::load_dot_env(".env")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  # host = Sys.getenv("POSTGRES_HOSTNAME"),
  host = "db",
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

counties <- sf::read_sf("./data/counties.shp") %>% 
  dplyr::select(id=cnty_fp, name=cnty_nm) %>%
  dplyr::mutate(id = glue::glue("county_{id}"))

hucs <- sf::read_sf("./data/mt_hucs.geojson") %>% 
  dplyr::mutate(id = glue::glue("huc_{id}"))

rasters <- list.files("./data", pattern = ".tif", full.names = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(base = basename(f) %>% 
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(base, c("variable", "scenario", "period"))
