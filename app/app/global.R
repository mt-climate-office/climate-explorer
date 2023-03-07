library(dplyr)

dotenv::load_dot_env(".env")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("POSTGRES_HOSTNAME"),
  # host = "db",
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

dat <- dplyr::tbl(con, RPostgres::Id(schema = "data", table = "counties")) 
counties <- sf::read_sf("./data/counties.shp")
rasters <- list.files("./data", pattern = ".tif", full.names = T) %>% 
  tibble::tibble(f = .) %>% 
  dplyr::mutate(base = basename(f) %>% 
                  tools::file_path_sans_ext()) %>%
  tidyr::separate(base, c("variable", "scenario", "period"))
