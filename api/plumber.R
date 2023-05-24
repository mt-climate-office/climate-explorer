# plumber.R
source("./crud.R")

# dotenv::load_dot_env("../.env")
args <- list(
  RPostgres::Postgres(),
  host = "fcfc-mesonet-db.cfc.umt.edu",
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD"),
  port = 5433
)

#* Get historical GridMET dat for a county or watershed.
#* @get /data/historical/<loc_type:str>/<location:str>/<variable:str>/
#* @param loc_type:str The geometry type. Can either be 'huc' or 'county'
#* @param location:str The county FIPS code to gather data for.
#* @param variable:str The variable to gather data for. 
#* @param date_start:str The date (as YYYY-MM-DD) to start gathering data for.
#* @param date_end:str The date (as YYYY-MM-DD) to stop gathering data at.
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer csv
function(
    loc_type, location, variable, date_start="1970-01-01", 
    date_end="2021-01-01", us_units=TRUE
) {
  
  con <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(con))
  
  dplyr::tbl(con, RPostgres::Id(schema = "historical", table = loc_type)) %>% 
    dplyr::filter(id == location, variable == !!variable) %>%
    dplyr::filter(date > date_start, date < date_end) %>% 
    dplyr::collect() %>% 
    convert_units(variable, us_units) %>% 
    dplyr::mutate(value = round(value, 3))
}

#* Get CMIP6 projections at the county or watershed scale.
#* @get /data/future/<loc_type:str>/<location:str>/<variable:str>/
#* @param loc_type:str The geometry type. Can either be 'huc' or 'county'
#* @param location:str The county FIPS code to gather data for.
#* @param variable:str The variable to gather data for. 
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @param table_type:str Options include 'timeseries' to clean the data for timeseries
#* @param scenarios:str A comma separated list of scenarios to include. Options include ssp126, ssp245, ssp370, ssp585
#* analysis or 'monthly' to aggregate the data by month.
#* @serializer csv
function(
    loc_type, location, variable, diff=FALSE, us_units=TRUE, 
    table_type=c("timeseries", "monthly"), scenarios="ssp126,ssp245,ssp370,ssp585"
) {
  table_type = match.arg(table_type)
  
  con <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(con))

  f = switch(
    table_type,
    "timeseries" = prep_for_timeseries,
    "monthly" = prep_for_monthly_plot,
    function(x) {
      print("There is an error...")
    }
  )
  
  scenarios = stringr::str_replace_all(scenarios, ",", "|")
  scenarios = glue::glue("{scenarios}|historical")
  
  dplyr::tbl(con, RPostgres::Id(schema = "future", table = loc_type)) %>% 
    f(location=location, v=variable, us_units = us_units, scenarios=scenarios) %>% 
    plumber::as_attachment(glue::glue("future_{location}_{variable}_{table_type}.csv"))
  
}


# #* Write new data to database.
# #* @post /upload/<table:str>/
# #* @parser csv
# #* @param table:str The database table you would like to upload data to.
# #* @param f:file The .csv file you would like to write to the database.  
# function(
#     table, f
# ) {
#   
# }