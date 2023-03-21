# plumber.R
source("./crud.R")

dotenv::load_dot_env("../.env")
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "fcfc-mesonet-staging.cfc.umt.edu",
  # host = "db",
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

#* Plot out data from the iris dataset
#* @get /data/historical/<location:str>/<variable:str>/
#* @param location:str The county FIPS code to gather data for.
#* @param variable:str The variable to gather data for. 
#* @param date_start:str The date (as YYYY-MM-DD) to start gathering data for.
#* @param date_end:str The date (as YYYY-MM-DD) to stop gathering data at.
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer csv
function(
    location, variable, date_start="1970-01-01", 
    date_end="2021-01-01", us_units=TRUE
) {
  
  dplyr::tbl(con, RPostgres::Id(schema = "historical", table = "county")) %>% 
    dplyr::filter(id == location, variable == !!variable) %>%
    dplyr::filter(date > date_start, date < date_end) %>% 
    dplyr::collect() %>% 
    convert_units(variable, us_units)
}

#* Plot out data from the iris dataset
#* @get /data/historical/<location:str>/<variable:str>/
#* @param location:str The county FIPS code to gather data for.
#* @param variable:str The variable to gather data for. 
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @param data_type:str Options include 'timeseries' to clean the data for timeseries
#* analysis or 'monthly' to aggregate the data by month.
#* @serializer csv
function(location, variable, diff=FALSE, us_units=TRUE){
  
  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    prep_for_monthly_plot(location, variable, us_units = us_units)
  
}

#* Plot out data from the iris dataset
#* @get /data/monthly/<location:str>/<variable:str>/
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer csv
function(location, variable, diff=FALSE, us_units=TRUE){

  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    prep_for_monthly_plot(location, variable, us_units = us_units)
  
}

#* Plot out data from the iris dataset
#* @get /data/timeseries/<location:str>/<variable:str>/
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer csv
function(location, variable, diff=FALSE, us_units=TRUE){
  
  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    prep_for_timeseries(location, variable, us_units = us_units)
  
}