# plumber.R
source("./crud.R")

# dotenv::load_dot_env("../.env")
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "fcfc-mesonet-staging.cfc.umt.edu",
  # host = "db",
  dbname = Sys.getenv("POSTGRES_DBNAME"),
  user = Sys.getenv("POSTGRES_USER"),
  password = Sys.getenv("POSTGRES_PASSWORD")
)

#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot projected change in monthly average climate conditions. 
#* @get /plot/monthly/<location>/<variable>/
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer png
function(location, variable, diff=FALSE, us_units=TRUE){

  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    prep_for_monthly_plot(location, variable, us_units) %>%
    make_monthly_plot(us_units, diff) %>% 
    print()

}

#* Plot a timeseries of projected changes in climate conditions.
#* @get /plot/timeseries/<location:str>/<variable:str>/
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @param us_units:bool Whether to use U.S. units or SI Units
#* @serializer png
function(location, variable, diff=FALSE, us_units=TRUE){

  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    prep_for_timeseries(location, variable, us_units) %>% 
    make_timeseries_plot(us_units, diff) %>% 
    print()
  
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