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

#* Plot out data from the iris dataset
#* @get /plot/timeseries/<county>/<variable>
#* @param diff:bool Whether or not the plot should be a difference from normal. 
#* @serializer png
function(county, variable, diff){
  print(county)
  print(variable)
  print(diff)
  dplyr::tbl(con, RPostgres::Id(schema = "future", table = "county")) %>% 
    make_timeseries_plot(county, variable, TRUE, diff) %>% 
    print()

}