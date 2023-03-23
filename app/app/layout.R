# Choices for drop-downs
plot_types <- c(
  "Timeseries" = "timeseries",
  "Monthly Trend" = "monthly"
)

scenarios <- c(
  "SSP1-2.6" = "ssp126",
  "SSP2-4.5" = "ssp245",
  "SSP3-7.0" = "ssp370",
  "SSP5-8.5" = "ssp585"
)

variables <- c(
  "Potential ET" = "penman",
  "Avg. Air Temperature" = "tas",
  "Max. Air Temperature" = "tasmax",
  "Min. Air Temperature" = "tasmin",
  "Wind Speed" = "sfcWind",
  "Precipitation" = "pr"
)

reference <- c(
  "End of Century" = "end",
  "Mid Century" = "mid", 
  "Reference Period" = "reference"
)

# Shiny versions prior to 0.11 should use class = "modal" instead.
future_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = 'calc(95vh - 1px)',
  
  h2("CMIP6 Projections"),
  
  selectInput("variable", "Variable", variables),
  selectInput("scenario", "Scenario", scenarios),
  selectInput("plot_type", "Plot Type", plot_types),
  selectInput("reference", "Time Period", reference),
  radioButtons(
    "map_type", "Map Type:",
    c("Period Average" = FALSE,
      "Difference from Normal" = TRUE),
    inline = TRUE
  ),
  plotOutput("outPlot", height = 500),
)

gridmet_variables <- c(
"Energy Release Component"="erc",
"Potential ET (Alfalfa)"="etr",
"Potential ET (Grass)"="pet",
"Precipitation"="pr",
"Maximum Relative Humidity"="rmax",
"Minimum Relative Humidity"="rmin",
# "Specific Humidity"="sph",
# "Wind Direction"="th",
"Minumum Temperature"="tmmn",
"Maximum Temperature"="tmmx",
"Vapor Pressure Deficit"="vpd",
"Wind Speed"="vs"
)


time_periods <- c("Annual", tolower(month.abb)) %>%
  magrittr::set_names(c("Annual", month.name))

# Shiny versions prior to 0.11 should use class = "modal" instead.
historical_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = 'calc(95vh - 1px)',
  
  h2("GridMET Trends"),
  
  selectInput("historical_variable", "Variable", gridmet_variables, selected="pr"),
  selectInput("historical_period", "Time Period", time_periods),

  plotOutput("historical_outPlot", height = 500),
)

# Shiny versions prior to 0.11 should use class = "modal" instead.
report_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = 'calc(95vh - 1px)',
  
  h2("Create a Report"),
  
  selectInput("report_gridmet", "Historical Variables", gridmet_variables, multiple=TRUE),
  selectInput("report_cmip", "Future Variables", variables, multiple=TRUE),
  selectInput("report_scenarios", "Emission Scenarios", scenarios, multiple=TRUE, selected = c("ssp245", "ssp370")),
  downloadButton(outputId = "report", label = "Generate Report:"),
)