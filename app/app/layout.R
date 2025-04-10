# Choices for drop-downs
plot_types <- c(
  "Timeseries" = "timeseries",
  "Monthly Trend" = "monthly"
)

scenarios <- c(
  "Moderating Emissions (SSP1-2.6)" = "ssp126",
  "Middle of the Road (SSP2-4.5)" = "ssp245",
  "High Emissions (SSP3-7.0)" = "ssp370",
  "Accelerating Emissions (SSP5-8.5)" = "ssp585"
)


variables <- list(
  `CMIP6 Variables` = c(
    "Potential ET" = "penman",
    "Avg. Air Temperature" = "tas",
    "Max. Air Temperature" = "tasmax",
    "Min. Air Temperature" = "tasmin",
    "Wind Speed" = "sfcWind",
    "Precipitation" = "pr"
  ),
  `Derived Variables` = c(
    "Days Above 90°F" = "above90",
    "Freeze Free Days" = "freeze-free",
    "Growing Degree Days" = "gdd",
    "Consecutive Wet Days" = "con-wet",
    "Consecutive Dry Days" = "con-dry",
    "Dry Days" = "dry-days",
    "Wet Days" = "wet-days"
  )
)

reference <- c(
  "End of Century" = "end",
  "Mid Century" = "mid", 
  "Reference Period" = "reference"
)

# cool_title <-       absolutePanel(
#   id = "controlsTitle", class = "panel panel-default", fixed = TRUE,
#   draggable = FALSE, top = 65, right = 10, bottom = "auto",
#   width = 300, height = 100,
#   
#   h2(id="coolTitle", "A cool title!")
# )

# Shiny versions prior to 0.11 should use class = "modal" instead.
future_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = "90vh",
  
  h2("CMIP6 Projections"),
  
  selectInput("variable", "Variable", variables, selected = "tas"),
  selectInput("scenario", "Scenario", scenarios, multiple = TRUE, selected = c("ssp245", "ssp370")),
  selectInput("plot_type", "Plot Type", plot_types),
  selectInput("reference", "Time Period", reference),
  radioButtons(
    "map_type", "Map Type:",
    c("Period Average" = FALSE,
      "Difference from Normal" = TRUE),
    inline = TRUE
  ),
  plotly::plotlyOutput("outPlot", height = 500),
)

historical_variables <- list(
  `GridMET Variables`=c(
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
  ),
  `Satellite Indicators`=c(
    "RAP Annual Forb and Grass Cover"="afg",
    "RAP Bare Ground Cover"="bgr",
    "RAP Perennial Forb and Grass Cover"="pfg",
    "RAP Shrub Cover"="shr",
    "RAP Tree Cover"="tre",
    "MOD13 EVI"="evi",
    "MOD13 NDVI"="ndvi",
    "MOD16 ET"="et_m16",
    "MOD16 PET"="pet_m16",
    "MOD17 GPP"="gpp",
    "RAP Forb and Grass NPP"="afgnpp",
    "RAP Perennial Forb and Grass NPP"="pfgnpp",
    "RAP Shrub NPP"="shrnpp",
    "RAP Tree NPP"="trenpp"
  )
)


time_periods <- c("Annual", tolower(month.abb)) %>%
  magrittr::set_names(c("Annual", month.name))

# Shiny versions prior to 0.11 should use class = "modal" instead.
historical_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = "90vh",
  
  h2("Climate and Vegetation Trends"),
  
  selectInput("historical_variable", "Variable", historical_variables, selected="pr"),
  selectInput("historical_period", "Time Period", time_periods),

  plotly::plotlyOutput("historical_outPlot", height = 500),
)

# Shiny versions prior to 0.11 should use class = "modal" instead.
report_panel <- absolutePanel(
  id = "controls", class = "panel panel-default", fixed = TRUE,
  draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
  width = 500, height = "90vh",
  
  h2("Create a Report"),
  
  selectInput("report_gridmet", "Historical Variables", historical_variables, multiple=TRUE),
  selectInput("report_cmip", "Future Variables", variables, multiple=TRUE),
  selectInput("report_scenarios", "Emission Scenarios", scenarios, multiple=TRUE, selected = c("ssp245", "ssp370")),
  checkboxInput("monthly", "Show Monthly Plots", FALSE),
  span(h4("Selected Location: "), textOutput("report_text")),
  downloadButton(outputId = "report", label = "Generate Report"),
  br(),
  textOutput("report_loading")
)