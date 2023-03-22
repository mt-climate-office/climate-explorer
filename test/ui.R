library(leaflet)

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

build_panel <- function(name) {
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
    width = 500, height = 'calc(95vh - 1px)',
    
    h2("Climate Explorer"),
    
    selectInput(glue::glue("{name}_variable"), "Variable", variables),
    selectInput(glue::glue("{name}_scenario"), "Scenario", scenarios),
    selectInput(glue::glue("{name}_plot_type"), "Plot Type", plot_types),
    selectInput(glue::glue("{name}_reference"), "Time Period", reference),
    radioButtons(
     glue::glue("{name}_map_type"), "Map Type:",
     c("Period Average" = FALSE,
       "Difference from Normal" = TRUE),
     inline = TRUE
    ),
    plotOutput(glue::glue("{name}_outPlot"), height = 500),
   )
}

panel <- build_panel("test")

navbarPage("Montana Climate Office", id="nav",
  tabPanel("Climate Projections",
          div(class="outer",
              tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
                includeScript("gomap.js")
              ),
              # If not using custom CSS, set height of leafletOutput to a number instead of percent
              leafletOutput("map_future", width="100%", height="100%"),
              # build_panel("test1"),
              tags$div(id="cite",
                       'Data from', tags$em('NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'), ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
              )
          )
  ),
  tabPanel("Historical Trends",
          div(class="outer",
              tags$head(
                # Include our custom CSS
                includeCSS("styles.css"),
                includeScript("gomap.js")
              ),
              
              # If not using custom CSS, set height of leafletOutput to a number instead of percent
              leafletOutput("map_historical", width="100%", height="100%"),
              # build_panel("test2"),
              tags$div(id="cite",
                       'Data from', tags$em('NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'), ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
              )
          )),
  tabPanel("Generate Report")
)
