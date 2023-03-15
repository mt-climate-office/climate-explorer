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

navbarPage("Montana Climate Office", id="nav",
  tabPanel("Climate Projections",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 65, left = "auto", right = 10, bottom = "auto",
        width = 500, height = 'calc(95vh - 1px)',

        h2("Climate Explorer"),

        selectInput("variable", "Variable", variables),
        selectInput("scenario", "Scenario", scenarios),
        selectInput("plot_type", "Plot Type", plot_types),
        selectInput("reference", "Time Period", reference),
        radioButtons(
          "map_type", "Map Type:",
          c("Period Average" = "raw",
            "Difference" = "diff"),
          inline = TRUE
        ),
        plotOutput("outPlot", height = 500),
      ),

      tags$div(id="cite",
        'Data from', tags$em('NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'), ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
      )
    )
  ),
  tabPanel("Historical Trends"),
  tabPanel("Generate Report")
)
