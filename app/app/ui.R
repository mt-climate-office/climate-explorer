library(leaflet)
source("./layout.R")

navbarPage(
  "Montana Climate Office",
  id = "nav",
  tabPanel(
    "Climate Projections",
    div(
      class = "outer",
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map_future", width = "100%", height = "100%"),
      
      future_panel,
      
      tags$div(
        id = "cite",
        'Data from',
        tags$em(
          'NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'
        ),
        ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
      )
    )
  ),
  tabPanel(
    "Historical Trends",
    div(
      class = "outer",
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map_historical", width = "100%", height = "100%"),
      
      historical_panel,
      
      tags$div(
        id = "cite",
        'Data from',
        tags$em(
          'NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'
        ),
        ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
      )
    )
  ),
  tabPanel(
    "Generate Report",
    div(
      class = "outer",
      tags$head(# Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")),
      
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map_report", width = "100%", height = "100%"),
      
      report_panel,
      
      tags$div(
        id = "cite",
        'Data from',
        tags$em(
          'NASA Global Daily Downscaled Projections, CMIP6 (NEX-GDDP-CMIP6)'
        ),
        ' Thrasher, B., Wang, W., Michaelis, A. et al. (2022).'
      )
    )
  ),
)
