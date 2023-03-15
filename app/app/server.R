library(leaflet)
library(leafem)
source("./plot.R")

function(input, output, session) {

  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -107.5, lat = 47, zoom = 7) %>% 
      addPolygons(
        data = counties, fill = TRUE, color = "black", weight = 2, 
        popup = ~name, layerId = ~id, group = "Counties") %>% 
      # addPolygons(data = hucs, fill = TRUE, color = "black", weight = 2, 
      #             popup = ~name, layerId = ~id, group = "HUCs") %>% 
      addLayersControl(
        baseGroups = c("Counties"), # , "HUCs"
        options = layersControlOptions(
          collapsed = FALSE,
          position = "topleft"
        )
      )
    
  })

  output$outPlot <- renderPlot({
    click <- input$map_shape_click 
    if (is.null(click)) {
      return(placeholder_graph())
    }
    plt <- ifelse(input$plot_type == "timeseries", make_timeseries_plot, make_monthly_plot)
    
    click <- click$id %>% 
      stringr::str_split("_") %>% 
      unlist()
    
    dat <- dplyr::tbl(con, RPostgres::Id(schema = "future", table = click[[1]])) 
    
    return(plt(dat, click[[2]], input$variable, TRUE, input$map_type=="diff"))
  })

  observeEvent(input$map_type, {
    if (input$map_type == "raw") {
      choices <- c(
        "End of Century" = "end",
        "Mid Century" = "mid", 
        "Reference Period" = "reference"
      )
    } else {
      choices <- c(
        "End of Century" = "end",
        "Mid Century" = "mid"
      )
    }
    
    updateSelectInput(
      session, "reference",
      choices = choices
    )
  })
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    info <- handle_raster_plotting_logic(input)

    leafletProxy("map", data = counties) %>%
      removeTiles(layerId = "geo") %>%
      leafem::addGeotiff(
        url = info$r,
        autozoom = FALSE,
        colorOptions = leafem::colorOptions(
          palette = info$pal,
          breaks = seq(info$mn, info$mx, length.out=500),
          domain = c(info$mn, info$mx)
        ),
        layerId = "geo"
      ) %>%
      addLegend(
        position="bottomleft",
        layerId="colorLegend",
        colors = info$pal(10),
        labels = info$labels,
        title = legend_title(input$variable)
      ) %>%
      setView(lng = -107.5, lat = 47, zoom = 6)
  })
}

  