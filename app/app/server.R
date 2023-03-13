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
    
    return(plt(dat, click[[2]], input$variable, TRUE))
  })


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    print(input$reference)
    # r <- rasters %>%
    #   dplyr::filter(variable == input$variable,
    #                 scenario == input$scenario,
    #                 period == input$reference) %>%
    #   head(1) %>% 
    #   dplyr::pull(f) # %>%
      # terra::rast() #  %>% 
      # terra::crop(counties, mask = TRUE, snap = "out", touches=TRUE) 
    
    r <- glue::glue(
      "https://data.climate.umt.edu/mca/cmip/derived/{input$variable}_{input$scenario}_{input$reference}.tif"
    )
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = terra::rast(r) %>% 
        terra::values()
    )
    
    leafletProxy("map", data = counties) %>%
      removeTiles(layerId = "geo") %>%
      leafem::addGeotiff(
        url = r,
        autozoom = FALSE
      ) %>%
      # addRasterImage(x=r, layerId = "geo") %>%
      # addLegend(position = "bottomleft") %>%
      # addLegend("bottomleft", layerId="colorLegend", colors = brewer.pal(10, "RdBu"), labels = letters[1:10]) %>%
      setView(lng = -107.5, lat = 47, zoom = 7)
      # pal=pal, values=colorData, title=colorBy,
  })
}

  