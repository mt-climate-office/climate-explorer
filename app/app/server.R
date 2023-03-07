library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
source("./plot.R")

function(input, output, session) {

  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addTiles() %>%
      setView(lng = -107.5, lat = 47, zoom = 7) %>% 
      addPolygons(fill = TRUE, color = "black", weight = 2, popup = ~cnty_nm, layerId = ~cnty_fp)
  })

  output$outPlot <- renderPlot({
    click <- input$map_shape_click
    if (is.null(click)) {
      return(placeholder_graph())
    }
    plt <- ifelse(input$plot_type == "timeseries", make_timeseries_plot, make_monthly_plot)
    
    return(plt(dat, click$id, input$variable, TRUE))
  })


  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    r <- rasters %>%
      dplyr::filter(variable == input$variable,
                    scenario == input$scenario,
                    period == input$reference) %>%
      head(1) %>% 
      dplyr::pull(f) %>%
      terra::rast() %>% 
      terra::crop(counties, mask = TRUE, snap = "out", touches=TRUE) 

    leafletProxy("map", data = counties) %>%
      removeTiles(layerId = "geo") %>%
      addRasterImage(x=r, layerId = "geo") %>%
      # addLegend(position = "bottomleft") %>%
      # addLegend("bottomleft", layerId="colorLegend", colors = brewer.pal(10, "RdBu"), labels = letters[1:10]) %>%
      setView(lng = -107.5, lat = 47, zoom = 7)
      # pal=pal, values=colorData, title=colorBy,
  })
}
