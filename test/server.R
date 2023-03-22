library(leaflet)
library(leafem)
library(ggplot2)


function(input, output, session) {
  
  ## Interactive Map ###########################################
  # Create the map
  output$map_future <- output$map_historical <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -107.5, lat = 47, zoom = 7) 
    
  })
  
  output$outPlot <- renderPlot({
    click <- input$map_future_shape_click 
    
    hist(rnorm(50, sample.int(100, 1), sample.int(5, 1)))
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
}

