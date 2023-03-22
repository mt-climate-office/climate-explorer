library(leaflet)
library(leafem)
library(ggplot2)
source("./plot.R")

function(input, output, session) {

  ## Interactive Map ###########################################
  # Create the map
  output$map_future <- output$map_historical <- renderLeaflet({
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
    click <- input$map_future_shape_click 
    
    if (is.null(click)) {
      return(placeholder_graph())
    }
    
    click <- click$id %>% 
      stringr::str_split("_") %>% 
      unlist()
    
    dat <- glue::glue(
        "http://blm_api/data/future/{click[[2]]}/{input$variable}/?diff={input$map_type}&table_type={input$plot_type}"
      ) %>%
        readr::read_csv() %>% 
        factor_scenario() 

    if (input$plot_type == "monthly") {
      plt <- dat %>% 
        dplyr::mutate(month = factor(month, levels = month.abb)) %>%
        make_monthly_plot(TRUE, input$map_type) 
    } else {
      plt <- make_timeseries_plot(dat, TRUE, input$map_type)
    }
    return(plt)
  })
  
  output$historical_outPlot <- renderPlot({
    click <- input$map_historical_shape_click 
    
    if (is.null(click)) {
      return(placeholder_graph())
    }
    
    click <- click$id %>% 
      stringr::str_split("_") %>% 
      unlist()
    
    print(click)
    dat <- glue::glue(
      "http://blm_api/data/historical/{click[[2]]}/{input$historical_variable}/"
    ) %T>% print() %>% 
      readr::read_csv() 
    
    if (input$historical_period != "Annual") {
      dat <- dat %>% 
        dplyr::filter(
          lubridate::month(date) == which(tolower(month.abb) == tolower(input$historical_period))
        )
    } else {
      dat %>% 
        dplyr::group_by(year = lubridate::year(date)) %>% 
        dplyr::summarise(
          value = ifelse(variable %in% c("pr", "pet", "etr"), sum(value), mean(value)), 
        ) %>% 
        dplyr::mutate(year = as.Date())
    }
    
    plt <- dat %>% 
      ggplot(aes(x=date, y=value)) + 
        geom_point() + 
        geom_line() + 
        geom_smooth(method = "lm") + 
        theme_minimal() 
    
    return(plt)
  })

  observeEvent(input$map_type, {
    
    if (!as.logical(input$map_type)) {
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

    leafletProxy("map_future", data = counties) %>%
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
  
  historical_trigger <- reactive({
    list(input$historical_variable, input$historical_period)
  })
  
  observeEvent(ignoreInit = TRUE, historical_trigger(), {

    url = glue::glue("https://data.climate.umt.edu/mt-normals/cog/{input$historical_variable}/{tolower(input$historical_period)}_mean.tif")
    info <- gridmet_legend(input)
    leafletProxy("map_historical", data = counties) %>%
      removeTiles(layerId = "geo") %>%
      leafem::addGeotiff(
        url = url,
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
        title = legend_title(input$historical_variable)
      ) %>%
      setView(lng = -107.5, lat = 47, zoom = 6)
  })
}

  