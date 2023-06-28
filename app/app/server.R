library(leaflet)
library(leafem)
library(ggplot2)
source("./plot.R")


function(input, output, session) {

  ## Interactive Map ###########################################
  # Create the map
  output$map_future <- output$map_historical <- output$map_report <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -107.5, lat = 47, zoom = 7) %>% 
      addPolygons(
        data = counties, fill = TRUE, color = "black", weight = 2, 
        popup = ~name, layerId = ~id, group = "Counties") %>% 
      addPolygons(data = hucs, fill = TRUE, color = "black", weight = 2,
                  popup = ~name, layerId = ~id, group = "HUCs") %>%
      addPolygons(data = tribes, fill = TRUE, color = "black", weight = 2,
                  popup = ~name, layerId = ~id, group = "Tribal Lands") %>%
      addPolygons(data = blm, fill = TRUE, color = "black", weight = 2,
                  popup = ~name, layerId = ~id, group = "BLM Districts") %>%
      addLayersControl(
        overlayGroups = c("Counties", "HUCs", "Tribal Lands", "BLM Districts"), # , "HUCs"
        options = layersControlOptions(
          collapsed = FALSE,
          position = "topleft"
        )
      ) %>%
      hideGroup("HUCs") %>% 
      hideGroup("Tribal Lands") %>%
      hideGroup("BLM Districts")
    
  })

  output$outPlot <- plotly::renderPlotly({
    click <- input$map_future_shape_click 
    if (is.null(click)) {
      return(placeholder_graph())
    }
    
    click <- click$id %>% 
      stringr::str_split("_") %>% 
      unlist()
    
    scenarios <- paste(input$scenario, collapse = ",")
    print(glue::glue(
      "{API_URL}/data/future/{click[[1]]}/{click[[2]]}/{input$variable}/"
    ))
    dat <- glue::glue(
        "{API_URL}/data/future/{click[[1]]}/{click[[2]]}/{input$variable}/"
      ) %>%
        httr::GET(
          query = list(
            diff = input$map_type,
            table_type = input$plot_type,
            scenarios = scenarios
          )
        ) %>%
        httr::content(show_col_types = FALSE) %>% 
        factor_scenario() 
    
    print(dat)

    if (input$plot_type == "monthly") {
      plt <- dat %>% 
        dplyr::mutate(month = factor(month, levels = month.abb)) %>%
        make_monthly_plot(TRUE, input$map_type, size=10) 
    } else {
      plt <- make_timeseries_plot(dat, TRUE, input$map_type, size=10)
    }
    return(
      plotly::ggplotly(plt) %>%
        plotly::layout(
          legend = list(
            orientation = "h",   # show entries horizontally
            xanchor = "center",  # use center of legend as anchor
            x = 0.5,
            title = "",
            yaxis = list(automargin=TRUE)
          )
        ) %>%
        clean_pltly_legend() %>% 
        add_logo_to_plotly()
    )
  })
  
  output$historical_outPlot <- plotly::renderPlotly({
    click <- input$map_historical_shape_click 
    
    if (is.null(click)) {
      return(placeholder_graph())
    }
    
    click <- click$id %>% 
      stringr::str_split("_") %>% 
      unlist()
    
    dat <- glue::glue(
      "{API_URL}/data/historical/{click[[1]]}/{click[[2]]}/{input$historical_variable}/"

    ) %>% 
      readr::read_csv(show_col_types = FALSE) 
    
    plt <- make_historical_plot(dat, input$historical_variable, input$historical_period)
    return(
      plotly::ggplotly(plt) %>%
             add_logo_to_plotly()
    )
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
  
  # titleInput <- eventReactive(list(input$variable, input$historical_variable), {
  #   if (!is.null(input$variable)) {
  #     list(inputId = "variables", value = input$variable)
  #   } else if (!is.null(input$historical_variable)) {
  #     list(inputId = "gridmet_variables", value = input$historical_variables)
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # observeEvent(titleInput(), {
  #   print('asdf')
  #   output$coolTitle <- renderText("this is  a test")
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    input$nav
    
    info <- handle_raster_plotting_logic(input)

    leafletProxy("map_future", data = counties) %>% 
      removeTiles(
        layerId = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
      ) %>%
      add_layers("ssp126", info) %>%
      add_layers("ssp245", info) %>% 
      add_layers("ssp370", info, TRUE) %>% 
      add_layers("ssp585", info, with_legend = TRUE) %>%
      updateLayersControl(
        addOverlayGroups = c("Counties", "HUCs", "Tribal Lands", "BLM Districts"), # , "HUCs"
        addBaseGroups = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"),
        options = layersControlOptions(
          collapsed = FALSE,
          position = "topleft"
        )
      ) 
      
  })
  
  observe({
    # Add this here so it is triggered when the tab switches. 
    input$nav
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
      setView(lng = -107.5, lat = 47, zoom = 7)
  })
  
  output$report_text <- renderText({
    click <- input$map_report_shape_click 
    if (is.null(click)) {
      return(paste("No Location Selected. Click a location to generate a report."))
    } else {
      click <- click$id %>% 
        stringr::str_split("_") %>% 
        unlist()
      return(paste(click[[3]]))
    }
  })

  output$report <- downloadHandler(
    filename = function() {
      click <- input$map_report_shape_click 
      click <- click$id %>% 
        stringr::str_split("_") %>% 
        unlist()      
      
      glue::glue('{click[[2]]}_climReport_{stringr::str_replace_all(Sys.Date(), "-", "")}.pdf')
    },
    content = function(file) {
      showModal(modalDialog("Generating report, please wait. This could take a minute or two!", footer=NULL))
      click <- input$map_report_shape_click 

      click <- click$id %>% 
        stringr::str_split("_") %>% 
        unlist()
      
      if (length(input$report_gridmet) == 0) {
        showNotification("You must select a gridMET variable!", type = "error")
        return()
      }
      quarto::quarto_render(
        "blm_template.qmd", 
        execute_params = list(
          scenarios = input$report_scenarios,
          gridmet = input$report_gridmet,
          cmip = input$report_cmip,
          location_type = click[[1]],
          location_id = click[[2]],
          location_name = click[[3]]
        )
      )
      
      file.copy("qmd_output.pdf", file)
      on.exit(removeModal())
      
    }
  )
}
  