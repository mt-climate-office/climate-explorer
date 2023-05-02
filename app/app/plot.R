library(magrittr)
library(ggplot2)

name_mapper <- list(
  "tas"="Temperature",
  "tasmin" = "Minimum Temperature",
  "tasmax" = "Maximum Temperature",
  "sfcWind" = "Wind Speed",
  "pr" = "Precipitation",
  "rsds" = "Short Wave Radiation", 
  "rlds" = "Long Wave Radiation",
  "hurs" = "Relative Humidity",
  "penman" = "Potential ET"
)

orig_units <- list(
  "hurs" = "percent",
  "pr" = "mm",
  "rsds" = "W m-2",
  "rlds" = "W m-2",
  "sfcWind" = "m/s",
  "tas" = "kelvin",
  "tasmax" = "kelvin", 
  "tasmin" = "kelvin",
  "penman" = "mm"
)

units_us <- list(
  "hurs" = "percent",
  "pr" = "in",
  "rsds" = "W m-2",
  "rlds" = "W m-2",
  "sfcWind" = "mi/hr",
  "tas" = "degF",
  "tasmax" = "degF", 
  "tasmin" = "degF",
  "penman" = "in"
)

units_metric <- list(
  "hurs" = "percent",
  "pr" = "cm",
  "rsds" = "W m-2",
  "rlds" = "W m-2",
  "sfcWind" = "m/s",
  "tas" = "degC",
  "tasmax" = "degC", 
  "tasmin" = "degC",
  "penman" = "mm"
)

colors = 
  c("Historical Emissions" = rgb(0,0,0,
                                 maxColorValue = 255),
    "transparent",
    " "  = "transparent",
    "  "  = "transparent",
    "SSP1-2.6" = rgb(34,46,77,
                                             maxColorValue = 255),
    "SSP2-4.5" = rgb(223,146,71,
                                           maxColorValue = 255),
    "SSP3-7.0" = rgb(187,54,51,
                                       maxColorValue = 255),
    "SSP5-8.5" = rgb(122,41,40,
                                               maxColorValue = 255)
    
  )

scenario_code_to_long <- function(x) {
  switch(
    x,
    "hist" = 'Historical Emissions',
    "ssp126" = "Moderating Emissions\n(SSP1-2.6)",
    "ssp245" = "Middle of the Road\n(SSP2-4.5)",
    "ssp370" = "High Emissions\n(SSP3-7.0)",
    "ssp585" = "Accelerating Emissions\n(SSP5-8.5)",
  )
}

factor_scenario <- function(dat) {
  # readr::write_csv(dat, "./test.csv")
  # dat <- readr::read_csv("./app/app/test.csv")
  dplyr::mutate(
    dat,
    scenario = stringr::str_remove(scenario, ".*\n|") %>% 
      stringr::str_replace_all("\\(|\\)", "") %>% 
      factor(
        levels = c("Historical Emissions",
                   "SSP1-2.6",
                   "SSP2-4.5",
                   "SSP3-7.0",
                   "SSP5-8.5")
      )
  )
}

build_titles <- function(location, variable, us_units, monthly = FALSE) {

  if (us_units) {
    unit <- units_us[[variable]]
  } else {
    unit <- units_metric[[variable]]
  }
  
  if (monthly) {
    pretext <- "Monthly Climate Projections"
  } else {
    pretext <- "Climate Projections"
  }
  
  list(
    "y" = glue::glue("{name_mapper[[variable]]} [{unit}]"),
    "title" = glue::glue("{pretext} of\n{name_mapper[[variable]]} for {location}")
  )
}

convert_units <- function(dat, variable, us_units) {
  
  dat %>%
    dplyr::mutate(
      value = units::set_units(value, !!orig_units[[variable]]),
      value = units::set_units(value, !!units_metric[[variable]])
    ) %>% 
    {
      if (us_units) {
        dplyr::mutate(
          ., value = units::set_units(value, !!units_us[[variable]]) %>% 
            units::drop_units()
        )
      } else {
        units::drop_units(.)
      }
    } 
}

make_timeseries_plot <- function(dat, us_units=TRUE, difference=FALSE, size=14) {
  
  if (difference) {
    avg <- dat %>% 
      dplyr::filter(year <= 2020, year >=1991) %>%
      dplyr::pull(value) %>% 
      mean()
    
    dat %<>% 
      dplyr::mutate(
        upper = upper - avg,
        lower = lower - avg,
        value = value - avg
      )
  } 
  
  titles <- build_titles(dat$location[[1]], dat$variable[[1]], us_units)
  
  plt <- ggplot(dat, aes(x=year, color=scenario, fill=scenario)) +
    geom_line(aes(y=value)) + 
    geom_ribbon(aes(ymin=lower, ymax = upper),                
                color = NA,
                alpha = 0.5,
                linewidth = 0.25) +
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(1950,2100,25),
                       limits = c(1950,2100)) +
    labs(x = NULL,
         y = titles[["y"]],
         title = titles[["title"]] 
    ) +
    theme_minimal(size) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
      legend.title = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "bottom",
      legend.key.width = unit(0.25,"in"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.margin=unit(c(0.1,0.2,0.1,0.1), "in"),
      plot.title = element_text(hjust=0.5)
    ) +
    ggplot2::guides(colour = guide_legend(ncol = 2)) +
    coord_cartesian(clip = "off")
  
  if (difference) {
    plt = plt + geom_hline(yintercept=0)
  }
  
  return(plt)
}

make_monthly_plot <- function(dat, us_units=TRUE, difference=FALSE, size=14) {
  
  titles <- build_titles(dat$location[[1]], dat$variable[[1]], us_units, monthly = T)
  
  if (difference) {
    
    avg <-  dat %>% 
      dplyr::select(-upper, -lower, -grp) %>% 
      dplyr::distinct() %>% 
      dplyr::filter(scenario == "Historical Emissions") %>% 
      dplyr::select(month, avg=value)
    
    dat %<>%
      dplyr::filter(scenario != "Historical Emissions") %>% 
      dplyr::left_join(avg, by="month") %>% 
      dplyr::mutate(
        upper = upper - avg, 
        lower = lower - avg, 
        value = value - avg
      )
  }
  
  plt <- dat %>%
    dplyr::mutate(grp = stringr::str_replace(grp, "\\(", "\n(")) %>%
    ggplot(aes(x=month, color=scenario)) + 
    geom_pointrange(aes(y=value, ymin=lower, ymax=upper), position = position_dodge(width=0.25)) + 
    geom_line(aes(y=value, group=scenario)) +
    facet_grid(rows="grp") +
    labs(x = NULL,
         y = titles[["y"]],
         title = titles[["title"]]
    ) +
    theme_minimal(size) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
      legend.title = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "bottom",
      legend.key.width = unit(0.25,"in"),
      # axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.margin=unit(c(0.1,0.2,0.1,0.1), "in"),
      plot.title = element_text(hjust=0.5)
    ) + 
    coord_cartesian(clip="off")
    # ggplot2::guides(colour = guide_legend(ncol = 2)) +
    # coord_cartesian(clip = "off")
  
  if (difference) {
    plt = plt + geom_hline(yintercept=0)
  }
  
  return(plt)
}

placeholder_graph <- function() {
  
  tibble::tibble(x=1, y=1, txt="Click a county to plot data!") %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_text(aes(label=txt), size=8) + 
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

make_historical_plot <- function(dat, variable, period="Annual") {
  name <- dat$name[[1]]
  
  if (period != "Annual") {
    dat <- dat %>% 
      dplyr::filter(
        lubridate::month(date) == which(tolower(month.abb) == tolower(period))
      )
  } else {
    dat <- dat %>% 
      dplyr::mutate(date = lubridate::floor_date(date, "year")) %>%
      dplyr::group_by(date) %>% 
      dplyr::summarise(
        value = dplyr::if_else(
          dplyr::first(variable %in% c("pr", "pet", "etr")), 
          sum(value), 
          mean(value)
        ) 
      ) 
  }
  
  p_value <- lm(dat$value ~ dat$date) %>% 
    summary() %>% 
    purrr::pluck("coefficients") %>% 
    # as.numeric() %>%
    purrr::pluck(-1)
  
  plt <- dat %>% 
    ggplot(aes(x=date, y=value)) + 
    geom_point() + 
    geom_line() + 
    theme_minimal() + 
    labs(
      x="Year", 
      y=legend_title(variable),
      title = glue::glue("Trend in {name} {stringr::str_to_title(period)} {legend_title(variable)}"),
    ) + 
    theme(
      plot.title = element_text(hjust=0.5)
    )
  
  if (p_value <= 0.05) {
    plt <- plt +
      geom_smooth(formula = y ~ x, method = "lm") + 
      labs(subtitle = "Trend per Decade is Statistically Significant") + 
      theme(
        plot.subtitle = element_text(face="bold", hjust=0.5)
      )
  }
  
  return(plt)
}


clean_pltly_legend <- function(.pltly_obj, .new_legend = c()) {
  library(purrr)
  library(stringr)
  # Cleans up a plotly object legend, particularly when ggplot is facetted
  
  assign_leg_grp <- function(.legend_group, .leg_nms) {
    # Assigns a legend group from the list of possible entries
    # Used to modify the legend settings for a plotly object
    
    leg_nms_rem <- .leg_nms
    
    parse_leg_nms <- function(.leg_options) {
      # Assigns a .leg_name, if possible
      # .leg_options is a 2-element list: 1 = original value; 2 = remaining options
      
      if (is.na(.leg_options)) {
        .leg_options
      } else if(length(leg_nms_rem) == 0) {
        # No more legend names to assign
        .leg_options
      } else {
        # Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]
        
        leg_nm_new
      }
      
    }
    
    .legend_group %>% 
      map(~ parse_leg_nms(.))
    
  }
  
  simplify_leg_grps <- function(.legendgroup_vec) {
    # Simplifies legend groups by removing brackets, position numbers and then de-duplicating
    
    leg_grp_cln <-
      map_chr(.legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))
    
    modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)
    
  }
  
  pltly_obj_data <-
    .pltly_obj$x$data
  
  pltly_leg_grp <-
    # pltly_leg_grp is a character vector where each element represents a legend group. Element is NA if legend group not required or doesn't exist
    pltly_obj_data%>% 
    map(~ pluck(., "legendgroup")) %>% 
    map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
    # Elements where showlegend = FALSE have legendgroup = NULL. 
    
    simplify_leg_grps() %>% 
    
    assign_leg_grp(.new_legend) 
  
  pltly_obj_data_new <-
    pltly_obj_data %>% 
    map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))
  # i.e. showlegend set to FALSE when is.na(pltly_leg_grp), TRUE when not is.na(pltly_leg_grp)
  
  .pltly_obj$x$data <- pltly_obj_data_new
  
  .pltly_obj
  
}
