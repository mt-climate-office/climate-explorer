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
  "penman" = "Potential ET",
  "erc" = "Energy Release Component",
  "etr" = "Potential ET (Alfalfa)",
  "pet" = "Potential ET (Grass)",
  "rmax" = "Maximum Relative Humidity",
  "rmin" = "Minimum Relative Humidity",
  "sph" = "Specific Humidity",
  "th" = "Wind Direction",
  "tmmn" = "Minumum Temperature",
  "tmmx" = "Maximum Temperature",
  "vpd" = "Vapor Pressure Deficit",
  "vs" = "Wind Speed",
  "above90" = "Days Above 90°F",
  "con-dry" = "Consecutive Dry Days",
  "con-wet" = "Consecutive Wet Days",
  "dry-days" = "Dry Days",
  "freeze-free" = "Freeze-free Days",
  "gdd" = "Growing Degree Days",
  "wet-days" = "Wet Days",
  "afg" = "RAP Annual Forb and Grass Cover",
  "bgr" = "RAP Bare Ground Cover",
  "pfg" = "RAP Perennial Forb and Grass Cover",
  "shr" = "RAP Shrub Cover",
  "tre" = "RAP Tree Cover",
  "evi" = "MOD13 EVI",
  "ndvi" = "MOD13 NDVI",
  "et_m16" = "MOD16 ET",
  "pet_m16" = "MOD16 PET",
  "gpp" = "MOD17 GPP",
  "afgnpp" = "RAP Forb and Grass NPP",
  "pfgnpp" = "RAP Perennial Forb and Grass NPP",
  "shrnpp" = "RAP Shrub NPP",
  "trenpp" = "RAP Tree NPP"
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
  "penman" = "mm",
  "erc" = "",
  "etr" = "mm",
  "pet" = "mm",
  "rmax" = "percent",
  "rmin" = "percent",
  "sph" = "kg/kg",
  "tmmn" = "K",
  "tmmx" = "K",
  "vpd" = "kPa",
  "vs" = "m/s",
  "above90" = "",
  "con-dry" = "",
  "con-wet" = "",
  "dry-days" = "",
  "freeze-free" = "",
  "gdd" = "",
  "wet-days" = "",
  "afg" = "percent",
  "bgr" = "percent",
  "pfg" = "percent",
  "shr" = "percent",
  "tre" = "percent",
  "evi" = "",
  "ndvi" = "",
  "et_m16" = "mm",
  "pet_m16" = "mm",
  "gpp" = "kg m-2",
  "afgnpp" = "kg m-2",
  "pfgnpp" = "kg m-2",
  "shrnpp" = "kg m-2",
  "trenpp" = "kg m-2"
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
  "penman" = "in",
  "erc" = "",
  "etr" = "in",
  "pet" = "in",
  "rmax" = "percent",
  "rmin" = "percent",
  "sph" = "kg/kg",
  "tmmn" = "degF",
  "tmmx" = "degF",
  "vpd" = "millibar",
  "vs" = "mi/hr",
  "above90" = "",
  "con-dry" = "",
  "con-wet" = "",
  "dry-days" = "",
  "freeze-free" = "",
  "gdd" = "",
  "wet-days" = "",
  "afg" = "percent",
  "bgr" = "percent",
  "pfg" = "percent",
  "shr" = "percent",
  "tre" = "percent",
  "evi" = "",
  "ndvi" = "",
  "et_m16" = "in",
  "pet_m16" = "in",
  "gpp" = "kg m-2",
  "afgnpp" = "kg m-2",
  "pfgnpp" = "kg m-2",
  "shrnpp" = "kg m-2",
  "trenpp" = "kg m-2"
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
  "penman" = "mm",
  "erc" = "",
  "etr" = "mm",
  "pet" = "mm",
  "rmax" = "percent",
  "rmin" = "percent",
  "sph" = "kg/kg",
  "tmmn" = "degC",
  "tmmx" = "degC",
  "vpd" = "kPa",
  "vs" = "m/s",
  "above90" = "",
  "con-dry" = "",
  "con-wet" = "",
  "dry-days" = "",
  "freeze-free" = "",
  "gdd" = "",
  "wet-days" = "",
  "afg" = "percent",
  "bgr" = "percent",
  "pfg" = "percent",
  "shr" = "percent",
  "tre" = "percent",
  "evi" = "",
  "ndvi" = "",
  "et_m16" = "mm",
  "pet_m16" = "mm",
  "gpp" = "kg m-2",
  "afgnpp" = "kg m-2",
  "pfgnpp" = "kg m-2",
  "shrnpp" = "kg m-2",
  "trenpp" = "kg m-2"
)

colors = 
  c("Historical Emissions" = rgb(0,0,0,
                                 maxColorValue = 255),
    "transparent",
    " "  = "transparent",
    "  "  = "transparent",
    "Moderating Emissions\n(SSP1-2.6)" = rgb(34,46,77,
                                             maxColorValue = 255),
    "Middle of the Road\n(SSP2-4.5)" = rgb(223,146,71,
                                           maxColorValue = 255),
    "High Emissions\n(SSP3-7.0)" = rgb(187,54,51,
                                       maxColorValue = 255),
    "Accelerating Emissions\n(SSP5-8.5)" = rgb(122,41,40,
                                               maxColorValue = 255)
    
  )

factor_scenario <- function(dat) {
  dplyr::mutate(
    dat, 
    scenario = dplyr::recode(
      scenario, 
      "historical" = "Historical Emissions",
      "ssp126" = "Moderating Emissions\n(SSP1-2.6)",
      "ssp245" = "Middle of the Road\n(SSP2-4.5)",
      "ssp370" = "High Emissions\n(SSP3-7.0)",
      "ssp585" = "Accelerating Emissions\n(SSP5-8.5)"
    ) %>% 
      factor(
        levels = c("Historical Emissions",
                   "Moderating Emissions\n(SSP1-2.6)",
                   "Middle of the Road\n(SSP2-4.5)",
                   "High Emissions\n(SSP3-7.0)",
                   "Accelerating Emissions\n(SSP5-8.5)")
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

prep_for_timeseries <- function(dat, location, v, us_units=TRUE, scenarios) {
  
  loc <- dat %>% 
    dplyr::filter(id == location) %>% 
    head(1) %>% 
    dplyr::collect() %>% 
    dplyr::pull(name)

  dat %>% 
    dplyr::filter(id == location, variable == v) %>% 
    dplyr::filter(stringr::str_detect(scenario, scenarios)) %>% 
    dplyr::group_by(year=lubridate::year(date), scenario, model) %>% 
    dplyr::summarise(
      value = ifelse(v %in% c("pr", "penman", "hargreaves", "above90", "con-dry", "con-wet",
                              "dry-days", "freeze-free", "gdd", "wet-days", "et_m16", "pet_m16",
                              "gpp", "afgnpp", "pfgnpp", "shrnpp", "trenpp"), sum(value), mean(value)), 
      .groups = "drop"
    ) %>% 
    dplyr::collect() %>%
    convert_units(v, TRUE) %>%
    dplyr::group_by(year, scenario) %>% 
    dplyr::summarise(
      upper = quantile(value, 0.9) %>% as.numeric() %>% round(3),
      lower = quantile(value, 0.1) %>% as.numeric() %>% round(3),
      value = median(value) %>% round(3),
      .groups = "drop"
    ) %>% 
    factor_scenario() %>%
    dplyr::mutate(
      location = loc, 
      variable = v
    )
  
}

make_timeseries_plot <- function(dat, us_units=TRUE, difference=FALSE) {

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
    theme_minimal(14) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
      legend.title = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "bottom",
      legend.key.width = unit(0.25,"in"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.margin=unit(c(0.1,0.2,0.1,0.1), "in")
    ) +
    ggplot2::guides(colour = guide_legend(ncol = 2)) +
    coord_cartesian(clip = "off")
  
  if (difference) {
    plt = plt + geom_hline(yintercept=0)
  }
  
  return(plt)
}

prep_for_monthly_plot <- function(dat, location, v = "tas", us_units = T, scenarios) {
  
  loc <- dat %>% 
    dplyr::filter(id == location) %>% 
    head(1) %>% 
    dplyr::collect() %>% 
    dplyr::pull(name)
  
  out <- dat %>% 
    dplyr::filter(id == location, variable == v) %>%
    dplyr::filter(stringr::str_detect(scenario, scenarios)) %>% 
    dplyr::collect() %>%
    dplyr::mutate(
      year = lubridate::year(date), 
      month = month.abb[lubridate::month(date)],
      grp = dplyr::case_when(
        year %in% 1991:2020 ~ "Reference Period (1991-2020)", 
        year %in% 2040:2069 ~ "Mid Century (2040-2069)",
        year %in% 2970:2099 ~ "End-of-Century (2070-2099)"
      ), 
      scenario = ifelse(year >= 2015 & year <= 2020, "historical", scenario)
    )  %>% 
    dplyr::filter(!is.na(grp)) %>% 
    convert_units(v, us_units) %>% 
    dplyr::group_by(scenario, month, grp) %>% 
    dplyr::summarise(
      upper = quantile(value, 0.9) %>% as.numeric() %>% round(3),
      lower = quantile(value, 0.1) %>% as.numeric() %>% round(3),
      value = median(value) %>% round(3),
      .groups = "drop"
    ) 
  
  filt <- out %>% 
    dplyr::filter(scenario == "historical") 
  
  dplyr::bind_rows(
    dplyr::mutate(filt, grp = "Mid Century (2040-2069)"),
    dplyr::mutate(filt, grp = "End-of-Century (2070-2099)"),
    dplyr::filter(out, scenario != "historical")
  ) %>% 
    dplyr::mutate(
      month = factor(month, levels = month.abb),
      grp = factor(grp, levels = c(
        "Reference Period (1991-2020)",
        "Mid Century (2040-2069)",
        "End-of-Century (2070-2099)")
      )
    ) %>% 
    factor_scenario() %>% 
    dplyr::mutate(
      location = loc, 
      variable = v
    )
}

make_monthly_plot <- function(dat, us_units=TRUE, difference=FALSE) {

  
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
    ggplot(aes(x=month, color=scenario)) + 
    geom_pointrange(aes(y=value, ymin=lower, ymax=upper), position = position_dodge(width=0.25)) + 
    geom_line(aes(y=value, group=scenario)) +
    facet_grid(rows="grp") +
    labs(x = NULL,
         y = titles[["y"]],
         title = titles[["title"]]
    ) +
    theme_minimal(14) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
      legend.title = element_blank(),
      # legend.justification = c(1, 1),
      legend.position = "bottom",
      legend.key.width = unit(0.25,"in"),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
      plot.margin=unit(c(0.1,0.2,0.1,0.1), "in")
    ) +
    ggplot2::guides(colour = guide_legend(ncol = 2)) +
    coord_cartesian(clip = "off")
  
  if (difference) {
    plt = plt + geom_hline(yintercept=0)
  }
  
  return(plt)
}

placeholder_graph <- function() {
  
  tibble::tibble(x=1, y=1, txt="Click a county to plot data!") %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_text(aes(label=txt), size=10) + 
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}
