library(magrittr)
library(ggplot2)

tas_tsfm <- function(x)  {x - 273.15}

make_timeseries_plot <- function(dat, county = "Beaverhead County", variable = "tas", tsfm) {
  
  dat %>% 
    dplyr::filter(county_name == county, 
                  variable == !!variable) %>% 
    dplyr::mutate(value = tsfm(value)) %>% 
    ggplot(aes(x=date, y = value,  color= scenario)) + 
      geom_smooth()
}

dat <- readr::read_csv("./app/preprocess.csv")
