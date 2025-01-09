library(ggplot2)
library(arrow)
library(dplyr)


# bucket <- s3_bucket("voltrondata-labs-datasets/nyc-taxi-tiny")
# copy_files(from = bucket, to = "~/data/nyc-taxi")
ds <- open_dataset("~/data/nyc-taxi")


p <- ds %>%
  group_by(year) %>% 
  dplyr::summarise(
    count = dplyr::n()
  ) %>%
  collect() %>% 
  ggplot(aes(x=year, y=count)) + 
  geom_point() + geom_line() + 
  theme_minimal()


pltly <- plotly::ggplotly(p)
htmlwidgets::saveWidget(plotly::as_widget(pltly), "./test.html")
