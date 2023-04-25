FROM rocker/shiny:4

RUN apt update && \
    apt install -y libpq-dev && \
    apt install -y libudunits2-dev && \
    apt install -y libproj-dev && \
    apt install -y libgdal-dev && \
    apt install -y gdebi-core && \
    apt install -y curl && \ 
    apt install -y pandoc
    

RUN curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb
RUN gdebi --non-interactive quarto-linux-amd64.deb

RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('terra')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('sf')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('RColorBrewer')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('lattice')"
RUN R -e "install.packages('dbplyr')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('viridisLite')"
RUN R -e "install.packages('quarto')"
RUN R -e "install.packages('shinyjs')"

RUN R -e "devtools::install_github('rstudio/leaflet')"
RUN R -e "devtools::install_github('r-spatial/leafem')"

RUN R -e "install.packages('purrr')"
RUN R -e "install.packages('cowplot')"

WORKDIR /home/app

# Install tinytex after specifying workdir. 
RUN quarto install tool tinytex --no-prompt --update-path

COPY ./app/ .

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/local/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app

RUN chown app:app -R /home/app
USER app

# Open Port 3838 of the container
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]