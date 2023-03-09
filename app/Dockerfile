FROM rocker/shiny:4

RUN apt update && \
    apt install -y libpq-dev && \
    apt install -y libudunits2-dev && \
    apt install -y libproj-dev && \
    apt install -y libgdal-dev

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

RUN mkdir /opt/scripts
COPY ./install.R /opt/scripts

RUN chmod a+x /opt/scripts/install.R

RUN Rscript /opt/scripts/install.R

COPY ./app/ .