FROM rocker/shiny:4

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

COPY ./install.R /opt/scripts

RUN chmod a+x /opt/scripts/install.R

RUN Rscript /opt/ scripts/install.R

COPY ./app/* .