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

WORKDIR /home/app

COPY ./app/ .

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/local/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app

RUN chown app:app -R /home/app
USER app

# Open Port 3838 of the container
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]