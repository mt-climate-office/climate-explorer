FROM rstudio/plumber:v1.2.0

RUN apt update && \
    apt install -y libpq-dev && \
    apt install -y libudunits2-dev
    
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('dbplyr')"
RUN R -e "install.packages('units')"

RUN mkdir /app
COPY plumber.R /app/
COPY crud.R /app/

CMD ["/app/plumber.R"]