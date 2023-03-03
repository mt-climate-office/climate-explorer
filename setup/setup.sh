#!/bin/sh
apt update 
apt install -y postgis
docker volume create db-data
docker network create pg

# docker exec -it db apt update && apt install -y postgis

dbname='postgresql://mco:thisisatest1234!@localhost:5432/gis' &&
url=https://data.climate.umt.edu/mca/cmip/ACCESS-ESM1-5_historical_r1i1p1f1_pr.tif &&
raster2pgsql -s 4326 -t 256x256 -I -N '-3.4e+38' -C -n "name" -R /vsicurl/$url data.rast | psql $dbname

dbname='postgresql://mco:thisisatest1234!@localhost:5434/gis' &&
url=/home/cbrust/git/report-builder/setup/counties.shp &&
shp2pgsql -g geometry -I -s 4326 $url data.vect | psql $dbname

alter user mco set search_path to data, "$user", public;
SELECT (ST_MetaData(rast)).* FROM "data".rast LIMIT 1;
