#!/bin/sh

docker volume create db-data
# docker volume create db-backups
docker network create pg

# docker exec -it db apt update && apt install -y postgis

dbname='postgresql://mco:thisisatest1234!@localhost:5432/gis' &&
url=https://data.climate.umt.edu/mca/cmip/ACCESS-ESM1-5_historical_r1i1p1f1_pr.tif &&
raster2pgsql -s 4326 -t 256x256 -I -R /vsicurl/$url data.rast | psql $dbname

alter user mco set search_path to data, "$user", public;
SELECT (ST_MetaData(rast)).* FROM "data".rast LIMIT 1;
