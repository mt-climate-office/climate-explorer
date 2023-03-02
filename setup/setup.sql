create table "data".derived;

drop extension postgis_raster;
drop extension postgis;

create extension postgis schema "data";
create extension postgis_raster schema "data";

alter database gis set postgis.enable_outdb_rasters = true;
alter database gis set postgis.gdal_enabled_drivers to 'ENABLE_ALL';

alter database gis set search_path to 'data';