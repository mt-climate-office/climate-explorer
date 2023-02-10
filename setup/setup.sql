create extension postgis;
create extension postgis_raster;

alter database gis set postgis.enable_outdb_rasters = true;
alter database gis set postgis.gdal_enabled_drivers to 'ENABLE_ALL';

UPDATE pg_extension
SET extrelocatable = TRUE
WHERE extname = 'postgis';

UPDATE pg_extension
SET extrelocatable = TRUE
WHERE extname = 'postgis_raster';

ALTER EXTENSION postgis_raster
SET SCHEMA data;

psql "dbname=gis options=--search_path=data" -a -f /usr/share/postgresql/15/contrib/postgis-3.3/spatial_ref_sys.sql -U mco