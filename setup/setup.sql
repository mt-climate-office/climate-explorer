--create schema blm."data";
--drop table "data".counties;
create table "data".counties();
alter table "data".counties
	add column "name" varchar(128),
	add column county_fips varchar(128),
	add column model varchar(128),
	add column scenario varchar(128),
	add column variable varchar(128),
	add column "date" date,
	add column value numeric;

CREATE UNIQUE INDEX date_first ON counties USING btree (date DESC, county_fips, scenario, variable, model);
CREATE UNIQUE INDEX variable_first ON counties USING btree (variable, date DESC, county_fips,  scenario, model);
CREATE UNIQUE INDEX county_first ON counties USING btree (county_fips, variable, date DESC, scenario, model);

set datestyle to ISO, YMD;
copy "data".counties from '/data/preprocess.csv' delimiter ',' csv header;