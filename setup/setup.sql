CREATE SCHEMA future AUTHORIZATION mco;
create table future.county();
alter table future.county
	add column "name" varchar(64),
	add column id varchar(64),
	add column model varchar(64),
	add column scenario varchar(64),
	add column variable varchar(64),
	add column "date" date,
	add column value numeric;

CREATE UNIQUE INDEX county_unique_index ON future.county USING btree (id, variable, date DESC, scenario, model);

set datestyle to ISO, YMD;
copy future.county (model, scenario, variable, "name", "date", value, id) from '/data/county.csv' delimiter ',' csv header;
--

create table future.huc();
alter table future.huc
	add column "name" varchar(64),
	add column id varchar(64),
	add column model varchar(64),
	add column scenario varchar(64),
	add column variable varchar(64),
	add column "date" date,
	add column value numeric;

CREATE UNIQUE INDEX huc_unique_index ON future.huc USING btree (id, variable, date DESC, scenario, model);

set datestyle to ISO, YMD;
copy future.huc (model, scenario, variable, "name", "date", value, id) from '/data/huc.csv' delimiter ',' csv header;
