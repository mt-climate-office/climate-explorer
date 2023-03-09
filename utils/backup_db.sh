#!/bin/bash

docker exec -t db pg_dumpall -c -U mco > dump_$(date +%Y-%m-%d).dump