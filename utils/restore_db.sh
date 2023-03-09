#!/bin/bash

docker exec -it db pg_restore -U mco -d blm -1 $1 # Name of the .dump file as the script's only argument. 