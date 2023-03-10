# report-builder
Automated Reporting and Plotting of Climate Projections

## Initialize and Restore the Database
Spin up the database docker container: 
```{bash}
docker compose up --build -d db
```

Then, copy the backup script to the container and run the setup script:
```{bash}
docker cp ./setup/setup.sql db:/
docker exec -it db psql -U mco -d blm -a -f /setup.sql
```