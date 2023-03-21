library(plumber)

pr <- plumb("/app/plumber.R")
pr$run(host = "0.0.0.0", port = 80)