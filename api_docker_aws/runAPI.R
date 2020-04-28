library(plumber)

API <- plumb('modelo.R')

API$run(host = "0.0.0.0",
        port = 8080,
        swagger = T)