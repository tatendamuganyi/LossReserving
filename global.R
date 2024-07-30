library(RSQLite)
library(DBI)

#Connecting to the Insurance Database
stats_db<-dbConnect(RSQLite::SQLite(),"//retiasstorage//devops//LossReserving//db//InsdB.db")