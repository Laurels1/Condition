library(ROracle)
library(DBI)
library(dplyr)
library(rstudioapi)

drv <- dbDriver("Oracle")

#For SVDBS:
host <- "nefscdb1.nmfs.local"
port <- 1526
svc <- "nefsc_users"

#For Observer data:
  # host <- "155.206.139.31"
  # port <- 1526
  # svc <- NOVA


connect.string <- paste("(DESCRIPTION=",
                        "(ADDRESS=(PROTOCOL=tcp)(HOST=",host,")(PORT=",port,"))",
                        "(CONNECT_DATA=(SERVICE_NAME=",svc,")))",sep="")

user <- rstudioapi::showPrompt(title="Username", message="Please enter your Oracle username", default="")

passwd <- .rs.askForPassword("Please enter your Oracle password")

channel <- dbConnect(drv,username=user,password=passwd, dbname=connect.string)

#rs <- dbSendQuery(con, "SELECT owner,table_name FROM all_tables")

#data <- fetch(rs)

#print(data)