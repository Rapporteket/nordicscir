RegData <- rapbase::loadRegData(
  registryName = "data",
  query="SELECT * FROM mainformdatacontract",
  dbType="mysql")
RegData <- NSPreprosesser(RegData)

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")


source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

nordicscir::kjor_NSapper(register='nordicscir')
