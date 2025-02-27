
Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"SC\",\"U\":106896},{\"A\":80,\"R\":\"LU\",\"U\":105593},{\"A\":81,\"R\":\"LC\",\"U\":106896}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nordicscir/data-raw/config")
# Sys.unsetenv("MYSQL_PORT_LOG")
Sys.setenv(MYSQL_DB_DATA="nordicscirreportdatastaging")
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")


nordicscir::kjor_NSapper(register='nordicscir')
RegData <- rapbase::loadRegData(
  registryName = "data",
  query = "SELECT * FROM mainformdatacontract",
  dbType = "mysql"
)

RegData <- NSPreprosesser(RegData)

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir13a9fde4b.sql.gz__20250224_121908.tar.gz", keyfile = "p://.ssh/id_rsa", target_dir = "c://Users/ast046/Downloads/.")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST = "localhost") # for mobilt kontor

nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)
