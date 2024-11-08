
Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LC\",\"U\":106896},{\"A\":80,\"R\":\"SC\",\"U\":105593},{\"A\":81,\"R\":\"LC\",\"U\":2}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nordicscir/data-raw/config")
# Sys.unsetenv("MYSQL_PORT_LOG")
Sys.setenv(MYSQL_DB_DATA="NordicScirReportDataStaging")
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")


nordicscir::kjor_NSapper(register='nordicscir')
RegData <- rapbase::loadRegData(
  registryName = "data",
  query="SELECT * FROM eq5dlformdatacontract",
  dbType="mysql")

##############################
## Kjøring på mobilt kontor ##
##############################

devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE, dependencies = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/nordicscir573c60536ce3.sql.gz__20241107_122831.tar.gz", keyfile = "p://.ssh/id_rsa")

Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"LC\",\"U\":106896},{\"A\":80,\"R\":\"SC\",\"U\":105593},{\"A\":81,\"R\":\"LC\",\"U\":2}]")
Sys.setenv(MYSQL_DB_LOG="db_log")
Sys.setenv(MYSQL_DB_AUTOREPORT="db_autoreport")
Sys.setenv(MYSQL_DB_DATA="NordicScirReportDataStaging")
Sys.setenv(MYSQL_HOST="localhost")
Sys.setenv(MYSQL_USER="root")
Sys.setenv(MYSQL_PASSWORD="root")
Sys.setenv(FALK_APP_ID="80")
Sys.setenv(USERORGID="pilot")
Sys.setenv(SHINYPROXY_USERNAME="test@tester.no")
Sys.setenv(SHINYPROXY_USERGROUPS="pilot")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="data-raw/config")

nordicscir::kjor_NSapper(register='nordicscir')


######################
# autoreport-innslag #
######################


tmp_yml <- yaml::read_yaml("./dev/test.yml")
tmp_json <- jsonlite::serializeJSON(tmp_yml)
query <- paste0("INSERT INTO `autoreport` VALUES ('", tmp_json, "');")



