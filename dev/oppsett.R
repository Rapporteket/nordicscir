
Sys.setenv(FALK_EXTENDED_USER_RIGHTS="[{\"A\":80,\"R\":\"SC\",\"U\":106896},{\"A\":80,\"R\":\"LU\",\"U\":105593},{\"A\":81,\"R\":\"LC\",\"U\":106896}]")
Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH="/home/rstudio/nordicscir/data-raw/config")
# Sys.unsetenv("MYSQL_PORT_LOG")
Sys.setenv(MYSQL_DB_DATA="NordicScirReportDataStaging")
# Sys.setenv(MYSQL_USER="root")
# Sys.setenv(MYSQL_PASSWORD="root")


nordicscir::kjor_NSapper(register='nordicscir')
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

# Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor

nordicscir::kjor_NSapper(register='nordicscir')


######################
# autoreport-innslag #
######################


tmp_yml <- yaml::read_yaml("./dev/test.yml")
tmp_json <- jsonlite::serializeJSON(tmp_yml)
query <- paste0("INSERT INTO `autoreport` VALUES ('", tmp_json, "');")


Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)
install.packages("ggplot2")
devtools::install("../rapadm/.", upgrade = FALSE, dependencies = FALSE)

# enten
rapadm::run_app()
# eller

source("dev/sysSetenv.R")
shiny::shinyApp(ui = rapadm::app_ui, server = rapadm::app_server, options = list(launch.browser = TRUE))

