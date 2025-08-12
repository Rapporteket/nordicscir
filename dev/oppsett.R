
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)




RegData <- rapbase::loadRegData(
  registryName = "data",
  query = "SELECT * FROM spinal_cord_injury_core_data_set",
  dbType = "mysql"
)

RegData <- nordicscir::NSPreprosesser(RegData)


RegData <- rapbase::loadRegData(
  registryName = "data",
  query = "SELECT * FROM spinal_cord_injury_core_data_set",
  dbType = "mysql"
)

RegData <- nordicscir::NSPreprosesser(RegData)

AlleTab <- nordicscir::getRealData(register = 'norscir')
sapply(RegData, class)
