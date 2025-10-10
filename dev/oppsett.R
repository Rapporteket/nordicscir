
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=NSRegDataSQL())
RegData <- RegData[RegData$Aar >= 2022, ]

RegData <- nordicscir::NSPreprosesser(RegData)
NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'KomplPrim'), valgtVar = 'KomplPrim')
NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'UrinKirInngr'), valgtVar = 'UrinKirInngr')

table(RegData$Land)

AlleTab <- nordicscir::getRealData(register = 'norscir')
sapply(RegData, class)
