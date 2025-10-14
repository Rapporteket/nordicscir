
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=NSRegDataSQL(valgtVar = 'Kont'))

NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'KontrKompl'), valgtVar = 'KontrKompl')
table(RegData$CPressureUlcer, RegData$Aar)


AlleTab <- nordicscir::getRealData(register = 'norscir')
sapply(RegData, class)


sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet16ab69750.sql.gz__20251009_122654.tar.gz",
                     keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
                     target_dir = "c://Users/lro2402unn/RegistreGIT/data/."
                     )
