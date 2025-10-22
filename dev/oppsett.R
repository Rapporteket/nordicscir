
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=nordicscir::NSRegDataSQL(valgtVar = 'Alder'))

NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'KontControlInterruptedReason'), valgtVar = 'KontControlInterruptedReason')
table(RegData$ControlInterruptedReason, RegData$Aar)

NSFigAndelerGrVar(RegData=RegData,preprosess=0,
                              valgtVar='ABMI', datoFra='2015-01-01', datoTil=Sys.Date(),
                              minald=0, maxald=130, erMann='',
                              enhetsUtvalg=0,
                              Ngrense=10, reshID=0)

AlleTab <- nordicscir::getRealData(register = 'norscir')
sapply(RegData, class)

RegDataKtr <- rapbase::loadRegData(registryName = 'data', query = 'select *  FROM control_form', dbType="mysql")

sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet16ab69750.sql.gz__20251009_122654.tar.gz",
                     keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
                     target_dir = "c://Users/lro2402unn/RegistreGIT/data/."
                     )
