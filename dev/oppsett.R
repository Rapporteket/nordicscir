
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=nordicscir::NSRegDataSQL(valgtVar = 'Alder'))



Kvartal = c(lubridate::quarter(seq.Date(as.Date(lubridate::floor_date(min(RegData$RapDato), 'month')),
                                        max(RegData$RapDato),
                                        by = "quarter"), with_year = T),
            lubridate::quarter(max(RegData$RapDato),, with_year = T),
            lubridate::quarter(max(RegData$RapDato),, with_year = T))

sort(unique(Kvartal))

NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'KontControlInterruptedReason'), valgtVar = 'KontControlInterruptedReason')
table(RegData$ControlInterruptedReason, RegData$Aar)


AlleTab <- nordicscir::getRealData(register = 'norscir')
sapply(RegData, class)

RegDataKtr <- rapbase::loadRegData(registryName = 'data', query = 'select *  FROM control_form', dbType="mysql")

sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet16ab69750.sql.gz__20251009_122654.tar.gz",
                     keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
                     target_dir = "c://Users/lro2402unn/RegistreGIT/data/."
                     )
