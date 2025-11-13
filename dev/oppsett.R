
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=nordicscir::NSRegDataSQL(valgtVar = 'Kontroll'))

NSFigAndeler(RegData = NSRegDataSQL(valgtVar = 'KontControlInterruptedReason'), valgtVar = 'KontControlInterruptedReason')
table(RegData$ControlInterruptedReason, RegData$Aar)

NSFigAndeler(RegData=RegData,preprosess=0,
                              valgtVar='KontUtfHvordan')

AlleTab <- nordicscir::getRealData(register = 'norscir')
AlleTab <- nordicscir::processAllData(AlleTab, register = 'norscir')
attach(AlleTab)
reshID <- 106896

#Div undersøkelse av kontroller:

AlleTab <- nordicscir::getRealData(register = 'norscir')
attach(AlleTab)
dim(KontrollH)
DataKtr <- rapbase::loadRegData(registryName = 'data', query = 'select *  FROM control_form', dbType="mysql") #2614

DataHovedKtr <- NSRegDataSQL(valgtVar = 'KontXX') #1928
# Kontrollskjema som ikke er knyttet til hovedskjema:
  manglerHoved <- setdiff(DataKtr$SkjemaGUID, DataHovedKtr$SkjemaGUIDKont) #686 stk
  KtrUtenHoved <- DataKtr[!(DataKtr$HovedskjemaGUID %in% HovedSkjema$SkjemaGUIDHoved),]
  range(DataKtr$CreationDate[is.na(DataKtr$CNum)])
  table(DataKtr$CNum, useNA = 'a')
  sum(is.na(DataKtr$CNum))

  length(unique(DataHovedKtr$SkjemaGUIDKont)) #, '')]))
  DataHovedKtr$SkjemaGUIDHoved

# HAR ALLE SOM MANGLER CNUM OGSÅ KONTROLLSKJEMA FRA FØR HØSTEN 2017? SE PÅ
  as.Date(tapply(as.Date(KtrUtenHoved$CreationDate), KtrUtenHoved$PasientGUID, min))
  as.Date(16575)
  table(as.Date(KtrUtenHoved$CreationDate))

  length(unique(KtrUtenHoved$PasientGUID))
table(DataHovedKtr$CNum)
table(DataKtr$CNum)


sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet16ab69750.sql.gz__20251009_122654.tar.gz",
                     keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
                     target_dir = "c://Users/lro2402unn/RegistreGIT/data/."
                     )
