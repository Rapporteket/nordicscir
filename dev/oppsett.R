
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
