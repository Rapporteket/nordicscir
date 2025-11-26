
devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)


source("dev/sysSetenv.R")
nordicscir::kjor_NSapper(register = "nordicscir", browser = TRUE)

source("dev/sysSetenv.R")
Sys.setenv(MYSQL_DB_DATA="norscir")
nordicscir::kjor_NSapper(register = "norscir", browser = TRUE)

RegData <- nordicscir::NSPreprosesser(RegData=nordicscir::NSRegDataSQL(valgtVar = 'Kontr'))
RegDatau112 <- RegData[!(is.na(RegData$CNum) & RegData$ControlStatus==0), ]

AlleTab <- nordicscir::getRealData(register = 'norscir')
AlleTab <- nordicscir::processAllData(AlleTab, register = 'norscir')
attach(AlleTab)
reshID <- 106896

#Div undersøkelse av kontroller:
library(nordicscir)
AlleTab <- nordicscir::getRealData(register = 'norscir')
attach(AlleTab)
dim(KontrollH)
DataKtr <- rapbase::loadRegData(registryName = 'data', query = 'select *  FROM control_form', dbType="mysql") #2614
DataKtr <- DataKtr[DataKtr$CNeuExmDt >=  '2024-01-01', ]
DataKtr <- DataKtr[!is.na(DataKtr$CNum), ]

DataHovedKtr <- NSRegDataSQL(valgtVar = 'KontXX') #1928
# Kontrollskjema som ikke er knyttet til hovedskjema:
  manglerHoved <- setdiff(DataKtr$SkjemaGUID, DataHovedKtr$SkjemaGUIDKont) #686 stk
  KtrUtenHoved <- DataKtr[!(DataKtr$HovedskjemaGUID %in% HovedSkjema$SkjemaGUIDHoved),]
  write.table(KtrUtenHoved[,c("SkjemaGUID", "CNeuExmDt", 'ControlStatus')], file = '../data/KtrUtenHoved.csv', row.names = F, sep = ';')
  range(KtrUtenHoved$CNeuExmDt)
  table(DataKtr$CNum[DataKtr$ControlStatus==0], useNA = 'a')
  sum(is.na(DataKtr$CNum))

  unique(DataKtr[ ,c("Skjematype", "CNum", "ControlStatus")])
  range(DataKtr$CNeuExmDt[DataKtr$ControlStatus==0 & is.na(DataKtr$CNum)])

  length(unique(DataHovedKtr$SkjemaGUIDKont)) #, '')]))
  DataHovedKtr$SkjemaGUIDHoved

# HAR ALLE SOM MANGLER CNUM OGSÅ KONTROLLSKJEMA FRA FØR HØSTEN 2017? SE PÅ
  as.Date(tapply(as.Date(KtrUtenHoved$CreationDate), KtrUtenHoved$PasientGUID, min))
  as.Date(16575)
  table(as.Date(KtrUtenHoved$CreationDate))

  length(unique(KtrUtenHoved$PasientGUID))
table(DataHovedKtr$CNum)
table(DataKtr$CNum)


sship::dec("c://Users/lro2402unn/RegistreGIT/data/nordicscir1324ad7da.sql.gz__20251113_091431.tar.gz",
                     keyfile = "c://Users/lro2402unn/.ssh/id_rsa",
                     target_dir = "c://Users/lro2402unn/RegistreGIT/data/."
                     )
