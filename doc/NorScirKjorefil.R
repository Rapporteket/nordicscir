
#-----------------------Lage eksempeldatasett-----------------------
rm(list=ls())
# NorScirEksData <- read.table('E:/Registre/NorScir/data/NorScirEksempeldata.csv', header=T, sep=';')
# #perm.sammen:
# permA <- c('InjuryDt', 'AdmitDt', 'DischgDt', 'ANeuExmDt', 'FNeuExmDt', 'QolDt', 
# 		'AdmitRehDt', 'CNeuExmDt', 'InjuryDateUnknown')
# 
# permB <- c('ReshId', 'ShNavn')
# N <- dim(NorScirEksData)[1]
# 
# NorScirEksData[ ,permA] <- NorScirEksData[sample(N, N),permA]
# NorScirEksData[ ,permB] <- NorScirEksData[sample(N, N),permB]
# NorScirEksData$PasientId <- NorScirEksData$PasientId[sample(N,N)]
# NorScirEksData$isMale <- NorScirEksData$isMale[sample(N,N)]
# NorScirEksData$AlderAar <- NorScirEksData$AlderAar[sample(N,N)]

varBort <- c('ShNavn', 'RHF', 'HF', 'MunicipalNumber', 'Municipal', 'PostalCode', 'HealthUnitName', 'Hospital', 'HealthUnitId')
HovedSkjema <- lageTulleData(HovedSkjema, varBort=varBort, antSh=3) 
Livskval <- lageTulleData(Livskval, varBort=varBort, antSh=3) 
Kontroll <- lageTulleData(Kontroll, varBort=varBort, antSh=3) 
Urin <- lageTulleData(Urin, varBort=varBort, antSh=3) 
Tarm <- lageTulleData(Tarm, varBort=varBort, antSh=3) 
AktivFunksjon <- lageTulleData(AktivFunksjon, varBort=varBort, antSh=3) 
AktivTilfredshet <- lageTulleData(AktivTilfredshet, varBort=varBort, antSh=3) 

objekter <- c('HovedSkjema', 'Livskval', 'Kontroll', 'Urin', 'Tarm', 'AktivFunksjon', 'AktivTilfredshet')
save(list = objekter, file='A:/NordicScir/NordicScirFIKTIVEdata.RData')



#write.table(NorScirEksData, file='E:/Registre/NordicScir/data/NorScirEksData.csv', sep=';')

#--------------------------------------SAMLERAPPORT-----------------------------------
rm(list=ls())
library(knitr)
library(tools)	#texi2pdf
#setwd("C:/ResultattjenesteGIT/nordicscir/inst")
setwd("../nordicscir/inst")
#HENT DATA
reshID <- 107627	#0 - alle	#105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs
#load('A:/NordicScir/NordicScirData.RData')
knit2pdf('NSmndRapp.Rnw')
#knit('NSmndRapp.Rnw')
#texi2pdf('NSmndRapp.tex')

knit2pdf('NSsamleRappLand.Rnw')
#knit('NSsamleRappLand.Rnw')
#texi2pdf('NSsamleRappLand.tex')

knit2pdf('NSsamleRapp.Rnw')
#knit('NSsamleRapp.Rnw')
#texi2pdf('NSsamleRapp.tex')

filtype <- c('.toc','.log', '.lof','.lot','.out', '.aux', '.vrb','.snm','.nav')
removeFiles <- c(paste0('NSmndRapp',filtype), 
                 paste0('NSsamleRapp', filtype), 
                 paste0('NSsamleRappLand', filtype))
file.remove(file=removeFiles)

#------------------------ LASTE DATA -------------------------------------
library(nordicscir)
rm(list=ls())
load('A:/NordicScir/NordicScirData.RData')

dato <- 'FormDataContract2019-10-30' #2017-05-24
sti <- 'A:/NordicScir/'
HovedSkjema <- read.table(paste0(sti, 'Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Livskval <- read.table(paste0(sti, 'LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Kontroll <- read.table(paste0(sti, 'Control', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Urin <- read.table(paste0(sti, 'UrinaryTractFunction', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Tarm <- read.table(paste0(sti, 'BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
AktivFunksjon <- read.table(paste0(sti, 'ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
AktivTilfredshet <- read.table(paste0(sti, 'ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)

Livskval$HovedskjemaGUID <- toupper(Livskval$HovedskjemaGUID)
Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)

#DataAlleTab <- list()

#HovedSkjema <- NSPreprosesser(HovedSkjema)
LivskvalH <- KobleMedHoved(HovedSkjema,Livskval)
KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
UrinH <- KobleMedHoved(HovedSkjema,Urin)
TarmH <- KobleMedHoved(HovedSkjema,Tarm)
AktivFunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
Aktivitet <- KobleMedHoved(AktivFunksjon, AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
AktivTilfredshetH <- KobleMedHoved(HovedSkjema, Aktivitet)


objekter <- c('HovedSkjema', 'LivskvalH', 'KontrollH', 'UrinH', 'TarmH', 'AktivFunksjonH', 'AktivTilfredshetH')
save(list = objekter, file='A:/NordicScir/NordicScirData.RData')

# KobleMedHoved <- function(HovedSkjema,Skjema2) {
#       varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
#       Skjema2 <- Skjema2[ ,c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
#       NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','XX'),
#                       by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = F, all.y=F)
# return(NSdata)
# }
ind18 <- which(as.Date(HovedSkjema$AdmitDt) >= '2018-01-01' & as.Date(HovedSkjema$AdmitDt) <= '2018-12-31')
HovedSkjema18 <- HovedSkjema[ind18,]
RegData18 <- KobleMedHoved(HovedSkjema18,Urin)
RegData <- RegData18
RegData <- NSPreprosesser(RegData)

save(AktivTilfredshet, file='AktivTilfredshet.RData')

# Jeg har koblet i to steg i R etter følgende skisse:
#       NyTab = AktivFunksjon.SkjemaGUID <-> Satisfaction. HovedskjemaGUID  
# Main.SkjemaGUID <-> NyTab. HovedskjemaGUID  

#------------------------ TESTE DATA -------------------------------------

#---Oppsummering/test av andel som har fått oppfølging---
paste0('Ant. hovedskjema m/Livskvalitet: ',sum(HovedSkjema$SkjemaGUID %in% Livskval$HovedskjemaGUID))
paste0('Ant. hovedskjema m/AktivFunksjon: ', sum(HovedSkjema$SkjemaGUID %in% AktivFunksjon$HovedskjemaGUID))
paste0('Ant. Satisfaction m/AktivFunksjon: ', sum(AktivFunksjon$SkjemaGUID %in% AktivTilfredshet$HovedskjemaGUID))

length(unique(Livskval$HovedskjemaGUID))
sum(unique(Livskval$HovedskjemaGUID) %in% HovedSkjema$SkjemaGUID)
sum(Livskval$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)

table(table(Livskval$HovedskjemaGUID))
table(table(LivskvalHoved$SkjemaGUID))


tabVar <- c('HealthUnitName','Aar')
Hskjema <- HovedSkjema[c('SkjemaGUID','HealthUnitName','AdmitDt')][order(HovedSkjema$SkjemaGUID),]
RaaTab <- cbind(Hskjema,
                #Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
                MedLivskval = match(Hskjema$SkjemaGUID, sort(Livskval$HovedskjemaGUID)),
                MedKtr = match(HovedSkjema$SkjemaGUID, sort(Kontroll$HovedskjemaGUID)),
                MedUrin = match(HovedSkjema$SkjemaGUID, sort(Urin$HovedskjemaGUID)),
                MedTarm = match(HovedSkjema$SkjemaGUID, sort(Tarm$HovedskjemaGUID)),
                MedFunksjon = match(HovedSkjema$SkjemaGUID, sort(AktivFunksjon$HovedskjemaGUID)),
                MedTilfreds = match(HovedSkjema$SkjemaGUID,
                                     sort(Hskjema$SkjemaGUID[match(AktivFunksjon$SkjemaGUID, sort(AktivTilfredshet$HovedskjemaGUID))]))
)
#RaaTab <- RaaTab1[which(as.Date(Hskjema$AdmitDt)> '2014-12-31'),]
Tot <- table(is.na(RaaTab$MedLivskval))/dim(RaaTab)[1]*100  #'MedKtr'
AntLivskval <- ftable(RaaTab[!is.na(RaaTab$MedLivskval),tabVar])
AndelLivskval <- round(100*AntLivskval/ftable(RaaTab[,tabVar]),1)
AndelKtr <- round(100*ftable(RaaTab[!is.na(RaaTab$MedKtr),tabVar])/ftable(RaaTab[,tabVar]),1)
AntSatisfact <- ftable(RaaTab[!is.na(RaaTab$MedSatisfact),tabVar])
AndelSatisfact <- round(100*ftable(RaaTab[!is.na(RaaTab$MedSatisfact),tabVar])/ftable(RaaTab[,tabVar]),1)
AndelPerform <- round(100*ftable(RaaTab[!is.na(RaaTab$MedPerform),tabVar])/ftable(RaaTab[,tabVar]),1)


table(HovedSkjema$FormStatus)
table(LivskvalH$FormStatus)
table(UrinH$FormStatus)
table(TarmH$FormStatus)
table(KontrollH$FormStatus)
table(AktivFunksjonH$FormStatus)
table(AktivTilfredshetH$FormStatus)


#------------------------------ Parametre --------------------------
rm(list=ls())
library(nordicscir)
setwd("C:/Registerinfo og historie/NordicScir/Figurer/")
load('A:/NordicScir/NordicScirData.RData')
reshID <- 107627             ##105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs, standard i funksj: 0 dvs. 'Alle'. Standard i rapporten skal v?re at man f?r opp eget sykehus.
enhetsUtvalg <- 0
minald <- 0
maxald <- 130
erMann <- 9                      #1-menn, 0-kvinner, Standard: '', dvs. begge
traume <- 'alle'    #'ja','nei', standard: ikke valgt
AIS <- 99 # as.character(c(1,4))	#AISgrad ved innleggelse alle(''), velge en eller flere fra 1:5
nivaaUt <- 99
datoFra <- '2018-01-01'             #Standard: bør være minste registrerte verdi ? min og max dato i utvalget vises alltid i figuren.
datoTil <- '2019-12-31'
valgtMaal='gjsn'	#'Med'-median, 'Gjsn' gjennomsnitt
grVar <- 'ShNavn'
datoUt=0 #Velge ut-dato som filtrering
outfile <- ''
valgtVar <- 'alder'

RegData <- NSPreprosesser(HovedSkjema)
table(RegData$Aar, RegData$ASensLvlLC)
UtDataFord <- NSFigAndelerSh(RegData=HovedSkjema, valgtVar = 'Ntsci', datoFra='2018-01-01')

#------------------------------ Fordelinger --------------------------
RegData <- HovedSkjema
valgtVar <- 'UtTil'	#AAis, FAis, Alder, DagerRehab, DagerTilRehab, NivaaInn, Ntsci,
					#OpphTot[HosptlDy], Permisjon[OutOfHosptlDy], UtTil[PlaceDis], 
                             	#Pustehjelp[VentAssi], PPlaceDis, RegForsinkelse,  #SkadeArsak[Scietiol]  
#UrinSkjema: 
RegData <- KobleMedHoved(HovedSkjema,Urin)
valgtVar <- 'UrinTomBlareHoved'   #'UrinInkontinens', 'UrinLegemidler','UrinLegemidlerHvilke', 'UrinKirInngr', 
                                    #'UrinTomBlareHoved', 'UrinTomBlareTillegg'
#TarmSkjema: 
RegData <- KobleMedHoved(HovedSkjema,Tarm)
valgtVar <- 'TarmInkontinensFra2019'   #'TarmAvfHoved','TarmAvfTillegg', TarmAvfmiddel, TarmAvfmiddelHvilke
                              #TarmInkontinens, TarmKirInngrep, TarmKirInngrepHvilke

#Livskvalitet
RegData <- KobleMedHoved(RegData,Livskval)
valgtVar <- 'LivsPsyk'                #LivsGen, LivsFys, LivsPsyk

#Funksjon (Aktivitet og deltagelse)
RegData <- KobleMedHoved(HovedSkjema,AktivFunksjon)
valgtVar <- 'FunkSpis'               #FunkDo, FunkKler, FunkMob, FunkSpis

#Tilfredshet. NB: Kobles til A&D
RegData <- KobleMedHoved(AktivFunksjon,AktivTilfredshet)
RegData <- KobleMedHoved(HovedSkjema,RegData)
valgtVar <- 'TilfSpis'               #TilfDo, TilfKler, TilfMob, TilfSpis

outfile <- '' #paste0(valgtVar, '.png')	#Navn angis av Jasper
RegData <- TilfredsH
preprosess <- 0

UtDataFord <- NSFigAndeler(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, nivaaUt=nivaaUt,
		reshID=reshID, enhetsUtvalg=0)    #, preprosess=1
#Aktuelt å legge til en parameter som sier hvilket skjema variabelen tilhører. Dette for å koble
#sammen riktig skjema til hovedskjema.

#Hoved
variable <- c('AAis', 'FAis', 'Alder', 'DagerRehab', 'DagerTilRehab', 'NivaaInn', 'NivaaUt',
              'Ntsci', 'OpphTot', 'UtTil', 'SkadeArsak')
#Urin
variable <- c('UrinInkontinens', 'UrinLegemidler','UrinLegemidlerHvilke', 'UrinKirInngr', 
                  'UrinTomBlareHoved', 'UrinTomBlareTillegg')
#Tarm
variable <- c('TarmAvfHoved','TarmAvfTillegg', 'TarmAvfmiddel', 'TarmAvfmiddelHvilke',
                  'TarmInkontinens', 'TarmKirInngrep', 'TarmKirInngrepHvilke')
#Livs
variable  <- c('LivsGen', 'LivsFys', 'LivsPsyk')

#Funk
variable  <- c('FunkDo', 'FunkKler', 'FunkMob', 'FunkSpis')

#Tilf
variable  <- c('TilfDo', 'TilfKler', 'TilfMob', 'TilfSpis')

setwd("C:/Registerinfo og historie/NordicScir/Figurer/")
for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '.png')
	NSFigAndeler(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	             AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, nivaaUt=nivaaUt,
	             reshID=reshID, enhetsUtvalg=1, hentData=0)
}

#------------------------------ Sentralmål --------------------------
outfile <- '' #paste(valgtVar, '_Sh.png')	#Navn angis av Jasper
valgtVar <- 'LivsGen'   #'Alder', 'DagerRehab', 'DagerTilRehab', 'OpphTot', 'LivsGen', 'LivsFys', 'LivsPsyk'
datoFra <- '2017-07-01'
Data <- NSFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		valgtMaal='med', AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume)

variable <- c('Alder', 'DagerRehab', 'DagerTilRehab', 'OpphTot', 'RegForsinkelse') 
variable <- c('LivsGen', 'LivsFys', 'LivsPsyk') #Koble på Livskvalitetsdata
for (valgtVar in variable) {
	outfile <- paste0('M_',valgtVar, '.png')
	NSFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	               valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, traume=traume,
	               AIS=AIS, nivaaUt = nivaaUt)
}
#------------------------------ Nevrologisk kategori --------------------------
rm(list=ls())

valgtVar <- 'NevrNivaaInn'	#M? velge... NevrNivaaInnUt, NevrNivaaInn, NevrNivaaUt, 
outfile <- paste0(valgtVar, '.png') #navn p? fil figuren skrives ned til
setwd("C:/ResultattjenesteGIT/nordicscir/")

NSFigAndelStabel(RegData=NSdata, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, 
		datoTil=datoTil, traume=traume, 
		minald=minald, maxald=maxald, erMann=erMann, enhetsUtvalg=enhetsUtvalg, reshID=reshID)	#egenavd=egenavd 
		
for (valgtVar in c('NevrNivaaInnUt', 'NevrNivaaInn', 'NevrNivaaUt' )) {
	outfile <- paste(valgtVar, '.pdf', sep='')
FigAndelStabel(RegData, libkat=libkat, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann, traume=traume, reshID=reshID)	#, sml=sml)
}

I_ABC <- which(RegData$AAis %in% c('A','B','C'))






#-----------------------------Nevrologiske kategorier----------------------------
#Motoriske variable:
MotVar <- c('AMtrLvlAreaL', 'AMtrLvlLC', 'AMtrLvlLT', 'AMtrLvlLL', 'AMtrLvlLS', 'FMtrLvlAreaL', 'FMtrLvlLC', 'FMtrLvlLT', 'FMtrLvlLL', 'FMtrLvlLS', 'AMtrLvlAreaR', 'AMtrLvlRC', 'AMtrLvlRT', 'AMtrLvlRL', 'AMtrLvlRS', 'FMtrLvlAreaR', 'FMtrLvlRC', 'FMtrLvlRT', 'FMtrLvlRL', 'FMtrLvlRS')
#Sensoriske variable:
SensVar <- c('ASensLvlAreaL', 'ASensLvlLC', 'ASensLvlLT', 'ASensLvlLL', 'ASensLvlLS', 'FSensLvlAreaL', 'FSensLvlLC', 'FSensLvlLT', 'FSensLvlLL', 'FSensLvlLS', 'ASensLvlAreaR', 'ASensLvlRC', 'ASensLvlRT', 'ASensLvlRL', 'ASensLvlRS', 'FSensLvlAreaR', 'FSensLvlRC', 'FSensLvlRT', 'FSensLvlRL', 'FSensLvlRS')
#Innleggelse variable:
InnVar <- c('AAis', 'ASensLvlAreaL', 'ASensLvlAreaR', 'AMtrLvlAreaR', 'AMtrLvlAreaL', 'ASensLvlLC', 'ASensLvlLT', 'ASensLvlLL', 'ASensLvlLS', 'ASensLvlRC', 'ASensLvlRT', 'ASensLvlRL', 'ASensLvlRS', 'AMtrLvlLC', 'AMtrLvlLT', 'AMtrLvlLL', 'AMtrLvlLS', 'AMtrLvlRC', 'AMtrLvlRT', 'AMtrLvlRL', 'AMtrLvlRS')
#Utskriving variable:
UtVar <- c('FAis', 'FSensLvlAreaL', 'FSensLvlAreaR', 'FMtrLvlAreaL', 'FMtrLvlAreaR', 'FSensLvlLC', 'FSensLvlLT', 'FSensLvlLL', 'FSensLvlLS', 'FSensLvlRC', 'FSensLvlRT', 'FSensLvlRL', 'FSensLvlRS', 'FMtrLvlLC', 'FMtrLvlLT', 'FMtrLvlLL', 'FMtrLvlLS', 'FMtrLvlRC', 'FMtrLvlRT', 'FMtrLvlRL', 'FMtrLvlRS')
#Venstre variable:
VVar <- c('ASensLvlAreaL', 'AMtrLvlAreaL', 'ASensLvlLC', 'ASensLvlLT', 'ASensLvlLL', 'ASensLvlLS', 'AMtrLvlLC', 'AMtrLvlLT', 'AMtrLvlLL', 'AMtrLvlLS', 'FSensLvlAreaL', 'FMtrLvlAreaL', 'FSensLvlLC', 'FSensLvlLT', 'FSensLvlLL', 'FSensLvlLS', 'FMtrLvlLC', 'FMtrLvlLT', 'FMtrLvlLL', 'FMtrLvlLS')
#H?yre variable:
HVar <- c('ASensLvlAreaR', 'AMtrLvlAreaR', 'ASensLvlRC', 'ASensLvlRT', 'ASensLvlRL', 'ASensLvlRS', 'AMtrLvlRC', 'AMtrLvlRT', 'AMtrLvlRL', 'AMtrLvlRS', 'FSensLvlAreaR', 'FMtrLvlAreaR', 'FSensLvlRC', 'FSensLvlRT', 'FSensLvlRL', 'FSensLvlRS', 'FMtrLvlRC', 'FMtrLvlRT', 'FMtrLvlRL', 'FMtrLvlRS')


#Inn, Motorisk, V
c('AAis','AMtrLvlAreaL', 	'AMtrLvlLC', 	'AMtrLvlLT', 	'AMtrLvlLL', 	'AMtrLvlLS')
#Inn, Motorisk, H
c('AAis','AMtrLvlAreaR', 	'AMtrLvlRC', 	'AMtrLvlRT', 	'AMtrLvlRL', 	'AMtrLvlRS')
#Inn, Sensorisk, V
c('AAis','ASensLvlAreaL', 	'ASensLvlLC', 	'ASensLvlLT', 	'ASensLvlLL', 	'ASensLvlLS')
#Inn, Sensorisk, H
c('AAis','ASensLvlAreaR', 	'ASensLvlRC', 	'ASensLvlRT', 	'ASensLvlRL', 	'ASensLvlRS')
#Ut, Motorisk, V
c('FAis','FMtrLvlAreaL', 	'FMtrLvlLC', 	'FMtrLvlLT', 	'FMtrLvlLL', 	'FMtrLvlLS')
#Ut, Motorisk, H
c('FAis','FMtrLvlAreaR', 	'FMtrLvlRC', 	'FMtrLvlRT', 	'FMtrLvlRL', 	'FMtrLvlRS')
#Ut, Sensorisk, V
c('FAis','FSensLvlAreaL', 	'FSensLvlLC', 	'FSensLvlLT', 	'FSensLvlLL', 	'FSensLvlLS')
#Ut, Sensorisk, H
c('FAis','FSensLvlAreaR', 	'FSensLvlRC', 	'FSensLvlRT', 	'FSensLvlRL', 	'FSensLvlRS')

#Inn, 
I_ABC <- which(RegData$AAis %in% c('A','B','C'))
I_D <- which(RegData$AAis == 'D')
#Motorisk, Venstre
#A,B,C+C1-4
IMV_ABC_C0104 <- length(intersect(I_ABC, which(RegData$AMtrLvlLC %in% c('C01','C02','C03','C04'))))/N
#A,B,C+C5-8
IMV_ABC_C0104 <- length(intersect(I_ABC, which(RegData$AMtrLvlLC %in% c('C01','C02','C03','C04')) ))/N
#A,B,C+T,L,S
IMV_ABC_TLS <- length(intersect(I_ABC, union(which(RegData$AMtrLvlAreaL %in% c('Lumbal', 'Sacral', 'Thoracic')))/N
D
#Pustehjelp VentAssi (0-3,9)- andel 1-3 av 0-3
Pustehjelp <- length(which(RegData$VentAssi %in% 1:3))/length(which(RegData$VentAssi %in% 0:3)


105593 - Haukeland, Nevrologisk avdeling "Nevro post 4 / spinalenheten"
106896 - Sunnaas sykehus "Seksjon for Poliklinikk, Vurdering og Oppf?lging"
107627 - St.Olavs hospital "Avdeling for spinalskader"


table(Kontroll$HealthUnitShortName, Kontroll$CNum)
