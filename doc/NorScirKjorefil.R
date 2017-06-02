#Lage eksempeldatasett
rm(list=ls())
NorScirEksData <- read.table('E:/Registre/NorScir/data/NorScirEksempeldata.csv', header=T, sep=';')
#perm.sammen:
permA <- c('InjuryDt', 'AdmitDt', 'DischgDt', 'ANeuExmDt', 'FNeuExmDt', 'QolDt', 
		'AdmitRehDt', 'CNeuExmDt', 'InjuryDateUnknown')

permB <- c('ReshId', 'ShNavn')
N <- dim(NorScirEksData)[1]

NorScirEksData[ ,permA] <- NorScirEksData[sample(N, N),permA]
NorScirEksData[ ,permB] <- NorScirEksData[sample(N, N),permB]
NorScirEksData$PasientId <- NorScirEksData$PasientId[sample(N,N)]
NorScirEksData$isMale <- NorScirEksData$isMale[sample(N,N)]
NorScirEksData$AlderAar <- NorScirEksData$AlderAar[sample(N,N)]

save(NorScirEksData, file='E:/Registre/NordicScir/data/NorScirEksData.Rdata')
#write.table(NorScirEksData, file='E:/Registre/NordicScir/data/NorScirEksData.csv', sep=';')

#--------------------------------------SAMLERAPPORT-----------------------------------
rm(list=ls())
library(knitr)
NSdata <- HovedSkjema
setwd("C:/ResultattjenesteGIT/nordicscir/inst")
reshID <- 107627	#0 - alle	#105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs

library(tools)	#texi2pdf
knit('NSsamleRappLand.Rnw')
texi2pdf('NSsamleRappLand.tex')
knit('NSsamleRapp.Rnw')
texi2pdf('NSsamleRapp.tex')


#------------------------ LASTE DATA -------------------------------------

rm(list=ls())
dato <- 'FormDataContract2017-05-24'
HovedSkjema <- read.table(paste0('C:/Registre/NordicScir/data/Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Livskvalitet <- read.table(paste0('C:/Registre/NordicScir/data/LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Kontroll <- read.table(paste0('C:/Registre/NordicScir/data/Control',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Urin <- read.table(paste0('C:/Registre/NordicScir/data/UrinaryTractFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Tarm <- read.table(paste0('C:/Registre/NordicScir/data/BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Satisfact <- read.table(paste0('C:/Registre/NordicScir/data/ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Performance <- read.table(paste0('C:/Registre/NordicScir/data/ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)

HovedSkjema$SkjemaGUID <- tolower(HovedSkjema$SkjemaGUID)
Performance$SkjemaGUID <- tolower(Performance$SkjemaGUID)
Kontroll$SkjemaGUID <- tolower(Kontroll$SkjemaGUID)


KobleMedHoved <- function(HovedSkjema,Skjema2) {
      varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
      Skjema2 <- Skjema2[ ,c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
      NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','XX'),
                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = F, all.y=F)
return(NSdata)
}

RegData <- KobleMedHoved(HovedSkjema,Tarm)

#------------------------ TESTE DATA -------------------------------------

#---Oppsummering/test av andel som har fått oppfølging---
paste0('Ant. Livskvalitet m/hovedskjema: ',sum(HovedSkjema$SkjemaGUID %in% Livskvalitet$HovedskjemaGUID))
paste0('Ant. Performance m/hovedskjema: ', sum(HovedSkjema$SkjemaGUID %in% Performance$HovedskjemaGUID))
paste0('Ant. Satisfaction m/Performance: ', sum(Satisfact$HovedskjemaGUID %in% Performance$SkjemaGUID))
sum(test)

tabVar <- c('HealthUnitName','Aar')
Hskjema <- HovedSkjema[c('SkjemaGUID','HealthUnitName','AdmitDt')][order(HovedSkjema$SkjemaGUID),]
RaaTab <- cbind(Hskjema,
      Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
      MedLivskval = match(Hskjema$SkjemaGUID, sort(Livskvalitet$HovedskjemaGUID)),
      MedKtr = match(HovedSkjema$SkjemaGUID, sort(Kontroll$HovedskjemaGUID)),
      MedSatisfact = match(HovedSkjema$SkjemaGUID, sort(Satisfact$HovedskjemaGUID)),
      MedPerform = match(HovedSkjema$SkjemaGUID, sort(Performance$HovedskjemaGUID))
)
#RaaTab <- RaaTab1[which(as.Date(Hskjema$AdmitDt)> '2014-12-31'),]
Tot <- table(is.na(RaaTab$MedLivskval))/dim(RaaTab)[1]*100  #'MedKtr'
AntLivskval <- ftable(RaaTab[!is.na(RaaTab$MedLivskval),tabVar])
AndelLivskval <- round(100*AntLivskval/ftable(RaaTab[,tabVar]),1)
AndelKtr <- round(100*ftable(RaaTab[!is.na(RaaTab$MedKtr),tabVar])/ftable(RaaTab[,tabVar]),1)
AntSatisfact <- ftable(RaaTab[!is.na(RaaTab$MedSatisfact),tabVar])
AndelSatisfact <- round(100*ftable(RaaTab[!is.na(RaaTab$MedSatisfact),tabVar])/ftable(RaaTab[,tabVar]),1)
AndelPerform <- round(100*ftable(RaaTab[!is.na(RaaTab$MedPerform),tabVar])/ftable(RaaTab[,tabVar]),1)


#------------------------------ Parametre --------------------------
rm(list=ls())
library(nordicscir)
setwd("C:/ResultattjenesteGIT/nordicscir/")
reshID <- 105593             ##105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs, standard i funksj: 0 dvs. 'Alle'. Standard i rapporten skal v?re at man f?r opp eget sykehus.
enhetsUtvalg <- 0
minald <- 0
maxald <- 130
erMann <- 9                      #1-menn, 0-kvinner, Standard: '', dvs. begge
traume <- ''    #'ja','nei', standard: ikke valgt
AIS <- 99 # as.character(c(1,4))	#AISgrad ved innleggelse alle(''), velge en eller flere fra 1:5
paratetra <- 99
datoFra <- '2016-01-01'             #Standard: b?r v?re minste registrerte verdi ? min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
valgtMaal='gjsn'	#'Med'-median, 'Gjsn' gjennomsnitt
grVar <- 'ShNavn'

#------------------------------ Fordelinger --------------------------
RegData <- HovedSkjema
valgtVar <- 'Alder'	#M? velge... AAis, FAis, Alder, DagerRehab, DagerTilRehab, NivaaInn
							#OpphTot[HosptlDy], Permisjon[OutOfHosptlDy], UtTil[PlaceDis], SkadeArsak[Scietiol]  
							#Pustehjelp[VentAssi]
#UrinSkjema: 
RegData <- KobleMedHoved(HovedSkjema,Urin)
valgtVar <- 'UrinLegemidler'   #'UrinInkontinens', 'UrinLegemidler','UrinLegemidlerHvilke', 'UrinKirInngr', 
                                    #'UrinTomBlareHoved', 'UrinTomBlareTillegg'
#TarmSkjema: 
RegData <- KobleMedHoved(HovedSkjema,Tarm)
valgtVar <- 'TarmAvfHoved'   #'TarmAvfHoved','TarmAvfTillegg', TarmAvfmiddel, TarmAvfmiddelHvilke
                              #TarmInkontinens, TarmKirInngrep, TarmKirInngrepHvilke

#Livskvalitet
RegData <- KobleMedHoved(HovedSkjema,Livskvalitet)
valgtVar <- 'LivsGen'                #LivsGen, LivsFys, LivsPsyk

outfile <- '' #paste0(valgtVar, '.png')	#Navn angis av Jasper

NSFigAndeler(RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, paratetra=paratetra,
		reshID=reshID, enhetsUtvalg=enhetsUtvalg, hentData=0)    #, preprosess=1
#Aktuelt å legge til en parameter som sier hvilket skjema variabelen tilhører. Dette for å koble
#sammen riktig skjema til hovedskjema.

#Hoved
variable <- c('AAis', 'FAis', 'Alder', 'DagerRehab', 'DagerTilRehab', 'NivaaInn', 'NivaaUt',
              'OpphTot', 'UtTil', 'SkadeArsak')
#Urin
variable <- c('UrinInkontinens', 'UrinLegemidler','UrinLegemidlerHvilke', 'UrinKirInngr', 
                  'UrinTomBlareHoved', 'UrinTomBlareTillegg')
#Tarm
variable <- c('TarmAvfHoved','TarmAvfTillegg', 'TarmAvfmiddel', 'TarmAvfmiddelHvilke',
                  'TarmInkontinens', 'TarmKirInngrep', 'TarmKirInngrepHvilke')
#Livs
variable  <- c('LivsGen', 'LivsFys', 'LivsPsyk')

for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '.png')
	NSFigAndeler(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	             AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, paratetra=paratetra,
	             reshID=reshID, enhetsUtvalg=enhetsUtvalg, hentData=0)
}

#------------------------------ Sentralmål --------------------------
outfile <- '' #paste(valgtVar, '.png', sep='')	#Navn angis av Jasper
valgtVar <- 'DagerRehab'   #'Alder', 'DagerRehab', 'DagerTilRehab', 'OpphTot', 'LivsGen', 'LivsFys', 'LivsPsyk'
NSFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		valgtMaal=valgtMaal, AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume)

variable <- c('Alder', 'DagerRehab', 'DagerTilRehab', 'OpphTot') 
variable <- c('LivsGen', 'LivsFys', 'LivsPsyk') #Koble på livskvalitetsdata
for (valgtVar in variable) {
	outfile <- paste0('M_',valgtVar, '.png')
	NSFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	               valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, traume=traume,
	               AIS=AIS, paratetra = paratetra)
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
