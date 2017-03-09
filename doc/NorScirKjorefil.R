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

#------------------------Teste data-------------------------------------
HovedSkjema <- read.table('C:/Registre/NordicScir/data/MainFormDataContract2017-02-13.csv', sep=';', header=T)
Livskvalitet <- read.table('C:/Registre/NordicScir/data/LifeQualityFormDataContract2017-02-13.csv', sep=';', header=T)
Kontroll <- read.table('C:/Registre/NordicScir/data/ControlFormDataContract2017-02-13.csv', sep=';', header=T)
Bowel <- read.table('C:/Registre/NordicScir/data/BowelFunctionFormDataContract2017-02-13.csv', sep=';', header=T)
Fornoyd <- read.table('C:/Registre/NordicScir/data/ActivityAndParticipationSatisfactionFormDataContract2017-02-13.csv', sep=';', header=T)
Performance <- read.table('C:/Registre/NordicScir/data/ActivityAndParticipationPerformanceFormDataContract2017-02-13.csv', sep=';', header=T)

#Sjekk for hvilke variabelnavn som finnes i begge datasett
varBegge <- intersect(names(Livskvalitet),names(HovedSkjema))
Livskvalitet <- Livskvalitet[ ,c("HovedskjemaGUID", names(Livskvalitet)[!(names(Livskvalitet) %in% varBegge)])]  #"SkjemaGUID",

HovedSkjema$SkjemaGUID <- tolower(HovedSkjema$SkjemaGUID)

NSdata <- merge(HovedSkjema, Livskvalitet, suffixes = c('','XX'),
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = F, all.y=F)

#---Oppsummering/test av andel som har fått oppfølging---
test <- HovedSkjema$SkjemaGUID %in% Livskvalitet$HovedskjemaGUID

tabVar <- c('HealthUnitName','Aar')
Hskjema <- HovedSkjema[c('SkjemaGUID','HealthUnitName','AdmitDt')][order(HovedSkjema$SkjemaGUID),]
RaaTab <- cbind(Hskjema,
      Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
      MedLivskval = match(Hskjema$SkjemaGUID, sort(Livskvalitet$HovedskjemaGUID)),
      MedKtr = match(HovedSkjema$SkjemaGUID, sort(Kontroll$HovedskjemaGUID))
)
#RaaTab <- RaaTab1[which(as.Date(Hskjema$AdmitDt)> '2014-12-31'),]
Tot <- table(is.na(RaaTab$MedLivskval))/dim(RaaTab)[1]*100  #'MedKtr'
AndelLivskval <- round(100*ftable(RaaTab[!is.na(RaaTab$MedLivskval),tabVar])/ftable(RaaTab[,tabVar]),1)
AndelKtr <- round(100*ftable(RaaTab[!is.na(RaaTab$MedKtr),tabVar])/ftable(RaaTab[,tabVar]),1)




#--------------------------------------SAMLERAPPORT-----------------------------------
rm(list=ls())
library(knitr)
NSdata <- read.table('C:/Registre/NordicScir/data/MainFormDataContract2016-06-08.csv', sep=';', header=T)
setwd("C:/ResultattjenesteGIT/nordicscir/inst")
reshID <- 107627	#0 - alle	#105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs

library(tools)	#texi2pdf
knit('NSsamleRappLand.Rnw')
texi2pdf('NSsamleRappLand.tex')
knit('NSsamleRapp.Rnw')
texi2pdf('NSsamleRapp.tex')
 

NSurin <- read.table('C:/Registre/NordicScir/data/UrinaryTractFunctionFormDataContract2016-09-21.csv', sep=';', header=T)

#------------------------------ Fordelinger --------------------------
rm(list=ls())
NSdata <- read.table('C:/Registre/NordicScir/data/MainFormDataContract2016-06-08.csv', sep=';', header=T)
RegData <- NSdata

setwd("C:/ResultattjenesteGIT/nordicscir/")
reshID <- 107627             ##105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs, standard i funksj: 0 dvs. 'Alle'. Standard i rapporten skal v?re at man f?r opp eget sykehus.
enhetsUtvalg <- 1
minald <- 0
maxald <- 130
traume <- ''    #'ja','nei', standard: ikke valgt
AIS <- '' # as.character(c(1,4))	#c('A','B','U')		#AISgrad ved innleggelse alle(''), velge en eller flere fra A,B,C,D,E,U
#<defaultValueExpression><![CDATA["all"]]></defaultValueExpression>
#      </parameter>
datoFra <- '2011-01-01'             #Standard: b?r v?re minste registrerte verdi ? min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
erMann <- 1                      #1-menn, 0-kvinner, Standard: '', dvs. begge
valgtVar <- 'AAis'	#M? velge... AAis, FAis, Alder, DagerRehab, DagerTilRehab, 
							#OpphTot[HosptlDy], Permisjon[OutOfHosptlDy], UtTil[PlaceDis], SkadeArsak[Scietiol]  
							#Pustehjelp[VentAssi]
outfile <- paste(valgtVar, '.png', sep='')	#Navn angis av Jasper

NSFigAndeler(RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, reshID=reshID, 
      		enhetsUtvalg=enhetsUtvalg, hentData=0)    #, preprosess=1
	

for (valgtVar in c('AAis', 'FAis', 'Alder', 'DagerRehab', 'DagerTilRehab', 
				'OpphTot', 'UtTil', 'SkadeArsak', 'Pustehjelp')) {
	outfile <- paste0(valgtVar, '.png')
	NSFigAndeler(RegData=RegData, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	             AIS=AIS, minald=minald, maxald=maxald, erMann=erMann, traume=traume, reshID=reshID, 
	             enhetsUtvalg=enhetsUtvalg, hentData=0)
}

I_ABC <- which(RegData$AAis %in% c('A','B','C'))

#------------------------------ Sentralm?l --------------------------
rm(list=ls())
#load('C:/Registre/NordicScir/data/NSdata.Rdata')
NSdata <- read.table('C:/Registre/NordicScir/data/MainFormDataContract2016-06-08.csv', sep=';', header=T)
RegData <- NSdata
#RegData <- NSdata
# Inndata til funksjon:
#egenReshID <- #105593-Haukeland, 106896-Sunnaas sykehus, 107627-St.Olavs #M? sendes med til funksjon

##105593-Haukeland, 106896-Sunnaas, 107627-St.Olavs
minald <- 0
maxald <- 130
traume <- ''    #'ja','nei', standard: ikke valgt
datoFra <- '2010-01-01'             #Standard: b?r v?re minste registrerte verdi ? min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-05-25'
erMann <- ''                   #1-menn, 0-kvinner, Standard: '', dvs. begge
valgtVar <- 'Alder'	#M? velge... Alder, DagerRehab, DagerTilRehab, OpphTot[HosptlDy], 
							#Permisjon[OutOfHosptlDy],
valgtMaal=''	#'Med'-median, ellers gjennomsnitt
setwd("C:/ResultattjenesteGIT/nordicscir/")
outfile <- '' #paste(valgtVar, '.png', sep='')	#Navn angis av Jasper

NSFigGjsnGrVar(RegData=NSdata, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, traume=traume)

for (valgtVar in c('Alder', 'DagerRehab', 'DagerTilRehab', 'OpphTot', 'Permisjon')) {
	outfile <- paste(valgtVar, '.png', sep='')
	NSFigGjsnGrVar(RegData=NSdata, outfile=outfile, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
	               valgtMaal=valgtMaal, minald=minald, maxald=maxald, erMann=erMann, traume=traume)
}
#------------------------------ Nevrologisk kategori --------------------------
rm(list=ls())
#load('C:/Registre/NordicScir/data/NSdata.Rdata')
NSdata <- read.table('C:/Registre/NordicScir/data/NordicScir2014-09-30.csv', sep=';', header=T)
RegData <- NSdata
# Inndata til funksjon:

reshID <- 107627	##105593-Haukeland, 106896-Sunnaas sykehus, 107627-St.Olavs
#egenavd <- 1 #1:eget sykehus, 0:hele landet (standard) Kun for valgtVar=='NevrNivaaInnUt'
erMann <- 0 #kj?nn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
minald <- 0 #alder, fra og med
maxald <- 130 #alder, til og med
traume <- 'ja' #'ja','nei', standard: ikke valgt
datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2015-05-25'
enhetsUtvalg <- 0 #1:eget sykehus mot resten(standard), 0:hele landet, 2: eget 
valgtVar <- 'NevrNivaaInn'	#M? velge... NevrNivaaInnUt, NevrNivaaInn, NevrNivaaUt, 
outfile <- paste(valgtVar, '.png', sep='') #navn p? fil figuren skrives ned til
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
