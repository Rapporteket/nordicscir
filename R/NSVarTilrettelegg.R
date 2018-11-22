#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'    }
#'    
#' NB: valgtVar må komponeres slik at den henspeiler på hvilken 
#' tabell variabelen kommer fra. Dette for å senere kunne hente og koble dataene riktig.
#' valgtSkjema <- substr(valgtVar,1,4) 
#' Prefixet til  \emph{valgtVar} skal være følgende:
#'    \itemize{
#'    \item Livs - LifeQuality, 
#'    \item Urin - UrinaryTractFunction
#'    \item Tarm - BowelFunction
#'    \item Kont - Control
#'    \item Funk - ActivityAndParticipationPerformance
#'    \item Tilf - ActivityAndParticipationSatisfaction
#'         }
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'    	}							
#'    				
#' @inheritParams NSFigAndeler
#' @inheritParams NSUtvalg
#'				
#' @return Definisjon av valgt variabel.
#' 
#' @export
#'

NSVarTilrettelegg  <- function(RegData, valgtVar, grVar='', figurtype='andeler'){
      
      
      "%i%" <- intersect
      
      #----------- Figurparametre ------------------------------
      cexgr <- 1.1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      xAkseTxt <- ''	#Benevning
      flerevar <- 0
      
      grNavn <- ''
      xAkseTxt <- ''
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- FALSE  #Sortering av resultater
      KImaal <- NA
 
#Fra NordicScir
      txtretn <- 1
#      Nutv <- N
 
      
      RegData$VariabelGr <- 0
      variable <- ''

      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      # For hver valgtVar:
      # Definer og gjør utvalg for variabelen
      # tittel, xAkseTxt, sortAvtagende (standard: TRUE)
      
 tittel <- '' #I AndelerGrVar og gjsnGrVar genereres tittel i beregningsfunksjonen

      if (valgtVar=='Alder') { #Fordeling, gjsnGrVar
            tittel <- 'Alder ved innleggelse'
            gr <- c(0,15,30,45,60,75,200)	#c(seq(0, 90, 15), 120)
            RegData$Variabel <- RegData$Alder   #til gjsnGrVar
            RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('0-14','15-29','30-44','45-59','60-74','75+')
            cexgr <- 0.9
            xAkseTxt <- switch(figurtype,
                               andeler= 'Aldersgrupper (år)',
                               gjsnGrVar = 'alder (år)')
      }

      if (valgtVar %in% c('AAis', 'FAis')) {
            retn <- 'H'
            #-1: Velg verdi, 1:A Komplett skade, 2:B Inkomplett, 3:C Inkomplett, 4:D Inkomplett, 5:E Normal, 
            #9: U Ukjent eller ikke anvendbar
			#Fra 01.01.2015 
			#Aais == 9 & ANeuNoMeasure = FALSE: Ukjent/Ikke klassifiserbar
            #Aais == -1 & ANeuNoMeasure = TRUE: Ikke utført. Tilsv. F
		RegData <- RegData[RegData$InnDato >= as.Date('2015-01-01'), ]
            RegData$VariabelGr <- RegData[,valgtVar]
            if (valgtVar == 'AAis') {
                  RegData$VariabelGr[which((RegData$AAis == 9) & (RegData$ANeuNoMeasure == FALSE))] <- 6
                  RegData$VariabelGr[which((RegData$AAis == -1) & (RegData$ANeuNoMeasure == TRUE))] <- 7
                  #RegData$VariabelGr[which((RegData$AAis == 9) & (RegData$ANeuNoMeasure == 0))] <- 6
                  #RegData$VariabelGr[which((RegData$AAis == -1) & (RegData$ANeuNoMeasure == -1))] <- 7
                  }
            if (valgtVar == 'FAis') {
                  #RegData$VariabelGr[which((RegData$FAis == 9) & (RegData$FNeuNoMeasure == 0))] <- 6
                  #RegData$VariabelGr[which((RegData$FAis == -1) & (RegData$FNeuNoMeasure == -1))] <- 7
                  RegData$VariabelGr[which((RegData$FAis == 9) & (RegData$FNeuNoMeasure == FALSE))] <- 6
                  RegData$VariabelGr[which((RegData$FAis == -1) & (RegData$FNeuNoMeasure == TRUE))] <- 7
            }
            tittel <- switch(valgtVar, AAis = 'AIS ved innleggelse', FAis = 'AIS ved utskriving') #paste('Fordeling av', )
            grtxt <- c('A: Komplett', 'B: Inkomplett', 'C: Inkomplett', 'D: Inkomplett', 'E: Normal', 
						'Ukjent/Ikke klassifiserbar', 'Ikke utført')	#c('A','B','C','D','E','U')
            xAkseTxt <- 'AIS-kategori'
            RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:7), labels = grtxt) 
      }
      
      if (valgtVar %in% c('DagerRehab', 'DagerTilRehab', 'OpphTot')) {
            dato <- switch(valgtVar, 
                          DagerRehab = '2015-01-01',
                          DagerTilRehab = '2015-01-01',
                          OpphTot = '2011-01-01')
            RegData <- RegData[RegData$InnDato >= as.Date(dato), ]
            grmax <- switch(valgtVar,
                            DagerRehab= '180+',
                            DagerTilRehab = '100+',
                            OpphTot = '240+')
            gr <- switch(valgtVar, 
                         DagerRehab = c(seq(0, 160, 20), 180, 1000),
                         DagerTilRehab = c(seq(0, 100, 10), 1000),
                         OpphTot = c(seq(0, 240, 30), 1000))
            RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
            RegData$Variabel <- RegData[,valgtVar]
            grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], grmax)
            tittel <- switch(valgtVar, 
                             DagerRehab='Tid innlagt på ryggmargsskadeavdeling', 
                             DagerTilRehab = 'Tid fra akuttinnleggelse til innleggelse på ryggmargsskadeavd.',
                             OpphTot = 'Total tid innlagt på sykehus')
            cexgr <- 0.9
            txtretn <- 2
            xAkseTxt <- 'Antall dager'
      }	#Variable med antall dager
      
#      if (valgtVar=='Permisjon') {
#            tittel <- 'Antall døgn ute av sykehus'
#            gr <- c(0,1,7,14,21,28,35, 1000)
#            grmax <- '50+'
#            RegData$VariabelGr <- cut(RegData$Permisjon, breaks=gr, include.lowest=TRUE, right=FALSE)
#            grtxt <- c('0','1-7','8-14','15-21','22-28','29-35', '35+')
#            cexgr <- 0.9
#            xAkseTxt <- 'Antall døgn'
#      }
      
      if (valgtVar == 'NivaaInn') {
            tittel <- 'Nivå ved innleggelse'
            #gr <- (1:6,9) - Kodene som registereres
            grtxt <- c('Paraplegi','Tetraplegi','Ukjent')
            #xAkseTxt <- ''
            RegData$VariabelGr <- factor(as.numeric(RegData$TetraplegiInn), levels=c(0:1,9), labels = grtxt)
      }
      if (valgtVar == 'NivaaUt') {
            tittel <- 'Nivå ved utreise'
            #gr <- (1:6,9) - Kodene som registereres
            grtxt <- c('Paraplegi','Tetraplegi','Ukjent')
            #xAkseTxt <- ''
            RegData$VariabelGr <- factor(as.numeric(RegData$TetraplegiUt), levels=c(0:1,9), labels = grtxt)
      }
      
      if (valgtVar == 'Ntsci') {
            tittel <- 'Ikke-traumatisk skadeårsak (NTSCI)'
            #gr <- (1:6, 8:9) - Kodene som registereres
            RegData <- RegData[which(RegData$Ntsci %in% 1:9) %i% 
                                     which(RegData$InnDato >= as.Date('2018-01-01')), ] 
            grtxt <- c('Medfødt/genetisk etiologi', 'Degenerativ etiologi', 'Tumor, godartet', 
                       'Tumor, ondartet', 'Vaskulær etiologi', 'Infeksjon', 
                       'Annen ryggmargsdysfunksjon', 'Ikke spesifisert/ukjent')
            #xAkseTxt <- ''
            RegData$VariabelGr <- factor(as.numeric(RegData$Ntsci), levels=c(1:6,8:9), labels = grtxt)
            retn <- 'H'
      }
      if (valgtVar == 'SkadeArsak') { #Andeler
            tittel <- 'Skadeårsaker'
            #gr <- (1:6,9) - Kodene som registereres
            RegData$SkadeArsak[which(RegData$SkadeArsak==9)] <- 7
            grtxtAlle <- c('Idrett', 'Vold', 'Transport', 'Fall', 'Andre traumer',
                           'Ikke-traumatisk', 'Uspesifisert')
            grtxt <- grtxtAlle
            xAkseTxt <- 'Utskrevet til'
            RegData$VariabelGr <- factor(as.numeric(RegData$SkadeArsak), levels=1:7, labels = grtxtAlle)
            retn <- 'H'
      }
      if (valgtVar == 'UtTil') {
            tittel <- 'Utskrevet til'
            #gr <- (1:10,99) - Kodene som registereres
            RegData$UtTil[which(RegData$UtTil==99)] <- 12
            RegData <- RegData[RegData$UtTil %in% 1:12, ]
            grtxtAlle <- c('Hjem', 'Sykehus', 'Sykehjem', 'Omsorgsbolig', 'Bofellesskap','Kriminalomsorg', 'Hotell', 'Bostedsløs', 'Avdød', 'Annet', 'Planlagt hjem', 'Ukjent')
            grtxt <- grtxtAlle
            xAkseTxt <- 'Utskrevet til'
            RegData$VariabelGr <- factor(as.numeric(RegData$UtTil), levels=1:12, labels = grtxtAlle)
            #Vurder om skal ta med bare de som er registrert
            #grtxt <- grtxtAlle[as.numeric(names(table(as.numeric(RegData$PlaceDis))))] #De som er reg.
            retn <- 'H'
      }
      if (valgtVar == 'PPlaceDis') {
            tittel <- 'Planlagt midlertidig utskrevet til '
            RegData$PPlacedis[RegData$PPlacedis==8] <- 5
            RegData <- RegData[which(RegData$PPlacedis %in% 1:5) %i% 
                                     which(RegData$InnDato >= as.Date('2018-01-01')), ]
            grtxt <- c('Pleiehjem/ \n avlastningsplass', 'Insitusjon \n m/trening', 
                           'Sykehus', 'Familie/slekt \n /venner', 'Annet')
            xAkseTxt <- 'Utskrevet til'
            RegData$VariabelGr <- factor(as.numeric(RegData$PPlacedis), levels=1:5, labels = grtxt)
            retn <- 'H'
      }
      if (valgtVar == 'RegForsinkelse') {  #Andeler, GjsnGrVar
            #Verdier: 0-3402
            RegData$Diff <- as.numeric(as.Date(as.POSIXct(RegData$FirstTimeClosed, format="%Y-%m-%d")) - 
                                             as.Date(as.POSIXct(RegData$DischgDt, format="%Y-%m-%d"))) #difftime(RegData$InnDato, RegData$Leveringsdato) #
#RegData[,c('InnDato', "FirstTimeClosed", "DischgDt", 'Diff')]
 #           RegData$InnDato <- as.Date(RegData$AdmitDt, format="%Y-%m-%d") #as.Date(RegData$AdmitDt, format="%Y-%m-%d")
  #          RegData$Test <- as.Date(RegData$FirstTimeClosed, format="%Y-%m-%d")
            RegData <- RegData[which(RegData$Diff > -1), ]
            tittel <- switch(figurtype,
                             andeler='Tid fra utskriving til ferdigstilt registrering',
                             andelGrVar = 'Mer enn 30 dager fra utskriving til ferdig registrering') #
            #RegData$Variabel[RegData$Diff > 2*7] <- 1
            RegData$Variabel <- RegData$Diff
            subtxt <- 'døgn'
            gr <- c(0,1,7,14,30,90,365,5000) #gr <- c(seq(0, 90, 10), 1000)
            RegData$VariabelGr <- cut(RegData$Diff, breaks = gr, include.lowest = TRUE, right = TRUE)
            grtxt <- c('1', '(1-7]', '(7-14]', '(14-30]', '(30-90]', '(90-365]', '>365')
            cexgr <- 0.9
            xAkseTxt <- 'dager'
            sortAvtagende <- FALSE
      }
      
#----------------URIN-skjema (start 01.01.2015):
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen

      if (substr(valgtVar,1,4)=='Livs') {
            indDato <- which(RegData$InnDato >= as.Date('2015-01-01')) #Ikke filtrer på startdato
            indTidspkt <- which(RegData$QolDt <= RegData$DischgDt)
            RegData <- RegData[indTidspkt, ]
            xAkseTxt <- 'Skåring: 0-10. Høyest er best.'
      }
      
      if (valgtVar == 'LivsGen') {
            tittel <- 'Tilfredshet med livet'
            RegData <- RegData[RegData$SatGenrl %in% 0:10, ]
            grtxt <- 0:10
            RegData$VariabelGr <- factor(as.numeric(RegData$SatGenrl), levels=0:10, labels = grtxt)
            RegData$Variabel <- as.numeric(RegData$SatGenrl)
            sortAvtagende <- TRUE
      }
      if (valgtVar == 'LivsFys') {
            tittel <- 'Tilfredshet med fysisk helse'
            RegData <- RegData[RegData$SatPhys %in% 0:10, ]
            grtxt <- 0:10
            RegData$VariabelGr <- factor(as.numeric(RegData$SatPhys), levels=0:10, labels = grtxt)
            RegData$Variabel <- as.numeric(RegData$SatPhys)
            sortAvtagende <- TRUE
      }

      if (valgtVar == 'LivsPsyk') {
            tittel <- 'Tilfredshet med psykisk helse'
            RegData <- RegData[RegData$SatPsych %in% 0:10, ]
            grtxt <- 0:10
            RegData$VariabelGr <- factor(as.numeric(RegData$SatPsych), levels=0:10, labels = grtxt)
            RegData$Variabel <- as.numeric(RegData$SatPsych)
            sortAvtagende <- TRUE
      }
   
#----------------URIN-skjema (start 01.01.2015):
      if (substr(valgtVar,1,4)=='Urin') {
       RegData <- RegData[which(RegData$InnDato >= as.Date('2015-01-01') & 
                                   RegData$LutfxnDt <= RegData$DischgDt), ]}
 
      if (valgtVar=='UrinInkontinens') {
            #0:4,9: Nei, Ja daglig, Ja ukentlig, Ja månedlig, Ikke relevant, Ukjent	
            tittel <- 'Ufrivillig urinlekkasje'
            gr <- c(0:4,9)
            RegData <- RegData[RegData$Incontnc %in% gr,]
            grtxt <- c('Nei', 'Ja, daglig', 'Ja, ukentlig', 'Ja, månedlig', 'Ikke relevant', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$Incontnc, levels = gr, labels = grtxt)
      }
      if (valgtVar=='UrinKirInngr') {
            flerevar <- 1
            retn <- 'H'
            tittel <- 'Kirurgiske inngrep i urinveiene'
            RegData <- RegData[which((RegData$Surgicalpr==1) & (RegData$InnDato >= as.Date('2015-01-01'))), ]
            #For Surgicalpr=='ja' Ta med egen kolonne for nei? Nei, for mange	
            variable <- c('Spcath', 'Bstnrm', 'Ustnrm', 'Bladag', 'Ustent', 'Botox', 'Artsph','Ilvscs', 
                          'Ilurts', 'Ccathv', 'Sarstm', 'Othsrg')
            grtxt <- c('Innsatt suprapubiskateter', 'Fjernet blærestein', 'Fjernet andre stein', 'Blæreforstørrelse', 
                       'Sfinkterotomi', 'Botulinumtoksininjeksjon', 'Kunstig sfinkter', 'Ilovesikostomi', 'Ileoureterostomi',
                       'Kateteriserbar urostomi', 'Sakralnervestimulator', 'Annet')
            #as.Date(RegData$SpcathDt, format="%Y-%m-%d") > '2017-06-01'
            indDato <- cbind(Spcath = as.Date(RegData$SpcathDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Bstnrm = as.Date(RegData$BstnrmDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Ustnrm = as.Date(RegData$UstnrmDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Bladag = as.Date(RegData$BladagDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Ustent = as.Date(RegData$UstentDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Botox = as.Date(RegData$BotoxDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Artsph = as.Date(RegData$ArtsphDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Ilvscs = as.Date(RegData$IlvscsDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Ilurts = as.Date(RegData$IlurtsDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Ccathv = as.Date(RegData$CcathvDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Sarstm = as.Date(RegData$SarstmDt, format="%Y-%m-%d") >= RegData$InnDato,
                             Othsrg = as.Date(RegData$OthsrgDt, format="%Y-%m-%d") >= RegData$InnDato)
            ind1 <- which(RegData[,variable]==TRUE & indDato==TRUE, arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      if (valgtVar=='UrinLegemidler') {
            RegData <- RegData[which(RegData$InnDato >= as.Date('2015-01-01')), ]
            #0:1,9: Nei, Ja, Ukjent	
            tittel <- 'Bruk av legemidler som påvirker urinveiene'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$AnyDrugs %in% gr,]
            grtxt <- c('Nei', 'Ja', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$AnyDrugs, levels = gr, labels = grtxt)
      }
      if (valgtVar=='UrinLegemidlerHvilke') {
            flerevar <- 1
            retn <- 'H'
            tittel <- 'Legemidler som påvirker urinveiene'
            RegData <- RegData[which((RegData$AnyDrugs %in% 0:1) & (RegData$InnDato >= as.Date('2015-01-01'))), ]
            variable <- c('Bladrelx', 'Spncrelx', 'DrugsAnti', 'Antiuti', 'Antiprop', 'Othdrg')
            grtxt <- c('Blæreavslappende legemidler', 'Avslappende, sfinkter/blærehals', 'Antibiotika/antiseptika', 
                       '...behandling av UVI', '...forebyggende', 'Annet')
            cexgr <- 1
            Dum <- RegData
            ind1 <- which(RegData[,variable]==TRUE , arr.ind=T)
            indIkkeAnti <- which(Dum$DrugsAnti==FALSE)
            RegData[ ,variable] <- 0
            RegData[indIkkeAnti ,c('Antiuti', 'Antiprop')] <- NA
            RegData[ ,variable][ind1] <- 1
      }
      
      
      if (valgtVar %in% c('UrinTomBlareHoved','UrinTomBlareTillegg')) {
            flerevar <- 1
            retn <- 'H'
            tittel <- switch(valgtVar,
                             UrinTomBlareHoved = 'Blæretømming, hovedmetode',
                             UrinTomBlareTillegg = 'Blæretømming, tilleggsmetode')
            RegData <- RegData[RegData$InnDato >= as.Date('2015-01-01'), ]
            variable <- switch(valgtVar,
                               UrinTomBlareHoved =  c('EmbladUn', 'EmbladM1', 'EmbladM2', 'EmbladM3', 'EmbladM4', 'EmbladM5', 'EmbladM6', 
                                                  'EmbladM7', 'EmbladM8', 'EmbladM9', 'EmbladM10', 'EmbladM11', 'EmbladM12'),
                               UrinTomBlareTillegg =c('EmbladS1', 'EmbladS2', 'EmbladS3', 'EmbladS4', 'EmbladS5', 'EmbladS6', 
                                                  'EmbladS7', 'EmbladS8', 'EmbladS9', 'EmbladS10', 'EmbladS11', 'EmbladS12'))
            grtxt <- c('Ukjent', 'Normal vannlating', 'Viljestyrt', 'Ufrivillig', 'Pressing', 'Ekstern kompresjon',
                       'Selvkateterisering', 'Kateterisering m/hjelp', 'Transuretralt', 'Suprapubisk', 
                       'Sakral nerverotstimulering', 'Stomi', 'Annen metode')
            if (valgtVar == 'UrinTomBlareTillegg'){grtxt <- grtxt[-1]}
            #Ikke nødvendig å gjøre om til 01 når har boolske variable
            ind1 <- which(RegData[,variable]==TRUE, arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      
#----------------TARM-skjema (start 01.01.2015):
      #BfxnbaDt	<= DischgDt
      if (substr(valgtVar,1,4)=='Tarm') {
            RegData <- RegData[which(RegData$InnDato >= as.Date('2016-01-01') & 
                                           RegData$BfxnbaDt <= RegData$DischgDt), ]}
                               
           
      if (valgtVar=='TarmAvfmiddel') {
            tittel <- 'Bruk av perorale avføringsmidler'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$OralLaxatives %in% gr,]
            grtxt <- c('Nei', 'Ja', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$OralLaxatives, levels = gr, labels = grtxt)
      }
      if (valgtVar=='TarmAvfmiddelHvilke') {
            RegData <- RegData[which(RegData$OralLaxatives==1), ]
            flerevar <- 1
            retn <- 'H'
            tittel <- 'Perorale avføringsmidler'
            variable <- c('Osmodrp', 'Osmotab', 'Irrtdrp', 'Irrttab', 'Prokinet', 'Othorlax')
            grtxt <- c('Osmotisk, flytende', 'Osmotisk, fast form', 'Tarmirriterende, flytende',
                       'Tarmirriterende, fast form', 'Prokinetiske', 'Annet')
            cexgr <- 1
            ind1 <- which(RegData[,variable]==TRUE , arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      if (valgtVar=='TarmInkontinens') {
            retn <- 'H'
            tittel <- 'Hyppighet av fekal inkontinens'
            gr <- c(1:7,9)
            RegData <- RegData[RegData$Fcincfrq %in% gr,]
            grtxt <- c('Mer enn daglig', 'Daglig', 'Ukentlig', 'Mer enn månedlig',
                       'Månedlig', 'Mindre enn månedlig', 'Aldri', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$Fcincfrq, levels = gr, labels = grtxt)
      }
      if (valgtVar=='TarmKirInngrep') {
            tittel <- 'Kirurgisk inngrep i mage/–tarm kanalen'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$SurgicalIntervention %in% gr,]
            grtxt <- c('Nei', 'Ja', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$SurgicalIntervention, levels = gr, labels = grtxt)
      }
      
      if (valgtVar=='TarmKirInngrepHvilke') {
            RegData <- RegData[which(RegData$SurgicalIntervention==1),  ]
            flerevar <- 1
            retn <- 'H'
            tittel <- 'Kirurgiske inngrep i mage/–tarm kanalen'
            variable <- c('Apndec', 'Chcyec', 'Colost', 'Ileost', 'Otgisurg')
            grtxt <- c('Appendektomi','Fjerning av galleblæren', 'Kolostomi', 'Ileostomi', 'Annet')
            cexgr <- 1
            ind1 <- which(RegData[,variable]==TRUE , arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      
      
      if (valgtVar %in% c('TarmAvfHoved','TarmAvfTillegg')) {
            flerevar <- 1
            retn <- 'H'
            tittel <- switch(valgtVar,
                             TarmAvfHoved = 'Avføring, hovedmetode',
                             TarmAvfTillegg = 'Avføring, tilleggsmetode')
            RegData <- RegData[RegData$InnDato >= as.Date('2016-01-01'), ]
            variable <- switch(valgtVar,
                               TarmAvfHoved =  c('DefcmthUn', 'DefcmthM1', 'DefcmthM2', 'DefcmthM3', 'DefcmthM4', 'DefcmthM5',
                                                      'DefcmthM6', 'DefcmthM7', 'DefcmthM8', 'DefcmthM9', 'DefcmthM10'),
                               TarmAvfTillegg =c('OthdefS1', 'OthdefS2', 'OthdefS3', 'OthdefS4', 'OthdefS5',
                                                      'OthdefS6', 'OthdefS7', 'OthdefS8', 'OthdefS9', 'OthdefS10'))
            grtxt <- c('Ukjent', 'Normal avføring', 'Pressing/trykking', 'Digital stimulering', 'Stikkpiller',
                       'Manuell fjerning', 'Miniklyster', 'Klyster (>150ml)', 'Kolostomi', 'Sakralstimulering', 'Annen')
            if (valgtVar == 'TarmAvfTillegg'){grtxt <- grtxt[-1]}
            #Ikke nødvendig å gjøre om til 01 når har boolske variable
            ind1 <- which(RegData[,variable]==TRUE, arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      
      #    \item Tilf - ActivityAndParticipationSatisfaction
      
      #---------------- ActivityAndParticipationPerformance  (start 01.01.2017):
      if (substr(valgtVar,1,4)=='Funk') {
            RegData <- RegData[which(RegData$InnDato >= as.Date('2017-01-01') &
                                           RegData$DataClDt <= RegData$DischgDt), ]}
      #Mobilitet «Mobilmod»,  Av/Påkledning, «Dreslbdy»,   Spising «Feeding», Toalettbesøk «Toiletin»
      
      if (valgtVar=='FunkDo') {
            retn <- 'H'
            tittel <- 'Toalettbesøk'
            gr <- c(0:4,99)
            grtxt <- c('Totalt hjelpetrengende',
                       'Hjelp til enkelte oppgaver (tørker \n seg ikke selv)',
                       'Hjelp til enkelte oppgaver (tørker \n seg selv)',
                       'Selvstendig med hjelpemidler/ \ntilrettelegging',
                       'Selvstendig ved toalettbesøk',
                       'Ukjent')
            RegData <- RegData[RegData$Toiletin %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$Toiletin), levels=gr, labels = grtxt)
      }
      if (valgtVar=='FunkKler') {
            retn <- 'H'
            tittel <- 'Av-/påkledning underkropp'
            gr <- c(0:4,99)
            grtxt <- c('Totalt hjelpetrengende',
                       'Hjelp til enkelte oppgaver ',
                       'Selvstendig med hjelpemidler/ \n tilrettelegging',
                       'Selvstendig, kun hjelp til knapper/ \nglidelås/lisser',
                       'Selvstendig i av-/påkledning alle \ntyper klær',
                       'Ukjent')
            RegData <- RegData[RegData$Dreslbdy %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$Dreslbdy), levels=gr, labels = grtxt)
            
      }
      if (valgtVar=='FunkMob') {
            retn <- 'H'
            tittel <- 'Mobilitet over kortere avstander (10-100m.)'
            gr <- c(0:8,99)
            grtxt <- c('Totalt hjelpetrengende', 
                       'El. rullestol/hjelp til \n manuell rullestol',
                       'Selvstendig i manuell \n rullestol',
                       'Går med tilsyn',
                       'Rullator/krykker',
                       'Krykker/to stokker',
                       'En stokk',
                       'Leggortose',
                       'Går u/hjelpemiddel',
                       'Ukjent')
            RegData <- RegData[RegData$Mobilmod %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$Mobilmod), levels=gr, labels = grtxt)
            
      }
      if (valgtVar=='FunkSpis') {
            retn <- 'H'
            tittel <- 'Spisesituasjon'
            gr <- c(0:3,99)
            grtxt <- c('Totalt hjelpetrengende',
                       'Hjelp til enkelte oppgaver',
                       'Selvstendig med hjelpemidler/ \n tilrettelegging',
                       'Selvstendig i spisesituasjon',
                       'Ukjent')
            RegData <- RegData[RegData$Feeding %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$Feeding), levels=gr, labels = grtxt)
      }
      
      
      
      #---------------- ActivityAndParticipationPerformance  (start 01.01.2017):
      if (substr(valgtVar,1,4)=='Tilf') {
            RegData <- RegData[which((RegData$InnDato >= as.Date('2017-01-01')) &
                                           RegData$DataClDtS <= RegData$DischgDt), ]
            gr <- c(0:2,99)
            grtxt <- c('Ikke tilfreds', 'Ganske tilfreds','Svært tilfreds','Ukjent')
      }
      #Mobilitet «Mobilmod»,  Av/Påkledning, «Dreslbdy»,   Spising «Feeding», Toalettbesøk «Toiletin»
      
      if (valgtVar=='TilfDo') {
            retn <- 'H'
            tittel <- 'Toalettbesøk'
            RegData <- RegData[RegData$ToiletinS %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$ToiletinS), levels=gr, labels = grtxt)
      }
      if (valgtVar=='TilfKler') {
            retn <- 'H'
            tittel <- 'Av-/påkledning underkropp'
            RegData <- RegData[RegData$DreslbdyS %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$DreslbdyS), levels=gr, labels = grtxt)
            
      }
      if (valgtVar=='TilfMob') {
            retn <- 'H'
            tittel <- 'Mobilitet over kortere avstander (10-100m.)'
            RegData <- RegData[RegData$MobilmodS %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$MobilmodS), levels=gr, labels = grtxt)
            
      }
      if (valgtVar=='TilfSpis') {
            retn <- 'H'
            tittel <- 'Spisesituasjon'
            RegData <- RegData[RegData$FeedingS %in% gr, ]
            RegData$VariabelGr <- factor(as.numeric(RegData$FeedingS), levels=gr, labels = grtxt)
      }
      UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, cexgr=cexgr, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      return(invisible(UtData)) 
      
}
