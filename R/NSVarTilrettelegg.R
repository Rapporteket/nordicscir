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
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'    	}							
#'    				
#' @inheritParams NSFigAndeler
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NSVarTilrettelegg  <- function(RegData, valgtVar, grVar=''){
      
      
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
      sortAvtagende <- TRUE  #Sortering av resultater
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
      
 tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen

      if (valgtVar=='Alder') {
            tittel <- 'Aldersfordeling'
            gr <- c(0,16,31,46,61,76,200)	#c(seq(0, 90, 15), 120)
            RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c('[0,15]','[16,30]','[31,45]','[46,60]','[61,75]','76+')
            #	grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '76+')
            cexgr <- 0.9
            xAkseTxt <- 'Aldersgrupper'
      }
       
      if (valgtVar %in% c('AAis', 'FAis')) {
            retn <- 'H'
            #-1: Velg verdi, 1:A Komplett skade, 2:B Inkomplett, 3:C Inkomplett, 4:D Inkomplett, 5:E Normal, 
            #9: U Ukjent eller ikke anvendbar
			#Fra 01.01.2015 
			#Aais == 9 & ANeuNoMeasure = FALSE: Ukjent/Ikke klassifiserbar
            #Aais == -1 & ANeuNoMeasure = TRUE: Ikke utført. Tilsv. F
		RegData <- RegData[RegData$InnDato >= as.POSIXlt('2015-01-01'), ]
            RegData$VariabelGr <- RegData[,valgtVar]
            if (valgtVar == 'AAis') {
                  RegData$VariabelGr[which((RegData$AAis == 9) & which(RegData$ANeuNoMeasure == FALSE))] <- 6
                  RegData$VariabelGr[which((RegData$AAis == -1) & (RegData$ANeuNoMeasure == TRUE))] <- 7
                  }
            if (valgtVar == 'FAis') {
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
            RegData <- RegData[RegData$InnDato >= as.POSIXlt(dato), ]
            grmax <- switch(valgtVar,
                            DagerRehab= '180+',
                            DagerTilRehab = '100+',
                            OpphTot = '240+')
            gr <- switch(valgtVar, 
                         DagerRehab = c(seq(0, 160, 20), 180, 1000),
                         DagerTilRehab = c(seq(0, 100, 10), 1000),
                         OpphTot = c(seq(0, 240, 30), 1000))
            RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], grmax)
            tittel <- switch(valgtVar, 
                             DagerRehab='Tid innlagt på ryggmargsskadeavdeling', 
                             DagerTilRehab = 'Tid fra akuttinnleggelse til innleggelse på ryggmargsskadeavdeling',
                             OpphTot = 'Total tid innlagt på sykehus')
            cexgr <- 0.9
            txtretn <- 2
            xAkseTxt <- 'Antall døgn'
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
      
      if (valgtVar == 'SkadeArsak') {
            tittel <- 'Skadeårsaker'
            #gr <- (1:6,9) - Kodene som registereres
            RegData$VariabelGr[which(RegData$SkadeArsak==9)] <- 7
            grtxtAlle <- c('Idrett', 'Vold', 'Transport', 'Fall', 'Andre traumer',
                           'Ikke-traumatisk', 'Uspesifisert')
            grtxt <- grtxtAlle
            xAkseTxt <- 'Utskrevet til'
            RegData$VariabelGr <- factor(as.numeric(RegData$VariabelGr), levels=1:7, labels = grtxtAlle)
            retn <- 'H'
      }
      if (valgtVar == 'UtTil') {
            tittel <- 'Utskrevet til'
            #gr <- (1:10,99) - Kodene som registereres
            RegData$VariabelGr[which(RegData$UtTil==99)] <- 11
            grtxtAlle <- c('Hjem', 'Sykehus', 'Pleiehjem', 'Omsorgsbolig', 'Bofellesskap',
                           'Kriminalomsorg', 'Hotell', 'Bostedsløs', 'Avdød', 'Annet', 'Ukjent')
            grtxt <- grtxtAlle
            xAkseTxt <- 'Utskrevet til'
            RegData$VariabelGr <- factor(as.numeric(RegData$VariabelGr), levels=1:11, labels = grtxtAlle)
            #Vurder om skal ta med bare de som er registrert
            #grtxt <- grtxtAlle[as.numeric(names(table(as.numeric(RegData$PlaceDis))))] #De som er reg.
            retn <- 'H'
      }
  
#----------------URIN-skjema (start 01.01.2015):
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
      
      #NB:  LutfxnDt	<= DischgDt

      if (valgtVar=='UrinInkontinens') {
            RegData <- RegData[which((RegData$AnyDrugs==1) & (RegData$InnDato >= as.POSIXlt('2015-01-01'))), ]
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
            RegData <- RegData[which((RegData$Surgicalpr==1) & (RegData$InnDato >= as.POSIXlt('2015-01-01'))), ]
            #For Surgicalpr=='ja' Ta med egen kolonne for nei? Nei, for mange	
            variable <- c('Spcath', 'Bstnrm', 'Ustnrm', 'Bladag', 'Ustent', 'Botox', 'Artsph','Ilvscs', 
                          'Ilurts', 'Ccathv', 'Sarstm', 'Othsrg')
            grtxt <- c('Innsatt suprapubiskateter', 'Fjernet blærestein', 'Fjernet andre stein', 'Blæreforstørrelse', 
                       'Sfinkterotomi', 'Botulinumtoksininjeksjon', 'Kunstig sfinkter', 'Ilovesikostomi', 'Ileoureterostomi',
                       'Kateteriserbar urostomi', 'Sakralnervestimulator', 'Annet')
            indDato <- cbind(Spcath = RegData$SpcathDt >= RegData$AdmitDt,
                             Bstnrm = RegData$BstnrmDt>= RegData$AdmitDt,
                             Ustnrm = RegData$UstnrmDt>= RegData$AdmitDt,
                             Bladag = RegData$BladagDt>= RegData$AdmitDt,
                             Ustent = RegData$UstentDt>= RegData$AdmitDt,
                             Botox = RegData$BotoxDt>= RegData$AdmitDt,
                             Artsph = RegData$ArtsphDt>= RegData$AdmitDt,
                             Ilvscs = RegData$IlvscsDt>= RegData$AdmitDt,
                             Ilurts = RegData$IlurtsDt>= RegData$AdmitDt,
                             Ccathv = RegData$CcathvDt>= RegData$AdmitDt,
                             Sarstm = RegData$SarstmDt>= RegData$AdmitDt,
                             Othsrg = RegData$OthsrgDt>= RegData$AdmitDt)
            ind1 <- which(RegData[,variable]==TRUE & indDato==TRUE, arr.ind=T)
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
      }
      if (valgtVar=='UrinLegemidler') {
            RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2015-01-01')), ]
            #0:1,9: Nei, Ja, Ukjent	
            tittel <- 'Bruk av legemidler som påvirker urinveiene'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$AnyDrugs %in% gr,]
            grtxt <- c('Ja', 'Nei', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$AnyDrugs, levels = gr, labels = grtxt)
      }
      if (valgtVar=='UrinLegemidlerHvilke') {
            flerevar <- 1
            retn <- 'H'
            tittel <- 'Legemidler som påvirker urinveiene'
            RegData <- RegData[which((RegData$AnyDrugs==1) & (RegData$InnDato >= as.POSIXlt('2015-01-01'))), ]
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
            RegData <- RegData[RegData$InnDato >= as.POSIXlt('2015-01-01'), ]
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
      
      if (valgtVar=='TarmAvfmiddel') {
            RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2016-01-01')), ]
            tittel <- 'Bruk av perorale avføringsmidler'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$OralLaxatives %in% gr,]
            grtxt <- c('Nei', 'Ja', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$OralLaxatives, levels = gr, labels = grtxt)
      }
      
      if (valgtVar=='TarmKirInngrep') {
            RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2016-01-01')), ]
            tittel <- 'Kirurgisk inngrep i mage/–tarm kanalen'
            gr <- c(0:1,9)
            RegData <- RegData[RegData$SurgicalIntervention %in% gr,]
            grtxt <- c('Nei', 'Ja', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$SurgicalIntervention, levels = gr, labels = grtxt)
      }
      
      if (valgtVar=='TarmKirInngrepHvilke') {
            RegData <- RegData[which((RegData$SurgicalIntervention==1) & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ]
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
      if (valgtVar=='TarmInkontinens') {
            RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2016-01-01')), ]
            tittel <- 'Hyppighet av fekal inkontinens'
            gr <- c(1:7,9)
            RegData <- RegData[RegData$Fcincfrq %in% gr,]
            grtxt <- c('Mer enn daglig', 'Daglig', 'Ukentlig', 'Mer enn månedlig',
                       'Månedlig', 'Mindre enn månedlig', 'Aldri', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$OralLaxatives, levels = gr, labels = grtxt)
      }
      
      if (valgtVar=='TarmAvfmiddelHvilke') {
            RegData <- RegData[which((RegData$OralLaxatives==1) & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ]
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
      
      
      if (valgtVar %in% c('TarmAvfHoved','TarmAvfTillegg')) {
            RegData <- RegData[which(RegData$InnDato >= as.POSIXlt('2016-01-01')), ]
            flerevar <- 1
            retn <- 'H'
            tittel <- switch(valgtVar,
                             TarmAvfHoved = 'Avføring, hovedmetode',
                             TarmAvfTillegg = 'Avføring, tilleggsmetode')
            RegData <- RegData[RegData$InnDato >= as.POSIXlt('2016-01-01'), ]
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
      

      UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, cexgr=cexgr, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #!!!!!!!!!!!!!RegData inneholder nå variablene 'Variabel' og 'VariabelGr'. Hm. her er bare VariabelGr... Sjekk med INTENSIV
      return(invisible(UtData)) 
      
}