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
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller 
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      
      
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
  
#----------------URIN-skjema:
      if (valgtVar=='Inkontinens') {
            #0:4,9: Nei, Ja daglig, Ja ukentlig, Ja månedlig, Ikke relevant, Ukjent	
            tittel <- 'Ufrivillig urinlekkasje'
            gr <- c(0:4,9)
            RegData <- RegData[RegData$Incontnc %in% gr,]
            grtxt <- c('Nei', 'Ja, daglig', 'Ja, ukentlig', 'Ja, månedlig', 'Ikke relevant', 'Ukjent')
            RegData$VariabelGr <- factor(RegData$Incontnc, levels = gr, labels = grtxt)
      }
      
      if (valgtVar=='UrinKirInngr') {
            flerevar <- 1
            tittel <- 'Kirurgiske inngrep i urinveiene'
      #For Surgicalpr=='ja' Ta med egen kolonne for nei? Nei, for mange	
      Innsetting av suprapubiskateter	suprapubiskateter	Spcath		hvis Surgicalpr==1 & SpcathDt>=AdmitDt
      Fjerning av blærestein	Bstnrm		hvis Surgicalpr==1 & BstnrmDt>=AdmitDt
      Fjerning av andre stein	Ustnrm		hvis Surgicalpr==1 & UstnrmDt>=AdmitDt
      Blæreforstørrelse	Bladag		hvis Surgicalpr==1 & BladagDt>=AdmitDt
      Sfinkterotomi	Ustent		hvis Surgicalpr==1 & UstentDt>=AdmitDt
      Botulinumtoksininjeksjon	Botox		hvis Surgicalpr==1 & BotoxDt>=AdmitDt
      Kunstig sfinkter	Artsph		hvis Surgicalpr==1 & ArtsphDt>=AdmitDt
      Ilovesikostomi	Ilvscs		hvis Surgicalpr==1 & IlvscsDt>=AdmitDt
      Ileoureterostomi	Ilurts		hvis Surgicalpr==1 & IlurtsDt>=AdmitDt
      Kateteriserbar urostomi	Ccathv		hvis Surgicalpr==1 & CcathvDt>=AdmitDt
      Sakralnervestimulator	Sarstm		hvis Surgicalpr==1 & SarstmDt>=AdmitDt
      Annet	Othsrg		hvis Surgicalpr==1 & OthsrgDt>=AdmitDt
      
       #(valgtVar == 'NegHend' ) 
            #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
            #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
            # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
            # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
            # som 0.
            #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
            #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
            flerevar <- 1
            variable <- c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')
            #Sjekk <- RegData[,variable]
            retn <- 'H'
            grtxt <- c('Fysisk mishandl.', 'Psykisk mishandl.', 'Overgrep', 'Mobbing')
            ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- NA
            #RegData[,variable] <- 
            RegData[ ,variable][ind01] <- 0
            RegData[ ,variable][ind1] <- 1
            #Beregne direkte:
            #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
            tittel <- 'Negative hendelser'
      }
      }
      
      

      UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, cexgr=cexgr, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, sortAvtagende=sortAvtagende)
      #!!!!!!!!!!!!!RegData inneholder nå variablene 'Variabel' og 'VariabelGr'. Hm. her er bare VariabelGr... Sjekk med INTENSIV
      return(invisible(UtData)) 
      
}