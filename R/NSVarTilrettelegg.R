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
            #-1: Velg verdi, 1:A Komplett skade, 2:B Inkomplett, 3:C Inkomplett, 4:D Inkomplett, 5:E Normal, 
            #9: U Ukjent eller ikke anvendbar
            tittel <- switch(valgtVar, AAis = 'AIS ved innleggelse', FAis = 'AIS ved utskriving') #paste('Fordeling av', )
            grtxt <- c('A','B','C','D','E','U')
            # Tar med -1 i Ukjent
            RegData$VariabelGr[RegData$VariabelGr==-1] <- 9
            xAkseTxt <- 'AIS kategori'
            RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9), labels = grtxt) 
      }
      
      if (valgtVar %in% c('DagerRehab', 'DagerTilRehab', 'OpphTot')) {
            
            grmax <- switch(valgtVar,
                            DagerRehab= '360+',
                            DagerTilRehab = '100+',
                            OpphTot = '300+')
            gr <- switch(valgtVar, 
                         DagerRehab = c(seq(0, 180, 20), 360, 1000),
                         DagerTilRehab = c(seq(0, 100, 10), 1000),
                         OpphTot = c(seq(0, 300, 30), 1000))
            RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt2 <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], grmax)
            tittel <- switch(valgtVar, 
                             DagerRehab='Antall dager med spesialisert rehabilitering', 
                             DagerTilRehab = 'Tid fra innleggelse til spesialisert rehabilitering',
                             OpphTot = 'Antall døgn innlagt på sykehus')
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
      if (valgtVar == 'Pustehjelp') {
            tittel <- 'Ventilasjonsstøtte'
            #gr <- (0:3,9) - Kodene som registereres. Nå bare 0:3?
            grtxt <- c('Nei', 'Mindre enn 24t/dag', 'Hele døgnet', 'Ukjent ant timer')
            xAkseTxt <- ''
            RegData$VariabelGr <- factor(as.numeric(RegData$VariabelGr), levels=c(0:3), labels = grtxt)
            retn <- 'H'
      }
      
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
      

      
#---------------KATEGORISKE
      if (valgtVar=='InnMaate') {
            tittel <- 'Fordeling av Innkomstmåte'   
            indMed <- which((RegData$InnMaate %in% c(0,6,8)))  #Maybe not neccesary just want to make sure that no other values than 0,6,8 
            RegData <- RegData[indMed, ]             
            gr <- c(0,6,8)
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            xAkseTxt <- 'Innkomstmåte'
      }
 
 
      
      
      UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, sortAvtagende=sortAvtagende)
      #!!!!!!!!!!!!!RegData inneholder nå variablene 'Variabel' og 'VariabelGr'. Hm. her er bare VariabelGr... Sjekk med INTENSIV
      return(invisible(UtData)) 
      
}