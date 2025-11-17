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
#'     \item Alder: Aldersfordeling, 10-årige grupper
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
#' @inheritParams NSUtvalgEnh
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
  grPP <- ''
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

  tittel <- '' #I AndelerGrVar genereres tittel i beregningsfunksjonen


  #-----------------------Hovedskjema--------------------------------------

  if (valgtVar=='Alder') { #Fordeling, gjsn
    tittel <- ifelse(figurtype == 'andeler',
                     'Alder ved innleggelse',
                     'alder ved innleggelse')
    gr <- c(0,15,30,45,60,75,200)	#c(seq(0, 90, 15), 120)
    RegData$Variabel <- RegData$Alder   #til gjsnGrVar
    RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('0-14','15-29','30-44','45-59','60-74','75+')
    cexgr <- 0.9
    xAkseTxt <- switch(figurtype,
                       andeler= 'Aldersgrupper (år)',
                       gjsn = 'alder (år)')
  }

  if (valgtVar %in% c('AAis', 'FAis', 'AAisFAis', 'KontFAis')) {
     'H'
    #-1: Velg verdi, 1:A Komplett skade, 2:B Inkomplett, 3:C Inkomplett, 4:D Inkomplett, 5:E Normal,
    #9: U Ukjent eller ikke anvendbar
    #Fra 01.01.2015
    #Aais == 9 & ANeuNoMeasure = 0: Ukjent/Ikke klassifiserbar
    #Aais == -1 & ANeuNoMeasure = 1: Ikke utført. Tilsv. F
    RegData <- RegData[RegData$InnDato >= as.Date('2015-01-01'), ]
    retn <- ifelse(figurtype == 'antGr', 'V', 'H')

    tittel <- switch(valgtVar,
                     AAis = 'AIS ved innleggelse',
                     FAis = 'AIS ved utskriving',
                     AAisFAis = 'AIS ved innleggelse og utskriving',
                     KontFAis = 'AIS ved utskriving og 1.kontroll')
    grtxt <- c('A: Komplett', 'B: Inkomplett', 'C: Inkomplett', 'D: Inkomplett', 'E: Normal',
               'Ukjent/Ikke klassifiserbar', 'Ikke utført')
    xAkseTxt <- 'AIS-kategori'
    if (valgtVar == 'AAis') {
      RegData$VariabelGr <- RegData[,valgtVar]
      RegData$VariabelGr[which((RegData$AAis == 9) & (RegData$ANeuNoMeasure == 0))] <- 6
      RegData$VariabelGr[which((RegData$AAis == -1) & (RegData$ANeuNoMeasure == 1))] <- 7
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = 1:7, labels = grtxt)
    }
    if (valgtVar == 'FAis') {
      RegData$VariabelGr <- RegData$FAis
      RegData$VariabelGr[which((RegData$FAis == 9) & (RegData$FNeuNoMeasure == 0))] <- 6
      RegData$VariabelGr[which((RegData$FAis == -1) & (RegData$FNeuNoMeasure == 1))] <- 7
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = 1:7, labels = grtxt)
    }
    if (valgtVar == 'AAisFAis') {
      grtxt <- grtxt[1:5]
      RegData <- RegData[which(RegData$AAis %in% 1:5 &
                                 RegData$FAis %in% 1:5), ]
      RegData$VariabelGr <- factor(RegData$AAis, levels = 1:5, labels = grtxt)
      RegData$VariabelGrPost <- factor(RegData$FAis, levels = 1:5, labels = LETTERS[1:5])
      xAkseTxt <- 'AIS ved innleggelse'
      grPP <- c('Innleggelse', 'Utskriving')
    }
    if (valgtVar == 'KontFAis') {
       grtxt <- grtxt[1:5]
        RegData <- RegData[which(RegData$FAis %in% 1:5 &
                                   RegData$CAis %in% 1:5 &
                                   RegData$CNum ==1), ]
        RegData$VariabelGr <- factor(RegData$FAis, levels = 1:5, labels = grtxt)
        RegData$VariabelGrPost <- factor(RegData$CAis, levels = 1:5, labels = LETTERS[1:5])
        xAkseTxt <- 'AIS ved utskriving'
        grPP <- c('Utskriving', '1.kontroll')
      }
    }

  if (valgtVar %in% c('ABMI', 'FBMI')) { #fordeling, andelgrvar
    RegData <- RegData[RegData$AdmitDt >= '2025-01-01', ]
    tittel <- paste0(ifelse(figurtype == 'andeler', 'BMI', 'Målt høyde og vekt'),
                     ' ved ',
                     ifelse(valgtVar == 'ABMI', 'innleggelse', 'utskriving'))
    varTxt <- 'registrert med BMI'
    ind <- which(!is.na(RegData[,valgtVar]))
    RegData$Variabel[ind] <- 1
  #  RegData <- RegData[which(RegData$BMI >10), ]
    if (figurtype == 'andeler') {
      RegData <- RegData[which(RegData$Alder >= 18), ]
      gr <- c(0, 18.5, 25, 30, 35, 40, 1000)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c('<18,5', levels(RegData$VariabelGr)[2:(length(gr)-2)],'40+')
      grtxt2 <- c('Undervekt', 'Normalvekt', 'Overvekt', 'Fedme', 'Fedme kl II', 'Fedme kl III')
      }
  }

  if (valgtVar == 'BMI') { #Fordeling, AndelGrVar
    #BMI > 30
    RegData <- RegData[which(RegData$BMI >10), ]
    gr <- c(0, 18.5, 25, 30, 35, 40, 1000)
    RegData$VariabelGr <- cut(RegData$BMI, breaks=gr, include.lowest=TRUE, right=FALSE)
    tittel <- 'Pasientenes BMI (Body Mass Index)'
    grtxt <- c('<18,5', levels(RegData$VariabelGr)[2:(length(gr)-2)],'40+')
    grtxt2 <- c('Undervekt', 'Normalvekt', 'Overvekt', 'Fedme', 'Fedme kl II', 'Fedme kl III')
    xAkseTxt <- '"Body Mass Index"'
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData$Variabel[which(RegData$BMI > 30)] <- 1
      tittel <- 'Pasienter med fedme: (BMI>30)'
      varTxt <- 'BMI>30'
      sortAvtagende <- FALSE
    }}


  if (valgtVar == 'AnbefTidKtr') {
    tittel <- 'Anbefalt tid til kontroll'
    retn <- 'H'
    gr <- 1:8 #8:1
    #1:5  Innen 1:5 år, 6 = Ikke aktuelt, 7 = Ikke avtalt kontroll, -1 = Velg verdi
    #grtxt <- c(paste0('Innen ', 1:5, ' år'),'Ikke aktuelt', 'Ikke avtalt kontroll')
    grtxt <- c(paste0(c('Innen: 1', 2:5), ' år'),'Ikke aktuelt', 'Ikke avtalt', 'Ikke relevant')
    RegData <- RegData[RegData$RecCtrl %in% gr,]
    RegData$VariabelGr <- factor(as.numeric(RegData$RecCtrl), levels=gr, labels = grtxt)
  }

  if (valgtVar %in% c('DagerRehab', 'DagerTilRehab', 'OpphTot')) { #andeler, gjsn
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
                     DagerRehab=ifelse(figurtype == 'gjsn',
                                       'antall dager med rehabilitering',
                                       'Tid innlagt på ryggmargsskadeavdeling'),
                     DagerTilRehab = ifelse(figurtype == 'gjsn',
                                            'antall dager før rehabilitering',
                                            'Tid fra akuttinnleggelse til innl. på ryggmargsskadeavd.'),
                     OpphTot = ifelse(figurtype == 'gjsn',
                                      'totalt opphold',
                                      'Total tid innlagt på sykehus')
    )
    cexgr <- 0.9
    txtretn <- 2
    xAkseTxt <- 'Antall dager'
  }	#Variable med antall dager


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

  if (valgtVar == 'KomplPrim'){
  tittel <- 'Komplikasjoner ved primærrehab.'
  RegData <- RegData[which(RegData$Aar >= 2023), ]
  flerevar <- 1
  retn <- 'H'
  variable <- c(
    'PressureUlcer', 'VTE', 'UTI', 'Sepsis', 'Pneumonia',
    'Spasticity', 'Syringomyelia',
    'HeterotopicOssification', 'AutonomicDysreflexia', 'OrthostaticHypotension',
    'Osteoporosis', 'ComplicOther', 'ComplicNone')
  grtxt <- c('Trykksår', 'Tromboembolisme', 'Beh.krevende UVI (≥3)', 'Sepsis', 'Pneumoni',
    'Invalidiserende spastisitet', 'Symptomgivende syringomyeli',
    'Heterotope ossifikasjoner', 'Autonom dysrefleksi', 'Ortostatisk hypotensjon',
    'Osteoporose', 'Andre komplikasjoner', 'Ingen av disse kompl.')
  ind1 <- which(RegData[,variable]==1, arr.ind=T)  #& indDato==TRUE
  RegData[ ,variable] <- 0
   RegData[ ,variable][ind1] <- 1
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

  if (valgtVar %in% c('UtTil', 'KontUtTil')) {
    tittel <- 'Utskrevet til'
    #gr <- (1:10,99) - Kodene som registereres
    grtxt <- c('Hjem', 'Sykehus', 'Sykehjem', 'Omsorgsbolig', 'Bofellesskap',
               'Kriminalomsorg', 'Hotell', 'Bostedsløs', 'Avdød', 'Annet', 'Planlagt hjem', 'Ukjent')
    xAkseTxt <- 'Utskrevet til'

    RegData$PlaceDis[which(RegData$PlaceDis==99)] <- 12
    RegData <- RegData[RegData$PlaceDis %in% 1:12, ]
    RegData$VariabelGr <- factor(as.numeric(RegData$PlaceDis), levels=1:12, labels = grtxt)
    if (valgtVar=='KontUtTil'){
      RegData <- RegData[RegData$CNum ==1, ]
      RegData$CPlaceDis[which(RegData$CPlaceDis==99)] <- 12
      RegData <- RegData[RegData$CPlaceDis %in% 1:12, ]
      RegData$VariabelGrPost <- factor(as.numeric(RegData$CPlaceDis), levels=1:12, labels = grtxt)
      grPP <- c('Utskriving', '1.kontroll')
    }
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

  if (valgtVar == 'RegForsinkelse') {  #Andeler, gjsn
    #Verdier: 0-3402
    RegData$Diff <- as.numeric(as.Date(RegData$FirstTimeClosed) - RegData$DischgDt)
    # RegData$Diff <- as.numeric(as.Date(as.POSIXct(RegData$FirstTimeClosed, format="%Y-%m-%d")) -
    #                              as.Date(as.POSIXct(RegData$DischgDt, format="%Y-%m-%d")))
    RegData <- RegData[which(RegData$Diff > -1), ]
    tittel <- switch(figurtype,
                     andeler='Tid fra utskriving til ferdigstilt registrering',
                     andelGrVar = 'Mer enn 30 dager fra utskriving til ferdig registrering',
                     gjsn = 'registreringsforsinkelse') #
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

  if (valgtVar == 'SpnlSurg2') {
    # -1 = Velg verdi, 0 Nei, 1 Ja, 8 Ikke relevant (ikke-traumatisk skade)[FEIL!], 9 Ukjent
    tittel <- 'Operasjon på ryggsøylen'
    RegData <- RegData[which(RegData$SpnlSurg2 %in% c(0,1,9)) %i%
                         which(RegData$InnDato >= as.Date('2024-01-01')), ]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(as.numeric(RegData$SpnlSurg2), levels=c(0,1,9), labels = grtxt)
    retn <- 'H'
  }

  if (valgtVar == 'VentAssi2') {
    # -1 = Velg verdi, 0 = Nei, 1 = Ja, < 24 timer/dag v/utskr, 2 = Ja, 24 timer/dag v utskrivning
    # 3 = Ja, ukjent antall timer/dag ved utskrivning, 4 = Ventilasjonsstøtte kun ved pusteforstyrrelser under søvn
    # 9 = Ukjent

    tittel <- 'Ventilasjonsstøtte ved utskriving (f.o.m. 2025)'
    RegData <- RegData[which(RegData$VentAssi2 %in% c(1:4,0,9)) %i%
                         which(RegData$InnDato >= as.Date('2025-01-01')), ]
    grtxt <- c('Ja, < 24 t/døgn', 'Ja, hele døgnet', 'Ja, ukjent ant. t/døgn',
               'Ja, v/pusteforstyrr. søvn', 'Nei', 'Ukjent')
    RegData$VariabelGr <- factor(as.numeric(RegData$VentAssi2), levels=c(1:4,0,9), labels = grtxt)
    retn <- 'H'
  }


  #----------------Livskvalitet-skjema (start 01.01.2015)---------------
  #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
  #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
  # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
  # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
  # som 0.
  #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
  #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen

  if (substr(valgtVar,1,4)=='Livs') {
    indDato <- which(RegData$InnDato >= as.Date('2015-01-01')) #Ikke filtrer på startdato
    indTidspkt <- which(as.Date(RegData$QolDt) <= RegData$DischgDt)
    indAlder <- which(RegData$Alder >= 16)
    RegData <- RegData[indTidspkt %i% indAlder, ]
    xAkseTxt <- 'Skåring: 0-10. Høyest er best.'
  }

  if (valgtVar == 'LivsGen') {
    tittel <- ifelse(figurtype == 'andeler',
                     'Tilfredshet med livet',
                     'tilfredshet med livet')
    RegData <- RegData[RegData$SatGenrl %in% 0:10, ]
    grtxt <- 0:10
    RegData$VariabelGr <- factor(as.numeric(RegData$SatGenrl), levels=0:10, labels = grtxt)
    RegData$Variabel <- as.numeric(RegData$SatGenrl)
    sortAvtagende <- TRUE
  }
  if (valgtVar == 'LivsFys') {
    tittel <- ifelse(figurtype == 'andeler',
                     'Tilfredshet med fysisk helse',
                     'tilfredshet med fysisk helse')
    RegData <- RegData[RegData$SatPhys %in% 0:10, ]
    grtxt <- 0:10
    RegData$VariabelGr <- factor(as.numeric(RegData$SatPhys), levels=0:10, labels = grtxt)
    RegData$Variabel <- as.numeric(RegData$SatPhys)
    sortAvtagende <- TRUE
  }

  if (valgtVar == 'LivsPsyk') {
    tittel <- ifelse(figurtype == 'andeler',
                     'Tilfredshet med psykisk helse',
                     'tilfredshet med psykisk helse')
    RegData <- RegData[RegData$SatPsych %in% 0:10, ]
    grtxt <- 0:10
    RegData$VariabelGr <- factor(as.numeric(RegData$SatPsych), levels=0:10, labels = grtxt)
    RegData$Variabel <- as.numeric(RegData$SatPsych)
    sortAvtagende <- TRUE
  }
  if (valgtVar == 'LivsSosLiv') {
    tittel <- ifelse(figurtype == 'andeler',
                     'Tilfredshet med sosialt liv',
                     'tilfredshet med sosialt liv')
    RegData <- RegData[RegData$SatSocIf %in% 0:10, ]
    grtxt <- 0:10
    RegData$VariabelGr <- factor(as.numeric(RegData$SatSocIf), levels=0:10, labels = grtxt)
    RegData$Variabel <- as.numeric(RegData$SatSocIf)
    sortAvtagende <- TRUE
  }


  #----------------URIN-skjema (start 01.01.2015)--------------
  if (substr(valgtVar,1,4)=='Urin') {
    RegData <- RegData[which(RegData$InnDato >= as.Date('2015-01-01') &
                               as.Date(RegData$LutfxnDt) <= RegData$DischgDt), ]}

  if (valgtVar=='UrinInkontinensTom2018') {
    #0:4,9: Nei, Ja daglig, Ja ukentlig, Ja månedlig, Ikke relevant, Ukjent
    tittel <- 'Ufrivillig urinlekkasje'
    gr <- c(0:4,9)
    RegData <- RegData[RegData$Incontnc %in% gr,]
    grtxt <- c('Nei', 'Ja, daglig', 'Ja, ukentlig', 'Ja, månedlig', 'Ikke relevant', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Incontnc, levels = gr, labels = grtxt)
  }
  if (valgtVar=='UrinInkontinens') {
    # -1 = Velg verdi
    # 1 = 1 Daglig
    # 2 = 2 En eller flere ganger per uke (men ikke daglig)
    # 3 = 3 Sjeldnere enn en gang per uke
    # 7 = 7 Aldri
    # 8 = 8 Ikke relevant
    # 9 = 9 Ukjent
    tittel <- 'Ufrivillig urinlekkasje'
    gr <- c(1:3,7:9)
    RegData <- RegData[RegData$Incontnc2 %in% gr,]
    grtxt <- c('Daglig', 'Ukentlig', 'Sjeldnere enn ukentlig', 'Aldri', 'Ikke relevant', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Incontnc2, levels = gr, labels = grtxt)
  }

  if (valgtVar=='UrinKirInngr') {
    flerevar <- 1
    retn <- 'H'
    tittel <- 'Kirurgiske inngrep i urinveiene'
    RegData <- RegData[which((RegData$Surgicalpr==1) & (RegData$InnDato >= as.Date('2015-01-01'))), ]
    #For Surgicalpr=='ja' Ta med egen kolonne for nei? Nei, for mange
    variable <- c('Spcath', 'Bstnrm', 'Ustnrm', 'Bladag', 'Ustent', 'Botox', 'Artsph','Ilvscs',
                  'Ilurts', 'Ccathv',  'Othsrg') #'Sarstm',
    grtxt <- c('Innsatt suprapubiskateter', 'Fjernet blærestein', 'Fjernet andre stein', 'Blæreforstørrelse',
               'Sfinkterotomi', 'Botulinumtoksininjeksjon', 'Kunstig sfinkter', 'Ilovesikostomi', 'Ileoureterostomi',
               'Kateteriserbar urostomi', #'Sakralnervestimulator', - Fjernet fra 01.01.2020
               'Annet')
    # indDato <- cbind(Spcath = as.Date(RegData$SpcathDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Bstnrm = as.Date(RegData$BstnrmDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Ustnrm = as.Date(RegData$UstnrmDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Bladag = as.Date(RegData$BladagDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Ustent = as.Date(RegData$UstentDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Botox = as.Date(RegData$BotoxDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Artsph = as.Date(RegData$ArtsphDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Ilvscs = as.Date(RegData$IlvscsDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Ilurts = as.Date(RegData$IlurtsDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Ccathv = as.Date(RegData$CcathvDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  #Sarstm = as.Date(RegData$SarstmDt, format="%Y-%m-%d") >= RegData$InnDato,
    #                  Othsrg = as.Date(RegData$OthsrgDt, format="%Y-%m-%d") >= RegData$InnDato)
    ind1 <- which(RegData[,variable]==TRUE, arr.ind=T)  #& indDato==TRUE
    RegData[ ,variable] <- 0
    RegData[ ,variable][ind1] <- 1
  }
  if (valgtVar=='UrinLegemidlerTom2018') {
    RegData <- RegData[which(RegData$InnDato >= as.Date('2015-01-01')), ]
    #0:1,9: Nei, Ja, Ukjent
    tittel <- 'Bruk av legemidler som påvirker urinveiene (siste år)'
    gr <- c(0:1,9)
    RegData <- RegData[RegData$AnyDrugs %in% gr,]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$AnyDrugs, levels = gr, labels = grtxt)
  }
  if (valgtVar=='UrinLegemidler') {
    RegData <- RegData[which(RegData$InnDato >= as.Date('2019-01-01')), ]
    #0:1,9: Nei, Ja, Ukjent
    tittel <- 'Bruk av legemidler som påvirker urinveiene (siste 4 uker) '
    gr <- c(0:1,9)
    RegData <- RegData[RegData$AnyDrugs2 %in% gr,]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$AnyDrugs2, levels = gr, labels = grtxt)
  }
  if (valgtVar=='UrinLegemidlerHvilke') {
    flerevar <- 1
    retn <- 'H'
    tittel <- 'Legemidler som påvirker urinveiene'
    #AnyDrugs: "siste året" (t.o.m. 2018), AnyDrugs2 "siste 4 uker"
    #RegData <- RegData[which((RegData$AnyDrugs %in% 0:1) & (RegData$InnDato >= as.Date('2015-01-01'))), ]
    RegData <- RegData[which((RegData$AnyDrugs2 == 1) & (RegData$InnDato >= as.Date('2019-01-01'))), ]
    variable <- c('Bladrelx', 'Spncrelx', 'DrugsAnti', 'Antiuti', 'Antiprop', 'Othdrg')
    grtxt <- c('Blæreavslappende legemidler', 'Avslappende, sfinkter/blærehals', 'Antibiotika/antiseptika',
               '...behandling av UVI', '...forebyggende', 'Annet')
    cexgr <- 1
    Dum <- RegData
    ind1 <- which(RegData[,variable]==1 , arr.ind=T) #==TRUE
    indIkkeAnti <- which(Dum$DrugsAnti==0) #==FALSE De som ikke fått antibiotika. Kan ikke være med i variabelens undergruppe.
    RegData[ ,variable] <- 0
    RegData[indIkkeAnti ,c('Antiuti', 'Antiprop')] <- NA #Disse ser ut til å være tomme fra før
    RegData[ ,variable][ind1] <- 1
  }
  #RegData[,variable]

  if (valgtVar %in% c('UrinTomBlareHoved','UrinTomBlareTillegg')) {
    flerevar <- 1
    retn <- 'H'
    tittel <- switch(valgtVar,
                     UrinTomBlareHoved = 'Blæretømming, hovedmetode',
                     UrinTomBlareTillegg = 'Blæretømming, tilleggsmetode')
    RegData <- RegData[RegData$InnDato >= as.Date('2015-01-01'), ]
    variable <- switch(valgtVar,
                       UrinTomBlareHoved =  c('EmbladUn', 'EmbladM1', 'EmbladM2', 'EmbladM3', 'EmbladM4', 'EmbladM5', 'EmbladM6',
                                              'EmbladM7', 'EmbladM8', 'EmbladM9', 'EmbladM11', 'EmbladM12'),
                       UrinTomBlareTillegg =c('EmbladS1', 'EmbladS2', 'EmbladS3', 'EmbladS4', 'EmbladS5', 'EmbladS6',
                                              'EmbladS7', 'EmbladS8', 'EmbladS9', 'EmbladS11', 'EmbladS12'))
    grtxt <- c('Ukjent', 'Normal vannlating', 'Trigge tømmerefleks, viljestyrt',
               'Trigge tømmerefleks, ufrivillig', 'Blæretømming pressing', 'Tømming ekstern kompresjon',
               'Selvkateterisering', 'Kateterisering m/hjelp', 'Transuretralt', 'Suprapubisk',
               'Stomi', 'Annen metode')
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
                               as.Date(RegData$BfxnbaDt) <= RegData$DischgDt), ]}


  if (valgtVar=='TarmAvfmiddel') {
    tittel <- 'Bruk av perorale avføringsmidler'
    gr <- c(0:1,9)
    RegData <- RegData[RegData$OralLaxatives %in% gr,]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$OralLaxatives, levels = gr, labels = grtxt)
  }
  if (valgtVar=='TarmAvfmiddelHvilke') {
    RegData <- RegData[which(RegData$OralLaxatives==1), ] #Andel av de som har fått avføringsmidler
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
  if (valgtVar=='TarmInkontinensTom2018') { #Utgår
    retn <- 'H'
    tittel <- 'Hyppighet av fekal inkontinens'
    gr <- c(1:7,9)
    RegData <- RegData[RegData$Fcincfrq %in% gr,]
    grtxt <- c('Mer enn daglig', 'Daglig', 'Ukentlig', 'Mer enn månedlig',
               'Månedlig', 'Mindre enn månedlig', 'Aldri', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Fcincfrq, levels = gr, labels = grtxt)
  }
  if (valgtVar=='TarmInkontinensFra2019') {
    retn <- 'H'
    tittel <- 'Hyppighet av fekal inkontinens'

    RegData$Fcincfrq2[RegData$Fcincfrq2 %in% c(4,7)] <- 4
    RegData$Fcincfrq3[RegData$Fcincfrq2 != -1] <- RegData$Fcincfrq2[RegData$Fcincfrq2 != -1]
    gr <- c(1:4,8:9)
    RegData <- RegData[RegData$Fcincfrq3 %in% gr,]
    grtxt <- c('Daglig', '1-6 ganger/uke', '1-4 ganger/måned',
               'Sjeldnere enn månedlig \n eller aldri', 'Ikke relevant', 'Ukjent' )
    RegData$VariabelGr <- factor(RegData$Fcincfrq3, levels = gr, labels = grtxt)
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
    variable <- c('Apndec', 'Chcyec', 'Colost', 'Ileost',
                  'Hemec', 'Apndic', 'Otgisurg') #
    grtxt <- c('Appendektomi','Fjerning av galleblæren', 'Kolostomi', 'Ileostomi',
               'Hemoroidektomi (fra 1/1-19)', 'Appendikostomi (fra 1/1-19)', 'Annet') #
    cexgr <- 1
    ind1 <- which(RegData[,variable]==1 , arr.ind=T) #which(RegData[,variable]==TRUE , arr.ind=T)
    RegData[ ,variable] <- 0
    RegData[ ,variable][ind1] <- 1
  }

  if (valgtVar=='TarmNBD') {
    # Verdi fra 0 - 47 er oppnåelig. Benyttes til å vurdere graden av nevrogen tarmdysfunksjon
    #0-6: svært liten, 7-9: liten, 10-13: moderat, >=14: alvorlig, -1:Ikke beregnet
    tittel <- 'NBD-skår'
    RegData <- RegData[RegData$NBD %in% 0:47,]
    gr <- c(0,7,10,14,48)	#c(seq(0, 90, 15), 120)
    RegData$VariabelGr <- cut(as.numeric(RegData$NBD), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('svært liten (0-6)', 'liten (7-9)', 'moderat (10-13)', 'alvorlig (\u2265 14)')
    #RegData$VariabelGr <- factor(RegData$NBD, levels = gr, labels = grtxt)
  }

  if (valgtVar %in% c('TarmAvfHoved','TarmAvfTillegg')) {
    flerevar <- 1
    retn <- 'H'
    tittel <- switch(valgtVar,
                     TarmAvfHoved = 'Avføring, hovedmetode',
                     TarmAvfTillegg = 'Avføring, tilleggsmetode')
    RegData <- RegData[RegData$InnDato >= as.Date('2016-01-01'), ]
    variable <- switch(valgtVar,
                       TarmAvfHoved =  c('DefcmthUn', 'DefcmthM1', 'DefcmthM2', 'DefcmthM3', 'DefcmthM4',
                                         'DefcmthM5', 'DefcmthM6', 'DefcmthM7', 'DefcmthM8', 'DefcmthM9',
                                         'DefcmthM10', 'DefcmthNa'),
                       TarmAvfTillegg =c('OthdefS1', 'OthdefS2', 'OthdefS3', 'OthdefS4', 'OthdefS5',
                                         'OthdefS6', 'OthdefS7', 'OthdefS9', 'OthdefS10'))
    grtxt <- switch(valgtVar,
                    TarmAvfHoved = c('Ukjent', 'Normal avføring', 'Pressing/trykking', 'Digital stimulering',
                                     'Stikkpiller', 'Manuell fjerning', 'Miniklyster', 'Klyster (>150ml)',
                                     'Kolostomi', 'Sakralstimulering', 'Annen', 'Ikke relevant'),
                    TarmAvfTillegg = c('Ukjent', 'Normal avføring', 'Pressing/trykking', 'Digital stimulering',
                                       'Stikkpiller', 'Manuell fjerning', 'Miniklyster', 'Klyster (>150ml)',
                                       'Sakralstimulering', 'Annen')
    )
    if (valgtVar == 'TarmAvfTillegg'){grtxt <- grtxt[-1]}
    #Ikke nødvendig å gjøre om til 01 når har boolske variable
    ind1 <- which(RegData[,variable]==TRUE, arr.ind=T)
    RegData[ ,variable] <- 0
    RegData[ ,variable][ind1] <- 1
  }

  #----------- EQ5D-5L------------------------------

  if (valgtVar == 'Eq5dQ5AnxietyDepression') {#fordeling
    tittel <- 'Angst/depresjon'
    grtxt <- c('Ingen', 'Litt','Middels', 'Svært','Ekstremt')
    RegData$VariabelGr <- factor(RegData$Eq5dQ5AnxietyDepression, levels = 1:5)
    subtxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
  }
  if (valgtVar == 'Eq5dQ1Mobility') { #fordeling
    tittel <- 'Problemer med gangfunksjon'
    grtxt <- c('Ingen', 'Litt','Middels', 'Store', 'Ute av stand')
    RegData$VariabelGr <- factor(RegData$Eq5dQ1Mobility, levels = 1:5)
    subtxt <- 'problemer med gange'
  }
  if (valgtVar == 'Eq5dQ4PainDiscomfort') { #fordeling
    tittel <- 'Smerter/ubehag'
    grtxt <- c('Ingen', 'Litt','Middels', 'Sterke', 'Svært mye')
    RegData$VariabelGr <- factor(RegData$Eq5dQ4PainDiscomfort, levels = 1:5)
    subtxt <- 'problemer med gange'
  }
  if (valgtVar == 'Eq5dQ2Selfcare') {#fordeling
    tittel <- 'Problemer med personlig stell'
    grtxt <- c('Ingen', 'Litt','Middels', 'Store', 'Ute av stand')
    RegData$VariabelGr <- factor(RegData$Eq5dQ2Selfcare, levels = 1:5)
    subtxt <- 'Grad av problemer'
  }
  if (valgtVar == 'Eq5dQ3UsualActivities') {#fordeling
    tittel <- 'Problemer med vanlige gjøremål'
    grtxt <- c('Ingen', 'Litt','Middels', 'Store', 'Ute av stand')
    RegData$VariabelGr <- factor(RegData$Eq5dQ3UsualActivities, levels = 1:5)
    subtxt <- 'Grad av problemer'
  }

  if (valgtVar == 'Eq5dQ6HealthToday') {
    tittel <- 'Egenvurdert helse'
    gr <- seq(0,100,10) #c(seq(0,90,10), 101)
    RegData$VariabelGr <- cut(as.numeric(RegData$Eq5dQ6HealthToday), breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- levels(RegData$VariabelGr)
  }

  #---------------- ActivityAndParticipationPerformance  (start 01.01.2017)------------------
  if (substr(valgtVar,1,4)=='Funk') {
    RegData <- RegData[which(RegData$InnDato >= as.Date('2017-01-01') &
                               as.Date(RegData$DataClDt) <= RegData$DischgDt), ]}
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



  #---------------- ActivityAndParticipationPerformance  (start 01.01.2017)------------
  if (substr(valgtVar,1,4)=='Tilf') {
    RegData <- RegData[which((RegData$InnDato >= as.Date('2017-01-01')) &
                               as.Date(RegData$DataClDtS) <= RegData$DischgDt), ]
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

#---------Kontrollskjema-----------------

  if (valgtVar=='KontControlInterruptedReason') {
    tittel <- 'Årsak til ikke gjennomført kontroll ved utreise'
    retn <- 'H'
    gr <- c(1:9)
    RegData <- RegData[RegData$ControlInterruptedReason %in% gr,]
    grtxt <- c('Ikke møtt',
               'Pasient utsatt kontr.',
               'Ikke kontakt med pasient',
               'Helsemessig årsak',
               'Avd. ikke kapasitet',
               'Kontroll v/annet sykehus',
               'Ønsker ikke kontroll',
               'Dødsfall',
               'Annen årsak')
    RegData$VariabelGr <- factor(RegData$ControlInterruptedReason, levels = gr, labels = grtxt)
  }


  if (valgtVar == 'KontrKompl'){
    tittel <- 'Komplikasjoner, kontroll ?'
    RegData <- RegData[which(RegData$ControlStatus==0 & RegData$Creation >= 2022), ]
    RegData$Alder <- RegData$PatientAgeKont #For å få filtrering på alder ved kontroll, ikke innleggelse
    flerevar <- 1
    retn <- 'H'
    variable <- c(
      'CPressureUlcer', 'CVTE', 'CUTI', 'CSepsis', 'CPneumonia',
      'CSpasticity', 'CSyringomyelia',
      'CHeterotopicOssification', 'CAutonomicDysreflexia', 'COrthostaticHypotension',
      'COsteoporosis', 'CComplicOther', 'CComplicNone')
    grtxt <- c('Trykksår', 'Tromboembolisme', 'Beh.krevende UVI (≥3)', 'Sepsis', 'Pneumoni',
               'Invalidiserende spastisitet', 'Symptomgivende syringomyeli',
               'Heterotope ossifikasjoner', 'Autonom dysrefleksi', 'Ortostatisk hypotensjon',
               'Osteoporose', 'Andre komplikasjoner', 'Ingen av disse kompl.')
    ind1 <- which(RegData[,variable]==1, arr.ind=T)
    RegData[ ,variable] <- 0
    RegData[ ,variable][ind1] <- 1
  }


  if (valgtVar=='KontUtfHvordan') {
    tittel <- 'Kontrollen er utført på følgende måte'
    RegData$Alder <- RegData$PatientAgeKont #For å få filtrering på alder ved kontroll, ikke innleggelse
    retn <- 'H'
    gr <- rev(1:4)
    RegData <- RegData[RegData$ControlPerformed %in% gr,]
    grtxt <- rev(c('Innleggelse', 'Poliklinisk', 'Videokonsultasjon', 'Telefon'))
    RegData$VariabelGr <- factor(RegData$ControlPerformed, levels = gr, labels = grtxt)
  }




  UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, cexgr=cexgr, KImaal=KImaal, retn=retn,
                 tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende,
                 grPP=grPP)
  return(invisible(UtData))

}
