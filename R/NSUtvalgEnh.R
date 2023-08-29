#' Funksjon som gjør utvalg av data basert på brukervalg.
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og
#' utvalgsteksten. Funskjonen genererer også utvalg (indekser) for sammenligning av enhetsgrupper.
#' Tekster tilpasses nordisk register.
#'
#' @param datoFra startdato, fra og med Eks. '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil sluttdato, fra og med Eks. '2013-05-25'
#' @param datoUt datofiltrering basert på innleggelse (0), utskriving (1)
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', 'alle' standard: ikke valgt
#' @param AIS - AISgrad ved innleggelse alle(''), velge en eller flere fra 1:5, mappes til: A,B,C,D,E
#' @param nivaaUt - Nivå ved utreise, flervalgs 0:tetraplegi, 1:paraplegi, 2:C1-4, 3:C5-8, 9:ukjent
#' @param enhetsUtvalg - 0:hele landet/Norden (standard),
#'                       1:eget sykehus sml med resten av landet/Norden,
#'                       2:eget sykehus
#'                       3:eget sykehus mot eget land
#'                       4:eget land
#'                       5:eget land mot andre land
#' @export

NSUtvalgEnh <- function(RegData, datoFra='2010-01-01', datoTil=Sys.Date(), datoUt=0,
                        minald=0, maxald=110, erMann=99, traume='alle', AIS='', enhetsUtvalg=0,
                        nivaaUt=99, reshID=0, fargepalett='BlaaOff') {

  datoUt <- as.numeric(datoUt)
  # Definer intersect-operator
  "%i%" <- intersect
  RegData$RapDato <- if (datoUt==1) {as.Date(RegData$DischgDt)
  } else {as.Date(RegData$InnDato)}

  #Enhetsutvalg:
  #Når bare skal sml med eget land/ikke sammenlikne, trengs ikke alle data:
  reshID <- as.numeric(reshID)
  indEgen1 <- match(reshID, RegData$ReshId)
  #enhetsUtvalg <- ifelse(reshID==0 | is.na(indEgen1), 0, enhetsUtvalg )
  enhetsUtvalg <- ifelse(reshID==0, 0, enhetsUtvalg )
  egetLand <- RegData$Land[indEgen1]
  if (enhetsUtvalg %in% 2:4) {
    RegData <- switch(as.character(enhetsUtvalg),
                      '2' = RegData[which(RegData$ReshId == reshID), ],	#kun egen enhet
                      '3' = RegData[which(RegData$Land == egetLand), ],	#mot eget land
                      '4' = RegData[which(RegData$Land == egetLand), ])	#kun eget land
  }

  Ninn <- dim(RegData)[1]

  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indDato <- which(RegData$RapDato >= datoFra & RegData$RapDato <= datoTil) #Får bort NA
  #traumeValgBort <- switch(traume, ja = c(6,9) , nei = c(1:5,9), alle = 99) #6 ikke-tr, 1:5 traumer, 9 ukjent
  traumeValg <- switch(traume, nei = 6 , ja = 1:5, alle = 99) #6 ikke-tr, 1:5 traumer, 9 ukjent
  indTr <-  if (traume %in% c('ja','nei')) {which(RegData$SkadeArsak %in% traumeValg)} else {1:Ninn}
  indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {1:Ninn}
  indAIS <- if (length(which(as.numeric(AIS) %in% 1:5))>0) {
    which(RegData$FAis %in% AIS)} else {1:Ninn}
  #indPTbort <- if (nivaaUt %in% c(0,1,9)) {which(RegData$TetraplegiUt != nivaaUt)} else {NULL}
  indNivaaUt <- if (nivaaUt %in% c(0:3,9)) {switch(as.character(nivaaUt),
                                                   '0' = which(RegData$TetraplegiUt == nivaaUt),
                                                   '1' = which(RegData$TetraplegiUt == nivaaUt),
                                                   '2' = which(RegData$ASensLvlLC %in% 1:4),
                                                   '3' = which(RegData$ASensLvlLC %in% 5:8),
                                                   '9' = which(RegData$TetraplegiUt == nivaaUt)
  )
  } else {1:Ninn}

  indMed <- indAld %i% indDato %i% indTr %i% indKj %i% indAIS %i% indNivaaUt
  RegData <- RegData[indMed,]
  N <- length(indMed)

  utvalgTxt <- c(paste0(c('Innleggelsesperiode: ', 'Utskrivingsperiode: ')[datoUt+1],
                        if (N>0) {min(RegData$RapDato, na.rm=T)} else {datoFra},
                        ' til ', if (N>0) {max(RegData$RapDato, na.rm=T)} else {datoTil}),
                 if ((minald>0) | (maxald<110)) {paste0('Pasienter fra og med ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                                                        ' til og med ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
                 if (traume %in% c('ja','nei')) {paste0('Traume:', traume)},
                 if (erMann %in% 0:1){paste0('Kjønn: ', c('kvinner', 'menn')[erMann+1])},
                 #if (length(which(AIS %in% c(LETTERS[1:5],'U')))>0) {paste0('AIS, ut: ', paste0(AIS, collapse=','))}
                 #Får character fra Jasper
                 if (length(which(AIS %in% 1:5))>0) {paste0('AIS, ut: ', paste0(LETTERS[AIS], collapse=','))},
                 if (nivaaUt %in% c(0:3,9)) {paste0('Nivå ved utreise: ',
                                                    (c('Paraplegi','Tetraplegi', 'C1-4', 'C5-8',
                                                       rep('',5), 'Ukjent'))[nivaaUt+1])}
  )


  #Enhetsutvalg:
  #Benytter variabelen LandKode til å sjekke om det er nordiske eller norske data
  nordisk <- ifelse(sum(as.numeric(unique(RegData$LandKode)))==1, 0, 1)
  alleTxt <- ifelse(sum(as.numeric(unique(RegData$LandKode)))==1, 'Hele landet', 'Hele Norden')

  indEgen1 <- match(reshID, RegData$ReshId)

  #Enhetsutvalg:
  if (enhetsUtvalg %in% 1:3) {	#Involverer egen enhet
    hovedgrTxt <- as.character(RegData$ShNavn[indEgen1])
  } else {
    hovedgrTxt <- switch(as.character(enhetsUtvalg),
                         '0' = alleTxt,
                         '4' = egetLand,
                         '5' = egetLand)
  }

  ind <- list(Hoved=0, Rest=0)
  smltxt <- ''
  if (enhetsUtvalg %in% c(0,2,4)) {		#Ikke sammenlikning
    medSml <- 0
    N <- dim(RegData)[1]
    ind$Hoved <- if (N>0) {1:N} else {NULL}	#Tidligere redusert datasettet for 2,4
    ind$Rest <- NULL
  } else {						#Skal gjøre sammenlikning 1,3,5
    medSml <- 1
    ind$Hoved <- switch(as.character(enhetsUtvalg),
                        '1' = which(as.numeric(RegData$ReshId)==reshID),
                        '3' = which(as.numeric(RegData$ReshId)==reshID),
                        '5' = which(RegData$Land == egetLand)
    )
    smltxt <- switch(as.character(enhetsUtvalg),
                     '1' = c('landet forøvrig', 'Norden forøvrig')[nordisk+1],
                     '3' = egetLand,
                     '5' = 'andre land')
    ind$Rest <- switch(as.character(enhetsUtvalg),
                       '1' = which(as.numeric(RegData$ReshId) != reshID),
                       '3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
                       '5' = which(RegData$Land != egetLand))
  }


  UtData <- list(RegData=RegData, hovedgrTxt=hovedgrTxt, smltxt=smltxt, utvalgTxt=utvalgTxt,
                 medSml=medSml, ind=ind, fargepalett=fargepalett) #, enhetsUtvalgTest=enhetsUtvalg) #GronnHNpms624,
  return(invisible(UtData))
}
