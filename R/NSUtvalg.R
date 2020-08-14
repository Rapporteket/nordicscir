#' Funksjon som gjør utvalg av data basert på brukervalg.
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og
#' utvalgsteksten.
#'
#' @param datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#' @param datoTil <- '2013-05-25'
#' @param erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param minald - alder, fra og med
#' @param maxald - alder, til og med
#' @param traume - 'ja','nei', 'alle' standard: ikke valgt
#' @param AIS - AISgrad ved innleggelse alle(''), velge en eller flere fra 1:5, mappes til: A,B,C,D,E
#' @param nivaaUt - Nivå ved utreise, flervalgs tetraplegi, paraplegi, C1-4, C5-8, ukjent
#' @param enhetsUtvalg - 1:eget sykehus sml med resten, 2:eget sykehus, 0:hele landet (standard) 

#' @export


NSUtvalg <- function(RegData, datoFra='2010-01-01', datoTil=Sys.Date(), minald=0, maxald=110,
                     erMann=99, traume='alle', AIS='', enhetsUtvalg=0, nivaaUt=99,
                     reshID=0, fargepalett='BlaaOff', datoUt=0) {
      
      
     # Definer intersect-operator
      "%i%" <- intersect
      if (datoUt==1) {RegData$InnDato <- as.Date(RegData$DischgDt)}
      
      #Enhetsutvalg:
      #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
      #trengs ikke data for hele landet:
      reshID <- as.numeric(reshID)
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg == 2) {	
				RegData <- RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
                           }
      

	Ninn <- dim(RegData)[1]

	indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
      indDato <- which(RegData$InnDato >= datoFra & RegData$InnDato <= datoTil) #Får bort NA
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
      
      utvalgTxt <- c(paste0('Innleggelsesperiode: ',
                           if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                           ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
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
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
            hovedgrTxt <- as.character(RegData$ShNavn[indEgen1]) 
			} else {
                  hovedgrTxt <- 'Hele landet'}
      
      
      ind <- list(Hoved=0, Rest=0)
      smltxt <- ''      
      medSml <- 0
      ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2
      ind$Rest <- NULL
      
      if (enhetsUtvalg ==1 ) {	#Egen mot resten
                ind$Hoved <-which(as.numeric(RegData$ReshId)==reshID)
				medSml <- 1
				smltxt <- 'landet forøvrig'
				ind$Rest <- which(as.numeric(RegData$ReshId) != reshID)
					}								
      
      
      UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, hovedgrTxt=hovedgrTxt, smltxt=smltxt, medSml=medSml, ind=ind, fargepalett=fargepalett) #GronnHNpms624,
      return(invisible(UtData))
}
