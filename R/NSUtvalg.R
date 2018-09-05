#' Funksjon som gjør utvalg av data basert på brukervalg.
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og
#' utvalgsteksten.
#'
#' @inheritParams NSFigAndeler
#' @export


NSUtvalg <- function(RegData, datoFra='2010-01-01', datoTil='3000-05-25', minald=0, maxald=130,
                     erMann=99, traume='', AIS='', enhetsUtvalg=0, paratetra=99,
                     reshID=0, fargepalett='BlaaOff') {
      
      
     # Definer intersect-operator
      "%i%" <- intersect
      
      #Enhetsutvalg:
      #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
      #trengs ikke data for hele landet:
      reshID <- as.numeric(reshID)
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg == 2) {	
				RegData <- RegData[which(RegData$ReshId == reshID),]	#kun egen enhet
                           }
      

	  Ninn <- dim(RegData)[1]
	   #indSkjemaUt <- which(RegData$SkjemaID != 1)     #NB: Kan senere bli variabelspesifikk!!!
      indAldUt <- which(RegData$Alder < minald | RegData$Alder > maxald)
      indDatoUt <- setdiff(1:Ninn,
                           which(RegData$InnDato > datoFra & RegData$InnDato < datoTil)) #Får bort NA
      traumeValgBort <- switch(traume, ja = c(6,9) , nei = c(1:5,9), alle = NULL) #6 ikke-tr, 1:5 traumer, 9 ukjent
      indTrUt <-  which(RegData$SkadeArsak %in% traumeValgBort)
      indKjUt <- if (erMann %in% 0:1) {which(RegData$erMann != erMann)} else {indKjUt <- NULL}
      AIS <- as.numeric(AIS)
      indAISut <- if (length(which(as.numeric(AIS) %in% 1:5))>0) {
            setdiff(1:Ninn, which(RegData$AAis %in% AIS))} else {NULL}
      indPTbort <- if (paratetra %in% c(0,1,9)) {which(RegData$TetraplegiUt != paratetra)} else {NULL}
      
      indMed <- setdiff(1:Ninn, 
                        unique(c(indAldUt, indDatoUt, indTrUt, indKjUt, indAISut, indPTbort)))
                          
      RegData <- RegData[indMed,]
      
      N <- length(indMed)
      
      utvalgTxt <- c(paste0('Innleggelsesperiode: ',
                           if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                           ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
                     if ((minald>0) | (maxald<130)) {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                                                           ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
                     if (traume %in% c('ja','nei')) {paste0('Traume:', traume)},
                     if (erMann %in% 0:1){paste0('Kjønn: ', c('kvinner', 'menn')[erMann+1])},
                     #if (length(which(AIS %in% c(LETTERS[1:5],'U')))>0) {paste0('AIS, inn: ', paste0(AIS, collapse=','))} 
                        #Får character fra Jasper
                     if (length(which(AIS %in% 1:5))>0) {paste0('AIS, inn: ', paste0(LETTERS[AIS], collapse=','))},
                     if (paratetra %in% c(0,1,9)) {paste0('Nivå ved utreise: ', 
                                                          (c('Paraplegi','Tetraplegi',rep('',7), 'Ukjent'))[paratetra+1])}
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
