# Må det kanskje komme en overornet tittel her?
#---------------------------------------------

#' Hjelpefunksjoner. Group of functions page title
#' 
#' Fil med div hjelpefunksjoner.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' @section Finne reinnleggelser After function section:
#' Despite its location, this actually comes after the function section.
#' Fil som inneholder hjelpefunksjoner. 
#' SorterOgNavngiTidsEnhet Legger til tidsenhetene Aar, Halvaar, Mnd og Kvartal
#' 
#' 
#' @param RegData data
#' @param PasientID Variabelen som angir pasientidentifikasjon
# @inheritParams NIRFigAndeler
#' @return Div hjelpefunksjoner
#' @name hjelpeFunksjoner
NULL
#' @rdname hjelpeFunksjoner
#' @export

#' @section Tilrettelegge tidsenhetvariabel:
#' Probably better if all sections come first, uless have one section per function. Makes it easier to
#' see the information flow.
#' @rdname hjelpeFunksjoner
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar') {
      #Lager sorteringsvariabel for tidsenhet:
      RegData$TidsEnhetSort <- switch(tidsenhet,
                                      Aar = RegData$Aar-min(RegData$Aar)+1,
                                      Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                          +(RegData$Aar-min(RegData$Aar))*12, #format(RegData$InnDato, '%b%y'), #
                                      Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*4,
                                      Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                            (RegData$Aar-min(RegData$Aar))*2
      )
      #as.factor(format(RegData$InnDato, '%b%y')) #
      tidtxt <- switch(tidsenhet,
                       #Mnd = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                        #           sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='.'),
                       Mnd = RegData$MndAar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)],
                       Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))
      
      RegData$TidsEnhetSort <- factor(RegData$TidsEnhetSort, levels=1:max(RegData$TidsEnhetSort))
      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE, labels=tidtxt)
      
      #RegData$TidsEnhet <- RegData$TidsEnhetSort
      #levels(RegData$TidsEnhet) <- tidtxt
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}
#' @section Lage tulledata (simulerte data)
#' @rdname hjelpeFunksjoner
#' @export
lageTulleData <- function(RegData, varBort='ShNavn', antSh=27) {
      #FUNKER IKKE !!!
      library(synthpop)
      library(dplyr)
      #ForlopsID <- RegData$ForlopsID
      
      RegData <- RegData[,-which(names(RegData) %in% varBort)]
      sykehus <- paste('Sykehus', LETTERS[1:antSh])
      fordelingPasienter <- sample(1:antSh,antSh)
      RegData$SykehusNavn <- sample(sykehus, prob=fordelingPasienter/sum(fordelingPasienter), size=dim(RegData)[1], replace=T)
      RegDataSyn <- synthpop::syn(RegData, method = "sample", seed = 500) #Trekker med tilbakelegging
      RegData <- data.frame(RegDataSyn$syn) # FÅR feilmld...
	  return(RegData)
}

#' @section Automatisk linjebryting av lange tekstetiketter
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @rdname hjelpeFunksjoner
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}


#' @section Koble med aktuell hovedtabell
#' @param HovedSkjema hvilket skjema skal oppfølgingsskjemaet kobles til? De fleste kobles til Hovedskjema. 
#' Satisfaction kobles til Performance
#' @param Skjema2 Oppfølgingsskjemaet som skal kobles til sitt aktuelle hovedskjema
#' @param alleHovedskjema TRUE (FALSE) Om alle registreringer fra hovedskjemaet skal være med (TRUE), eller bare
#' de registreringer som har ei oppfølgning (FALSE).
#' @rdname hjelpeFunksjoner
#' @export
KobleMedHoved <- function(HovedSkjema, Skjema2, alleHovedskjema=F, alleSkjema2=F) {
      varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
      Skjema2 <- Skjema2[ , c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",   
      NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','_S2'),
                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = alleHovedskjema, all.y=alleSkjema2)
      return(NSdata)
}

#' @section Hente data basert på valgtVar
#' @param valgtVar Angir hvilke(n) variable det skal vises resultat for. 
#' @param Data Liste med alle skjema/tabeller 
#' @rdname hjelpeFunksjoner
#' @export
finnRegData <- function(valgtVar='Alder', Data = AlleTab){
      valgtSkjema <- substr(valgtVar,1,4)
      if (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Tilf', 'Funk', 'Kont', 'Akti')) {
            RegData <- switch(valgtSkjema, 
                              'Livs' = Data$LivskvalitetH,
                              'Tarm' = Data$TarmH,
                              'Urin' = Data$UrinH,
                              'Kont' = Data$KontrollH,
                              'Funk' = Data$FunksjonH,
                              'Tilf' = Data$TilfredsH,
                              'Akti' = Data$AktivitetH)} else {
                                    RegData <- Data$HovedSkjema}
      return(RegData)
}

      