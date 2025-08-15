# Hjelpefunksjoner for NorSCIR
#---------------------------------------------

#' Kjør Shiny Application
#' @param register Angi hvilket register som skal startes
#' @return Et objekt som representerer den aktuelle app'en
#' @export

kjor_NSapper <- function(register = 'norscir', browser = FALSE, logAsJson = FALSE) {

  if (logAsJson) {
    rapbase::loggerSetup()
  }
  app <- switch(register,
                'norscir' = shiny::shinyApp(
                  ui = nordicscir::ui_norscir,
                  server = nordicscir::server_norscir,
                  # ui = norscir::ui_norscir,
                  # server = norscir::server_norscir,
                  options = list(launch.browser = browser)
                ),
                'nordicscir' = shiny::shinyApp(
                  ui = nordicscir::ui_nordicscir,
                  server = nordicscir::server_nordicscir,
                  options = list(launch.browser = browser)
                )
  )

  if (!(register %in% c('norscir','nordicscir'))){
    warning('Angitt register har ingen app')}

  return(app)
}


#' Tilrettelegge tidsenhetvariabel:
#' @param RegData dataramme
#' @export
SorterOgNavngiTidsEnhet <- function(RegData, tidsenhet='Aar', tab=0, datoUt=0) {

  RegData$RapDato <- as.Date(RegData[ ,c('InnDato', 'DischgDt')[datoUt+1]])

  if (datoUt == 1) {
    RegData$DischgDt <- strptime(RegData$DischgDt, format="%Y-%m-%d")
  RegData$MndNum <- RegData$DischgDt$mon +1
  #head(format(RegData$DischgDt, '%b'))
  RegData$MndAar <- format(RegData$DischgDt, '%b%y')
  RegData$Kvartal <- ceiling(RegData$MndNum/3)
  RegData$Halvaar <- ceiling(RegData$MndNum/6)
  RegData$Aar <- lubridate::year(RegData$DischgDt) #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
  }


  #Lager sorteringsvariabel for tidsenhet:
  RegData$TidsEnhetSort <- switch(tidsenhet,
                                  Aar = RegData$Aar-min(RegData$Aar)+1,
                                  Mnd = RegData$MndNum-min(RegData$MndNum[RegData$Aar==min(RegData$Aar)])+1
                                  +(RegData$Aar-min(RegData$Aar))*12,
                                  Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$Aar==min(RegData$Aar)])+1+
                                    (RegData$Aar-min(RegData$Aar))*4,
                                  Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$Aar==min(RegData$Aar)])+1+
                                    (RegData$Aar-min(RegData$Aar))*2
  )

      tidtxt <- switch(tidsenhet,
                       Mnd = format.Date(seq(from=lubridate::floor_date(as.Date(min(as.Date(RegData$RapDato), na.rm = T)), 'month'),
                                             to=max(as.Date(RegData$RapDato), na.rm = T), by='month'), format = '%B%y'), #Hele måneden
                       Kvartal = lubridate::quarter(seq.Date(as.Date(lubridate::floor_date(min(RegData$RapDato), 'month')),
                                                             max(RegData$RapDato),
                                                 by = "quarter"), with_year = T),
                       #NGER: Kvartal = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                       #                sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Halvaar = paste(substr(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)], 3,4),
                                       sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]), sep='-'),
                       Aar = min(RegData$Aar):max(RegData$Aar)
                       #NGER: Aar = as.character(RegData$Aar[match(1:max(RegData$TidsEnhetSort), RegData$TidsEnhetSort)]))
      )

      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
      if (tidsenhet=='Mnd') {tidtxt <- paste0(substr(tidtxt, 1,3), ' '[tab], substrRight(tidtxt, 2))}

      RegData$TidsEnhet <- factor(RegData$TidsEnhetSort, ordered = TRUE,
                                  levels = min(RegData$TidsEnhetSort):max(RegData$TidsEnhetSort),
                                  labels=tidtxt[1:max(RegData$TidsEnhetSort)])
      UtData <- list('RegData'=RegData, 'tidtxt'=tidtxt)
      return(UtData)
}



# Lage tulledata (simulerte data). Takler ikke posixlt- type data.
# @export
# lageTulleData <- function(RegData, varBort='ShNavn', antSh=27) {
#       #FUNKER IKKE !!!
#       library(synthpop)
#       library(dplyr)
#       #ForlopsID <- RegData$ForlopsID
#
#       RegData <- RegData[,-which(names(RegData) %in% varBort)]
#       sykehus <- paste('Sykehus', LETTERS[1:antSh])
#       fordelingPasienter <- sample(1:antSh,antSh)
#       RegData$HealthUnitShortName <- sample(sykehus, prob=fordelingPasienter/sum(fordelingPasienter), size=dim(RegData)[1], replace=T)
#       RegDataSyn <- synthpop::syn(RegData, method = "sample") #, seed = 500) #Trekker med tilbakelegging
#       RegData <- data.frame(RegDataSyn$syn) # FÅR feilmld...
# 	  return(RegData)
#
#       }

#' Automatisk linjebryting av lange tekstetiketter
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#' @export
delTekst <- function(x, len) #x -tekststreng/vektor av tekststrenger, len - Lengden strengen skal brytes ved
{sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
        USE.NAMES = FALSE)
}


#' Koble med aktuell hovedtabell
#' @param HovedSkjema hvilket skjema skal oppfølgingsskjemaet kobles til? De fleste kobles til Hovedskjema.
#' Satisfaction kobles til Performance
#' @param Skjema2 Oppfølgingsskjemaet som skal kobles til sitt aktuelle hovedskjema
#' @param alleHovedskjema TRUE (FALSE) Om alle registreringer fra hovedskjemaet skal være med (TRUE), eller bare
#' de registreringer som har ei oppfølgning (FALSE).
#' @export
KobleMedHoved <- function(HovedSkjema, Skjema2, alleHovedskjema=F, alleSkjema2=F) {
      varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
      Skjema2 <- Skjema2[ , c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
      NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','_S2'),
                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = alleHovedskjema, all.y=alleSkjema2)
      return(NSdata)
}

#' Hente data basert på valgtVar
#' @param valgtVar Angir hvilke(n) variable det skal vises resultat for.
#' @param Data Liste med alle skjema/tabeller
#' @export
finnRegData <- function(valgtVar='Alder', Data = AlleTab){
      valgtSkjema <- substr(valgtVar,1,4)
      if (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Tilf', 'Eq5d', 'Funk', 'Kont', 'Akti')) {
            RegData <- switch(valgtSkjema,
                              'Livs' = Data$LivskvalH,
                              'Tarm' = Data$TarmH,
                              'Urin' = Data$UrinH,
                              'Eq5d' = Data$EQ5DH,
                              'Kont' = Data$KontrollH,
                              'Funk' = Data$AktivFunksjonH,
                              'Tilf' = Data$AktivTilfredshetH #,'Akti' = Data$AktivitetH
                              )} else {
                                    RegData <- Data$HovedSkjema}
      return(RegData)
}

#' Konvertere boolske variable fra tekst til boolske variable...
#' Boolske variabler kommer fra jun 2025 som 0-1. Funksjonen kan ikke brukes lenger
#' @param valgtVar Angir hvilke(n) variable det skal vises resultat for.
#' @param Data Liste med alle skjema/tabeller
#' @export
TilLogiskeVar <- function(Skjema){
      verdiGML <- c('True','False')
      verdiNY <- c(TRUE,FALSE)
      mapping <- data.frame(verdiGML,verdiNY)
      LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
      if (length(LogVar)>0) {
            for (k in 1:length(LogVar)) {
                  Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
            }}
      return(Skjema)
}

#' Gererere månedsrapport for abonnement
#'
#' @param rnwFil navn på Rnw-fila som skal kjøres ('mndRapp.Rnw')
#' @param brukernavn abonnentens brukernavn ved bestilling
#' @param reshID - reshID som abonnenten var logget inn med ved bestilling
#' @param datoFra - startdato for data som hentes til bruk i rapporten
#' @param datoTil - sluttdato for data som hentes til bruk i rapporten
#'
#' @export
abonnement <- function(rnwFil, brukernavn='ikke angitt', reshID=0, register='nordicscir',
                       datoFra=Sys.Date()-400, datoTil=Sys.Date()) {

  #NB: register må angis!
  AlleTab <- nordicscir::getRealData(register = register)
  AlleTab <- nordicscir::processAllData(AlleTab, register = register)
  attach(AlleTab)

  # comment(reshID)
  # reshID <- reshID[[1]]
  # datoFra <- datoFra[[1]]
  # datoTil <- datoTil[[1]]
  # comment(brukernavn)
  # brukernavn <- brukernavn[[1]]

  filbase <- substr(rnwFil[[1]], 1, nchar(rnwFil[[1]])-4)
  tmpFile <- paste0(filbase, Sys.Date(),'_',digest::digest(brukernavn)[[1]], '.Rnw')
  src <- normalizePath(system.file(rnwFil[[1]], package='nordicscir'))

  setwd(tempdir()) # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
  file.copy(src, tmpFile, overwrite = TRUE)

  knitr::knit2pdf(input=tmpFile)

  #gc() #Opprydning gc-"garbage collection"
  utfil <- paste0( getwd(), '/', substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf') #
  return(utfil)
}
