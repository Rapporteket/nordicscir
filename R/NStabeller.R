#' Funksjoner for å lage tabeller Group of functions page title
#'
#' Fil som beregner div tabeller.Group of functions Description section
#'
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer
#' Belegg (samlerapport), antall opphold per år og enhet, ant. pasienter per år og enhet, ant opph. per måned og enhet.
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param  Data Liste med alle datatabeller/skjema
#' @param datoFra fra og med dato
#' @param datoTil til og med dato
#' @inheritParams NSFigAndeler
#' @export
tabBelegg <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet,
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      #sep23: NSUtvalg -> NSUtvalgEnh
      RegData <- NSUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil,
                             enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)$RegData
      tabBeleggAnt <- rbind('Antall pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet,
                                                             FUN=function(x) length(unique(x))),
                            'Antall rehab.opphold' = tapply(RegData$SkjemaGUID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,
                            'Tot. ant. rehab. døgn' = round(as.numeric(tapply(RegData$DagerRehab, RegData$TidsEnhet,
                                                                              sum, na.rm=T)),0)
      )

      antTidsenh <- ifelse(tidsenhet=='Aar', 4, 11)

      tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste
      return(tabBeleggAnt)
}

#' Liggetider. Liggetid tot.(OpphTot), Liggetid rehab.(DagerRehab), Liggetid før rehab (DagerTilRehab):
#' min/max, gjsn, median
#' @param RegData data
#' @param Data Liste med alle datatabeller/skjema
#' @param datoFra fra og med dato
#' @param datoTil til og med dato
#' @inheritParams NSUtvalgEnh
#' @export
tabLiggetider <- function(RegData, datoFra='2018-01-01', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0,
                          traume='') {
      #sep23: NSUtvalg -> NSUtvalgEnh
      RegData <- NSUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil = datoTil, traume=traume,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData

      Liggetider <- rbind('Liggetid, totalt' = summary(RegData$OpphTot)[c(1,3,4,6)],
                          'Liggetid på rehab.' =summary(RegData$DagerRehab)[c(1,3,4,6)],
                          'Liggetid før rehab.' = summary(RegData$DagerTilRehab)[c(1,3,4,6)]
      )
      colnames(Liggetider) <- c('Min', 'Median', 'Gj.sn.', 'Maks')

      return(Liggetider)
}

#' tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil.
#' RegData må inneholde Aar.
#' @inheritParams NSUtvalgEnh
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), traume='', antMnd=12){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive
      datoFra <- lubridate::floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month') #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      #sep23: NSUtvalg -> NSUtvalgEnh
      RegData <- NSUtvalgEnh(RegData = RegData, traume=traume)$RegData
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- lubridate::floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(lubridate::ymd(colnames(tabAvdMnd1)), '%b%y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd <- addmargins(tabAvdMnd1)
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned = lubridate::floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd <- xtable::xtable(tabAvdMnd)
	return(tabAvdMnd)
}

#' Antall opphold siste 5 år
#' @inheritParams NSUtvalgEnh
#' @export
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date(), traume=''){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      #sep23: NSUtvalg -> NSUtvalgEnh
      RegData <- NSUtvalgEnh(RegData = RegData, traume=traume)$RegData
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}

#' Antall registreringer/pasienter siste 5 år
#' @param gr gruppering 'opph'-opphold, 'pas'-pasient
#' @export
tabAntOpphPasSh5Aar <- function(RegData, gr='opph', datoTil){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))

      if (gr == 'pas'){
            Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', 'PasientID')]
            tabAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
            tabAvdAarN[is.na(tabAvdAarN)] <- 0
            tabAvdAarN <- addmargins(tabAvdAarN) #, FUN = function(x) sum(x, na.rm=T))

      } else {
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      }
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}

#' Tabell: Antall og andel moder"skjema som har ulike typer registreringsskjema
#' @param moderSkjema Hvilket skjema man skal knytte oppfølgingene til
#' 'Hoved' - hovedskjema, 'Kont' - Kontrollskjema
#' @inheritParams NSUtvalgEnh
#' @export
#'
tabSkjemaTilknyttet <- function(Data=AlleTab, moderSkjema='Hoved',
                                datoUt=0,
                                datoFra='2017-01-01', datoTil=Sys.Date()){
  #Denne skal fungere både for HovedSkjema og kontrollskjema. I AlleTab er
  #attach(Data)
  ModerSkjema <- switch(moderSkjema,
                        'Hoved' = Data$HovedSkjema,
                        'Kont' = Data$KontrollH)

  if (moderSkjema == 'Hoved'){
    ModerSkjema <- NSUtvalgEnh(RegData=ModerSkjema, datoUt=datoUt,
                               datoFra = datoFra, datoTil = datoTil)$RegData

    RaaTab <- data.frame(Sykehus = ModerSkjema$ShNavn,
                         Livskvalitet = ModerSkjema$SkjemaGUIDHoved %in% Data$LivskvalH$HovedskjemaGUID,
                         Urin = ModerSkjema$SkjemaGUIDHoved %in% Data$UrinH$HovedskjemaGUID,
                         Tarm = ModerSkjema$SkjemaGUIDHoved %in% Data$TarmH$HovedskjemaGUID
    )
    if ('AktivFunksjonH' %in% names(Data)) {
      RaaTab <- cbind(RaaTab,
                      Funksjon = ModerSkjema$SkjemaGUIDHoved %in% Data$AktivFunksjonH$HovedskjemaGUID,
                      Tilfredshet = ModerSkjema$SkjemaGUIDHoved %in% Data$AktivTilfredshetH$SkjemaGUID #(SkjemaGUID er fra hovedskjema)
      )
    }
  }

  if (moderSkjema == 'Kont') {
    indDato <- which(as.Date(ModerSkjema$CNeuExmDt) >= datoFra &
                       as.Date(ModerSkjema$CNeuExmDt) <= datoTil)
    ModerSkjema <- ModerSkjema[indDato, ]
    RaaTab <- data.frame(Sykehus = ModerSkjema$ShNavn,
                         Livskvalitet = ModerSkjema$SkjemaGUIDKont %in% Data$LivskvalK$HovedskjemaGUID,
                         Urin = ModerSkjema$SkjemaGUIDKont %in% Data$UrinK$HovedskjemaGUID,
                         Tarm = ModerSkjema$SkjemaGUIDKont %in% Data$TarmK$HovedskjemaGUID,
                          Funksjon = ModerSkjema$SkjemaGUIDKont %in% Data$AktivFunksjonK$HovedskjemaGUID,
                          Tilfredshet = ModerSkjema$SkjemaGUIDKont %in% Data$AktivTilfredshetK$SkjemaGUID
      )
    }

  AntReg <- table(ModerSkjema$ShNavn)
  AntOppf <- cbind(Hoved = AntReg,
                   apply(RaaTab[ ,-1], MARGIN=2,
                         FUN=function(x) tapply(x,INDEX=RaaTab$Sykehus, sum))
  )
  addmargins(AntOppf, margin=1, FUN = list('Totalt' = sum) )
  AndelOppf <- (100*AntOppf / as.vector(AntReg))[,-1]
  if (moderSkjema == 'Kont') { colnames(AntOppf)[1] <- 'Kontroll'}

  tab = list(Antall = AntOppf,
             Andeler = AndelOppf)
  return(tab)
}


#' Tabell: Antall av ulike typer skjema
#' @export
tabAntSkjema <- function(Data=AlleTab, datoFra='2017-01-01', datoTil=Sys.Date()){
      # sum(Data$HovedSkjema$SkjemaGUID %in% Data$LivskvalitetH$HovedskjemaGUID)
      # sum(Data$LivskvalitetH$HovedskjemaGUID %in% Data$HovedSkjema$SkjemaGUID)
      # sum(table(Data$LivskvalitetH$ShNavn))
  # sep23: NSUtvalg -> NSUtvalgEnh
      AntSkjema <- rbind(Hovedskjema = table(NSUtvalgEnh(Data$HovedSkjema, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
            Livskvalitet = table(NSUtvalg(Data$LivskvalitetH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      #Kontroll = table(Data$LivskvalitetH$ShNavn),
                      Urin = table(NSUtvalgEnh(Data$UrinH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Tarm = table(NSUtvalgEnh(Data$TarmH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Funksjon = table(NSUtvalgEnh(Data$FunksjonH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Tilfredshet = table(NSUtvalgEnh(Data$TilfredsH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn)
      )
      AntSkjema <- addmargins(AntSkjema, margin=2, FUN=list('Hele landet' = sum))
      return(AntSkjema)
}


#' Vise figurdata som tabell, fordelingsfigur
#' @param UtDataFraFig Dataliste fra figuren
#' @export
lagTabavFigAndeler <- function(UtDataFraFig){
      tab <-cbind(UtDataFraFig$Ngr$Hoved,
            UtDataFraFig$AggVerdier$Hoved,
            UtDataFraFig$Ngr$Rest,
            UtDataFraFig$AggVerdier$Rest)
rownames(tab) <- UtDataFraFig$grtxt
colnames(tab) <- c(paste0(UtDataFraFig$hovedgrTxt,', N'),
                   paste0(UtDataFraFig$hovedgrTxt, ', Andel (%)'),
                   if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt,', N')},
                   if(!is.null(UtDataFraFig$Ngr$Rest)){paste0(UtDataFraFig$smltxt, ', Andel (%)')})

return(tab)
}

# RegData <- NSRegDataSQL(valgtVar = 'Alder')
# RegData <- NSPreprosesser(RegData = RegData)
# UtDataFraFig <- NSFigAndelerSh(RegData, valgtVar = 'Alder')
# test <- UtDataFraFig$AggVerdier
# t(test)
# UtDataFraFig$Ngr
# UtDataFraFig$grtxt

#' Vise figurdata som tabell, fordeling per sykehus
#' @param UtDataFraFig Dataliste fra figuren
#' @export
lagTabavFigAndelerSh <- function(UtDataFraFig){
  sensur <- is.na(UtDataFraFig$AggVerdier)[,1]
   tab <-rbind(UtDataFraFig$Ngr,
               UtDataFraFig$AggVerdier)
   tab[c(sensur,sensur),] <- NA #'lav N'
   grNavn <- attributes(UtDataFraFig$Ngr)$dimnames[[1]]
   rownames(tab) <- c(grNavn, grNavn)
   colnames(tab) <- UtDataFraFig$grtxt

   return(t(tab))
}
#lagTabavFigAndelerSh(UtDataFordSh)

#' Vise figurdata som tabell, sentralmål per sykshus
#' @param UtDataFraFig Dataliste fra figuren
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
      tab <-cbind(UtDataFraFig$Ngr,
                  UtDataFraFig$AggVerdier$Hoved
      )
      colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
      return(tab)
}


#' Nevrologisk klassifikasjon
#' @param HovedSkjema- hovedskjema
#' @inheritParams NSUtvalgEnh
#' @export
lagTabNevrKlass <- function(HovedSkjema, datoFra='2018-01-01', datoTil=Sys.Date(), datoUt=0){

      Utvalg <- NSUtvalgEnh(HovedSkjema, datoFra = datoFra, datoTil = datoTil, datoUt = datoUt)
      HovedSkjema <- Utvalg$RegData
      HovedSkjema$ShNavn <- as.factor(HovedSkjema$ShNavn)
      Ant <- addmargins(table(HovedSkjema$ShNavn))
      AntKlassInnUt <- addmargins(table(HovedSkjema$ShNavn[(HovedSkjema$AAis %in% c(1:5,9)) & (HovedSkjema$FAis %in% c(1:5,9))]))

NevrKlass <- rbind(
      'Utført og klassifiserbar, inn: ' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$AAis %in% 1:5])),
      'Ikke utført  ved innkomst:' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$ANeuNoMeasure == TRUE & HovedSkjema$AAis == -1])),
      'Utført, ikke klassifiserbar, inn: ' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$AAis==9])),
       'Utført og klassifiserbar, ut: ' =
             addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FAis %in% 1:5])),
      'Ikke utført ved utreise:' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FNeuNoMeasure == TRUE
                                                     & HovedSkjema$FAis == -1])),
      'Utført, ikke klassifiserbar, ut: ' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FAis==9])),
      'Utført ved både inn og ut: ' =
            paste0(sprintf('%.0f',AntKlassInnUt/Ant*100), '%')
)
colnames(NevrKlass)[dim(NevrKlass)[2] ] <- 'Alle enheter'

return(NevrKlass)
}

