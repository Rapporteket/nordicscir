#' Funksjoner for å lage tabeller Group of functions page title
#' 
#' Fil som beregner div tabeller.Group of functions Description section
#' 
#' Detaljer. kommer senereGroup of functions Details paragraph.
#'
#' Fil som inneholder funksjoner for å lage tabeller, i første rekke tellinger av personer 
#' Aktuelle tabeller:
#' -Belegg (samlerapport), antall opphold per år og enhet, ant. pasienter per år og enhet, ant opph. per måned og enhet.
#' -tabAntOpphSh12mnd: Antall opphold per måned og enhet siste 12 måneder fram til datoTil. 
#' RegData må inneholde InnDato.
#' -tabAntOpphSh5Aar:Antall opphold per år og enhet siste 5 år (inkl. inneværende år) fram til datoTil. 
#' RegData må inneholde Aar.
#' 
#' @param RegData data
#' @param personIDvar Variabelen som angir pasientidentifikasjon
#' @param datoTil sluttdato. Brukes i tabellene AntOpph per 12 mnd og Belegg
#' @param  Data Liste med alle datatabeller/skjema
#' @param datoFra fra og med dato
#' @param datoTil til og med dato
# @inheritParams NIRFigAndeler
#' @name NordicScirtabeller
NULL
#' @export

#' @section Belegg (antall opphold, pasienter og rehabdøgn). Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @rdname NordicScirtabeller
#' @export
tabBelegg <- function(RegData, tidsenhet='Mnd', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet, 
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil = datoTil, 
                             enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      #print(paste0('tidsenhet: ', tidsenhet))
      #print(paste0('dim Regdata: ', dim(RegData)[1]))
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)$RegData
      #RegData <- Mtid$RegData
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

#' @section Liggetider. Liggetid tot.(OpphTot), Liggetid rehab.(DagerRehab), Liggetid før rehab (DagerTilRehab): 
#' min/max, gjsn, median
#' @rdname NordicScirtabeller
#' @export
tabLiggetider <- function(RegData, datoFra='2018-01-01', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0,
                          traume='') {

      RegData <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil = datoTil, traume=traume,
                          enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData

      Liggetider <- rbind('Liggetid, totalt' = summary(RegData$OpphTot)[c(1,3,4,6)],
                          'Liggetid på rehab.' =summary(RegData$DagerRehab)[c(1,3,4,6)],	
                          'Liggetid før rehab.' = summary(RegData$DagerTilRehab)[c(1,3,4,6)]	
      )
      colnames(Liggetider) <- c('Min', 'Median', 'Gj.sn.', 'Maks')

      return(Liggetider)
}

#' @section tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#' @rdname NordicScirtabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), traume='', antMnd=12){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive 
      datoFra <- lubridate::floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month') #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      RegData <- NSUtvalg(RegData = RegData, traume=traume)$RegData
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(ymd(colnames(tabAvdMnd1)), '%b%y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd <- addmargins(tabAvdMnd1)
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd <- xtable::xtable(tabAvdMnd)
	return(tabAvdMnd)
}

#' @section Antall opphold siste 5 år
#' @rdname NordicScirtabeller
#' @export
#' 
tabAntOpphSh5Aar <- function(RegData, datoTil=Sys.Date(), traume=''){
      AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
      
      tabAvdAarN <- addmargins(table(RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar')]))
      rownames(tabAvdAarN)[dim(tabAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
      colnames(tabAvdAarN)[dim(tabAvdAarN)[2] ]<- 'Siste 5 år'
      tabAvdAarN <- xtable::xtable(tabAvdAarN)
      return(tabAvdAarN)
}

#' @section Antall registreringer/pasienter siste 5 år:
#' Hmmm
#' @rdname NordicScirtabeller
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

#' #' @section Tabell: Antall og andel hovedskjema som har ulike typer registreringsskjema
#' #' @rdname NordicScirtabeller
#' #' @export
#' #' 
#' tabSkjemaTilknyttetH <- function(Data=AlleTab, datoFra='2017-01-01', datoTil=Sys.Date()){
#'       
#'       Data$HovedSkjema <- NSUtvalg(Data$HovedSkjema, datoFra = datoFra, datoTil = datoTil)$RegData
#'       
#'       RaaTab <- data.frame(Sykehus = Data$HovedSkjema$ShNavn,
#'                      #Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
#'                      Livskvalitet = Data$HovedSkjema$SkjemaGUID %in% Data$LivskvalitetH$HovedskjemaGUID,
#'                      #Kontroll = HovedSkjema$SkjemaGUID %in% Kontroll$HovedskjemaGUID,
#'                      Urin = Data$HovedSkjema$SkjemaGUID %in% Data$UrinH$HovedskjemaGUID,
#'                      Tarm = Data$HovedSkjema$SkjemaGUID %in% Data$TarmH$HovedskjemaGUID,
#'                      Funksjon = Data$HovedSkjema$SkjemaGUID %in% Data$FunksjonH$HovedskjemaGUID,
#'                      Tilfredshet = Data$HovedSkjema$SkjemaGUID %in% 
#'                            Data$FunksjonH$HovedskjemaGUID[Data$FunksjonH$SkjemaGUID %in% Data$TilfredsH$HovedskjemaGUID]
#' )
#' 
#' AntReg <- table(Data$HovedSkjema$ShNavn)
#' AntOppf <- cbind(Hoved = AntReg, 
#'                  apply(RaaTab[ ,-1], MARGIN=2, 
#'                        FUN=function(x) tapply(x,INDEX=RaaTab$Sykehus, sum))
#'                  )
#' addmargins(AntOppf, margin=1, FUN = list('Hele landet' = sum) )
#' AndelOppf <- (100*AntOppf / as.vector(AntReg))[,-1]
#' 
#' tab = list(Antall = AntOppf,
#'            Andeler = AndelOppf)
#' return(tab)
#' }

#' @section Tabell: Antall og andel moder"skjema som har ulike typer registreringsskjema
#' @param moderSkjema Hvilket skjema man skal knytte oppfølgingene til
#' @rdname NordicScirtabeller
#' @export
#' 
tabSkjemaTilknyttet <- function(Data=AlleTab, moderSkjema='Hoved', datoFra='2017-01-01', datoTil=Sys.Date()){
      #Denne skal fungere både for HovedSkjema og kontrollskjema. I AlleTab er 
      ModerSkjema <- switch(moderSkjema,
                            'Hoved' = Data$HovedSkjema,
                            'Ktr' = Data$KontrollH)
      if (moderSkjema == 'Ktr') {ModerSkjema <- ModerSkjema[!is.na(ModerSkjema$CNum), ] }
      ModerSkjema <- NSUtvalg(ModerSkjema, datoFra = datoFra, datoTil = datoTil)$RegData
      
      RaaTab <- data.frame(Sykehus = ModerSkjema$ShNavn,
                           #Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
                           Livskvalitet = ModerSkjema$SkjemaGUID %in% Data$LivskvalitetH$HovedskjemaGUID,
                           #Kontroll = HovedSkjema$SkjemaGUID %in% Kontroll$HovedskjemaGUID,
                           Urin = ModerSkjema$SkjemaGUID %in% Data$UrinH$HovedskjemaGUID,
                           Tarm = ModerSkjema$SkjemaGUID %in% Data$TarmH$HovedskjemaGUID,
                           Funksjon = ModerSkjema$SkjemaGUID %in% Data$FunksjonH$HovedskjemaGUID,
                           Tilfredshet = ModerSkjema$SkjemaGUID %in% 
                                 Data$FunksjonH$HovedskjemaGUID[Data$FunksjonH$SkjemaGUID %in% Data$TilfredsH$HovedskjemaGUID]
      )
      
      AntReg <- table(ModerSkjema$ShNavn)
      AntOppf <- cbind(Hoved = AntReg, 
                       apply(RaaTab[ ,-1], MARGIN=2, 
                             FUN=function(x) tapply(x,INDEX=RaaTab$Sykehus, sum))
      )
      addmargins(AntOppf, margin=1, FUN = list('Hele landet' = sum) )
      AndelOppf <- (100*AntOppf / as.vector(AntReg))[,-1]
      
      tab = list(Antall = AntOppf,
                 Andeler = AndelOppf)
      return(tab)
}


#' @section Tabell: Antall av ulike typer skjema
#' @rdname NordicScirtabeller
#' @export
#' 
tabAntSkjema <- function(Data=AlleTab, datoFra='2017-01-01', datoTil=Sys.Date()){
      # sum(Data$HovedSkjema$SkjemaGUID %in% Data$LivskvalitetH$HovedskjemaGUID)
      # sum(Data$LivskvalitetH$HovedskjemaGUID %in% Data$HovedSkjema$SkjemaGUID)
      # sum(table(Data$LivskvalitetH$ShNavn))
      AntSkjema <- rbind(Hovedskjema = table(NSUtvalg(Data$HovedSkjema, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
            Livskvalitet = table(NSUtvalg(Data$LivskvalitetH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      #Kontroll = table(Data$LivskvalitetH$ShNavn),
                      Urin = table(NSUtvalg(Data$UrinH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Tarm = table(NSUtvalg(Data$TarmH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Funksjon = table(NSUtvalg(Data$FunksjonH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn),
                      Tilfredshet = table(NSUtvalg(Data$TilfredsH, datoFra = datoFra, datoTil = datoTil)$RegData$ShNavn)
      )
      AntSkjema <- addmargins(AntSkjema, margin=2, FUN=list('Hele landet' = sum))
      return(AntSkjema)
}


#' @section Vise figurdata som tabell, fordelingsfigur
#' @rdname NordicScirtabeller
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

#' @section Vise figurdata som tabell, sentralmål per sykshus
#' @rdname NordicScirtabeller
#' @export
lagTabavFigGjsnGrVar <- function(UtDataFraFig){
      tab <-cbind(UtDataFraFig$Ngr, 
                  UtDataFraFig$AggVerdier$Hoved
      )
      colnames(tab) <- c('Antall (N)', UtDataFraFig$SentralmaalTxt)
      return(tab)
}


#' @section Nevrologisk klassifikasjon
#' @rdname NordicScirtabeller
#' @export
lagTabNevrKlass <- function(HovedSkjema, datoFra='2018-01-01', datoTil=Sys.Date()){
      
      Utvalg <- NSUtvalg(HovedSkjema, datoFra = datoFra, datoTil = datoTil)
      HovedSkjema <- Utvalg$RegData
      Ant <- addmargins(table(HovedSkjema$ShNavn))
      AntKlassInnUt <- addmargins(table(HovedSkjema$ShNavn[(HovedSkjema$AAis %in% 1:5) & (HovedSkjema$FAis %in% 1:5)]))
      
NevrKlass <- rbind(
      'Utført og klassifiserbar, innkomst: ' = 
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$AAis %in% 1:5])),
      'Ikke utført  ved innkomst:' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$ANeuNoMeasure == TRUE & HovedSkjema$AAis == -1])),
      'Utført, men ikke klassifiserbar, innkomst: ' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$AAis==9])),
       'Utført og klassifiserbar, utreise: ' =
             addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FAis %in% 1:5])),
      'Ikke utført ved utreise:' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FNeuNoMeasure == TRUE
                                                     & HovedSkjema$FAis == -1])),
      'Utført, men ikke klassifiserbar, utreise: ' =
            addmargins(table(HovedSkjema$ShNavn[HovedSkjema$FAis==9])),
      'Klassifisert ved både inn- og utreise: ' = 
            paste0(sprintf('%.0f',AntKlassInnUt/Ant*100), '%')
)
colnames(NevrKlass)[dim(NevrKlass)[2] ]<- 'Hele landet'

# xtable::xtable(NevrKlass, align=c('l', rep('r', ncol(NevrKlass))), digits=0, 
#                caption=paste0('Nevrologisk klassifikasjon for ferdigstilte innleggelser fra og med ',
#                               datoFra, '.'))
return(NevrKlass)
}

