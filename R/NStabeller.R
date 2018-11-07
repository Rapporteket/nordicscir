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
# @inheritParams NIRFigAndeler
#' @return Div tabeller
#' @name NordicScirtabeller
NULL
#' @export

#' @section Belegg (antall opphold, pasienter og intensivdøgn)
#' @rdname NordicScirtabeller
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @export
tabBelegg <- function(RegData, tidsenhet='Aar', datoTil=Sys.Date(), enhetsUtvalg=0, reshID=0) {
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
#' @section tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#' @rdname NordicScirtabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil=Sys.Date(), antMnd=12){
      #RegData må inneholde DateAdmittedIntensive, DateDischargedIntensive 
      datoFra <- lubridate::floor_date(as.Date(datoTil)%m-% months(antMnd, abbreviate = T), 'month') #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
      aggVar <-  c('ShNavn', 'InnDato')
      RegDataDum <- RegData[RegData$InnDato <= as.Date(datoTil, tz='UTC')
                              & RegData$InnDato > as.Date(datoFra, tz='UTC'), aggVar]
      RegDataDum$Maaned1 <- floor_date(RegDataDum$InnDato, 'month')
      tabAvdMnd1 <- table(RegDataDum[ , c('ShNavn', 'Maaned1')])
      colnames(tabAvdMnd1) <- format(ymd(colnames(tabAvdMnd1)), '%b%y') #month(ymd(colnames(tabAvdMnd1)), label = T)
      tabAvdMnd1 <- addmargins((tabAvdMnd1))
      #tabAvdMnd1 <- RegDataDum %>% group_by(Maaned=floor_date(InnDato, "month"), ShNavn) %>%
      #      summarize(Antall=length(ShNavn))
      tabAvdMnd1 <- xtable::xtable(tabAvdMnd1)
	return(tabAvdMnd1)
}

#' @section Antall opphold siste 5 år
#' @rdname NordicScirtabeller
#' @export
#' 
tabAntOpphSh5Aar <- function(RegData, datoTil){
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
# tabAntPasSh5Aar <- function(RegData, personIDvar='PasientID' , datoTil){
#       AarNaa <- as.numeric(format.Date(datoTil, "%Y"))
#       
#       Data <- RegData[which(RegData$Aar %in% (AarNaa-4):AarNaa), c('ShNavn','Aar', personIDvar)]
#       tabPasAvdAarN <- tapply(Data$PasientID, Data[ c('ShNavn','Aar')], FUN=function(x) length(unique(x)))
#       tabPasAvdAarN[is.na(tabPasAvdAarN)] <- 0
#       
#       tabPasAvdAarN <- addmargins(tabPasAvdAarN) #, FUN = function(x) sum(x, na.rm=T))
#       rownames(tabPasAvdAarN)[dim(tabPasAvdAarN)[1] ]<- 'TOTALT, alle enheter:'
#       colnames(tabPasAvdAarN)[dim(tabPasAvdAarN)[2] ]<- 'TOTALT'
#       tabPasAvdAarN <- xtable::xtable(tabPasAvdAarN)
#       return(tabPasAvdAarN)
# }

#Tabell: Andel registreringsskjema med oppfølging siste 12 mnd


#' @section Antall opphold siste 5 år
#' @rdname NordicScirtabeller
#' @export
#' 
tabOppfShus <- function(HovedSkjema, datoTil){
      #RegData
      RaaTab <- data.frame(Sykehus = HovedSkjema$ShNavn,
                     #Aar = as.POSIXlt(Hskjema$AdmitDt, format="%Y-%m-%d")$year +1900,
                     Livskvalitet = HovedSkjema$SkjemaGUID %in% Livskvalitet$HovedskjemaGUID,
                     #Kontroll = HovedSkjema$SkjemaGUID %in% Kontroll$HovedskjemaGUID,
                     Urin = HovedSkjema$SkjemaGUID %in% Urin$HovedskjemaGUID,
                     Tarm = HovedSkjema$SkjemaGUID %in% Tarm$HovedskjemaGUID,
                     Funksjon = HovedSkjema$SkjemaGUID %in% AktivFunksjon$HovedskjemaGUID,
                     Tilfredshet = HovedSkjema$SkjemaGUID %in% 
                           AktivFunksjon$HovedskjemaGUID[AktivFunksjon$SkjemaGUID %in% Satisfact$HovedskjemaGUID]
)

AntReg <- table(HovedSkjema$ShNavn)
#AntOppf <- apply(RaaTab[ ,-1], MARGIN=2, 
#                      FUN=function(x) table(x,RaaTab$Sykehus)['TRUE',])
AntOppf <- apply(RaaTab[ ,-1], MARGIN=2, 
                 FUN=function(x) tapply(x,INDEX=RaaTab$Sykehus, sum))
AndelOppf <- 100*AntOppf / as.vector(AntReg)
#AndelOppf <- 100*apply(RaaTab[ ,-1], MARGIN=2, 
#                       FUN=function(x) prop.table(tapply(x, RaaTab$Sykehus, sum),margin=2)['TRUE',])

xtable::xtable(rbind(Hoved=AntReg,t(AntOppf)), digits=0, align=c('l', rep('r', nrow(AntOppf))),
               caption=paste0('Antall hovedskjema og antall av disse som har tilknyttede skjema. Innleggelser fra og med ', datoFra, '.'))

xtable::xtable(t(AndelOppf), digits=1, align=c('l', rep('r', nrow(AndelOppf))),
               caption=paste0('Andel (prosent) av registreringsskjemaene som har ulike typer oppfølgingsskjema.'))
return(tab)
}

#' @section Vise figurdata som tabell
#' @rdname NordicScirtabeller
#' @export
lagTabavFig <- function(UtDataFraFig){
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