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
#' @rdname NordicScirtabeller
#' @export

#' @section Belegg (antall opphold, pasienter og intensivdøgn)
#' @rdname NordicScirtabeller
#' Siste inntil 5 år eller siste inntil 12 måneder/kvartal/halvår
#' @export
tabBelegg <- function(RegData, tidsenhet='Aar', datoTil, enhetsUtvalg=0, reshID=0) {
      datoFra <- switch(tidsenhet, 
                        Mnd = lubridate::floor_date(as.Date(datoTil)%m-% months(12, abbreviate = T), 'month'), #as.Date(paste0(as.numeric(substr(datoTil,1,4))-1, substr(datoTil,5,8), '01'), tz='UTC')
                        Aar = paste0(lubridate::year(as.Date(datoTil))-4, '-01-01')
      )
      RegData <- NSUtvalg(RegData=RegData, datoFra=datoFra, datoTil = datoTil, 
                             enhetsUtvalg = enhetsUtvalg, reshID = reshID)$RegData
      RegData <- SorterOgNavngiTidsEnhet(RegData, tidsenhet=tidsenhet)$RegData
      #RegData <- Mtid$RegData
      tabBeleggAnt <- rbind('Ferdigstilte rehab.opphold' = tapply(RegData$PasientID, RegData$TidsEnhet, FUN=length), #table(RegDataEget$TidsEnhet), #Neget,		
                            'Registrerte pasienter' = tapply(RegData$PasientID, RegData$TidsEnhet, 
                                                             FUN=function(x) length(unique(x))),	
                            'Antal rehab.døgn' = round(as.numeric(tapply(RegData$liggetid, RegData$TidsEnhet, sum, na.rm=T)),0)	
      )

      antTidsenh <- ifelse(tidsenhet=='Aar', 4, 11)

      tabBeleggAnt <- tabBeleggAnt[, max(1, dim(tabBeleggAnt)[2]-antTidsenh) : dim(tabBeleggAnt)[2]] #Tar med 12 siste
      #format(as.yearmon(as.Date('2017-09-02')),'%b%y')
      #kol <- dimnames(tabBeleggAnt)[[2]]
      #format(as.Date(kol, format= '%y.%m'),'%b%y')
      
      # overskr <- dimnames(tabAvdNEget)[[2]]
      # aar <- substr(overskr, 1,2)
      # mnd <- as.numeric(substr(overskr, 4,5))
      # mndTxt <- c('jan', 'feb', 'mar', 'apr', 'mai', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'des') 
      # colnames(tabAvdNEget) <- paste0(mndTxt[mnd], aar)
      #tabBeleggAnt <- xtable::xtable(tabBeleggAnt, digits=0, align=c('l', rep('r', ncol(tabBeleggAnt))),
       #      caption=paste0('Antal opphald og liggedøger, ', shtxt,'.'), label='tab:RegEget')
      return(tabBeleggAnt)
}
#' @section tabAntOpphShMnd antall opphold siste X (antMnd) mnd
#' @rdname NordicScirtabeller
#' @export
tabAntOpphShMnd <- function(RegData, datoTil, antMnd=6){
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

#' @section Finn eventuelle dobbeltregistreringer
#' @rdname NordicScirtabeller
#--------#' @export
finnDblReg <- function(RegData, datoFra='2017-01-01', datoTil=Sys.Date(), reshID=114240){
      #Registreringer kor same pasient har fått registrert to innleggingar med mindre enn 2 timars mellomrom.
      #RegData må inneholde PasientID, Innleggelsestidspunkt og SkjemaGUID
      #Evt. legge til utvalg på tidsrom
      sortVar <- c('PasientID','Innleggelsestidspunkt', "SkjemaGUID")
      RegData <- RegData[which(RegData$ReshId == reshID), sortVar]
      RegDataSort <- RegData[order(RegData$PasientID, RegData$Innleggelsestidspunkt), ]
      RegDataSort$OpphNr <- ave(RegDataSort[ ,'PasientID'], RegDataSort[ ,'PasientID'], FUN=seq_along)
      indPasFlereOpph <- which(RegDataSort$OpphNr>1) 
      RegDataSort$TidInn <- NA
      RegDataSort$TidInn[indPasFlereOpph] <- 
            difftime(RegDataSort$Innleggelsestidspunkt[indPasFlereOpph], 
                     RegDataSort$Innleggelsestidspunkt[indPasFlereOpph-1], 
                     units = 'hour')
      
      indDbl <- which(abs(RegDataSort$TidInn) <2 )
      tabDbl <- RegDataSort[sort(c(indDbl, indDbl-1)), 
                            c('PasientID','Innleggelsestidspunkt', "SkjemaGUID")]
      if (dim(tabDbl)[1] == 0) {
            tabDbl <- 'Ingen dobbeltregistreringar'
      } else {tabDbl <- xtable::xtable(tabDbl)}
      #print(paste('Dim RegDATA: ',  dim(RegData), min(RegData$InnDato)))
      return(tabDbl)
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