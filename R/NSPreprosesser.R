#' Preprosesserer data fra Nordic Scir
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#'
#' @inheritParams NSFigAndeler
#'
#' @return Data En liste med det filtrerte datasettet
#'
#' @export
#'
NSPreprosesser <- function(RegData)
      {

   #Kjønn
      RegData$erMann <- RegData$PatientGender
      RegData$erMann[RegData$PatientGender == 2] <- 0 #Ingen manglende verdier i variabelen

      #****'Tetraplegi*****.
      #      0: 'Paraplegi', 1: 'Tetraplegi', 9: 'Ukjent'.
      '%u%' <- union
      RegData$TetraplegiInn <- 0
      RegData$TetraplegiInn[which(RegData$ASensLvlAreaL == 1) %u% which(RegData$ASensLvlAreaR == 1) %u%
          which(RegData$AMtrLvlAreaL == 1) %u% which(RegData$AMtrLvlAreaR == 1)]  <- 1
      RegData$TetraplegiInn[(RegData$ASensLvlAreaL == 9) & (RegData$ASensLvlAreaR == 9) &
                (RegData$AMtrLvlAreaL == 9) & (RegData$AMtrLvlAreaR == 9)] <- 9
      RegData$TetraplegiInn[(RegData$ASensLvlAreaL == -1) & (RegData$ASensLvlAreaR == -1) &
                (RegData$AMtrLvlAreaL == -1) & (RegData$AMtrLvlAreaR == -1)]  <- 9

      #****'Tetraplegi ved utskrivning*****.
       RegData$TetraplegiUt <- 0
       RegData$TetraplegiUt[which(RegData$FSensLvlAreaL==1) %u% which(RegData$FSensLvlAreaR==1) %u%
            which(RegData$FMtrLvlAreaL==1) %u% which(RegData$FMtrLvlAreaR==1)] <-1
      RegData$TetraplegiUt[(RegData$FSensLvlAreaL==9) & (RegData$FSensLvlAreaR==9) &
          (RegData$FMtrLvlAreaL==9) & (RegData$FMtrLvlAreaR==9)] <-9
      RegData$TetraplegiUt[(RegData$FSensLvlAreaL==-1) & (RegData$FSensLvlAreaR==-1) &
          (RegData$FMtrLvlAreaL==-1) & (RegData$FMtrLvlAreaR==-1)]  <- 9

      # Endre variabelnavn:
      names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
      names(RegData)[which(names(RegData) == 'PatientInRegistryGuid')] <- 'PasientID'
      names(RegData)[which(names(RegData) == 'HealthUnitShortName')] <- 'ShNavn'
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      names(RegData)[which(names(RegData) == 'PlaceDis')] <- 'UtTil'
      names(RegData)[which(names(RegData) == 'Scietiol')] <- 'SkadeArsak'
      names(RegData)[which(names(RegData) == 'VentAssi')] <- 'Pustehjelp'
      names(RegData)[which(names(RegData) == 'HosptlDy')] <- 'OpphTot'  #Sjekk forskjell HosptlDy og ..2
      names(RegData)[which(names(RegData) == 'RehabDy')] <- 'DagerRehab'
      names(RegData)[which(names(RegData) == 'BeforeRehDy')] <- 'DagerTilRehab'
      #names(RegData)[which(names(RegData) == 'OutOfHosptlDy')] <- 'Permisjon'
      #RegData$Permisjon <- with(RegData, OutOfHosptlDy+OutOfHosptlDy2+OutOfRehabDy)

      #Riktig format på datovariable:
      RegData$InnDato <- as.Date(RegData$AdmitDt, tz= 'UTC', format="%Y-%m-%d")
      RegData$AdmitDt <- strptime(RegData$AdmitDt, format="%Y-%m-%d")
      RegData$UtDato <- as.Date(RegData$DischgDt, tz= 'UTC', format="%Y-%m-%d")
      RegData$DischgDt <- strptime(RegData$DischgDt, format="%Y-%m-%d")

      #Kun ferdigstilte registreringer:
      # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.Men det kan dukke opp ufullstendige registreringer.

      # Riktig format
      #RegData$ShNavn <- as.factor(RegData$ShNavn)


      # Nye variabler:
      RegData$MndNum <- RegData$AdmitDt$mon +1
      RegData$MndAar <- format(RegData$AdmitDt, '%b%y')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- 1900 + RegData$AdmitDt$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
      RegData$LandKode <- substr(RegData$ReshId, 1, 1)
      RegData$Land <- as.character(factor(RegData$LandKode, #1-N, 3-Fin, 4-Dan, 5-Is, 6-Sverige,
                             levels = c(1,3,4,5,6),
                             labels = c('Norge', 'Finland', 'Danmark', 'Island', 'Sverige')))

        #Legge til dummynavn for manglende enhetsnavn
      RegData$ShNavn[which(RegData$ReshId==30000001)] <- 'Finland1'
      RegData$ShNavn[which(RegData$ReshId==40000001)] <- 'København'
      RegData$ShNavn[which(RegData$ReshId==40000002)] <- 'Viborg'
      RegData$ShNavn[which(RegData$ReshId==50000001)] <- 'Reykjavik'
      RegData$ShNavn[which(RegData$ReshId==60000001)] <- 'Linköping'
        ind <- which(RegData$ShNavn == '')
        RegData$ShNavn[ind] <- RegData$ReshId[ind]

        # Legg på Trondheim, Bergen, Nesodden/Oslo
        if (sum(as.numeric(unique(RegData$LandKode)))!=1){
          RegData$ShNavn[RegData$ReshId == 105593] <- 'Bergen'
          RegData$ShNavn[RegData$ReshId == 106896] <- 'Nesodden/Oslo'
          RegData$ShNavn[RegData$ReshId == 107627] <- 'Trondheim'

        }

      # #Konvertere boolske variable fra tekst til boolske variable...
      # TilLogiskeVar <- function(Skjema){
      #       verdiGML <- c('True','False')
      #       verdiNY <- c(TRUE,FALSE)
      #       mapping <- data.frame(verdiGML,verdiNY)
      #       LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
      #       if (length(LogVar)>0) {
      #             for (k in 1:length(LogVar)) {
      #                   Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
      #             }}
      #       return(Skjema)
      # }

      RegData <- TilLogiskeVar(RegData)



      return(invisible(RegData))
}
