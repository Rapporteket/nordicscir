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
      
  #   RegData$ShNavn <- factor(RegData$ReshId, levels=c(105593, 106896, 107627), 
  #                                        labels=c('Haukeland', 'Sunnaas', 'St.Olavs'))
      
     #med bruk av ekstra pakker kan dette gjøres mer elegant 
      #NYE VARIABLE
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
      RegData$InnDato <- as.Date(RegData$AdmitDt, tz= 'UTC', format="%Y-%m-%d") #as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")
      RegData$AdmitDt <- strptime(RegData$AdmitDt, format="%Y-%m-%d")

      #Kun ferdigstilte registreringer:
      # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.Men det kan dukke opp ufullstendige registreringer.
      #Fjerner de som mangler sykehus og eller AdmitDt:
      RegData <- RegData[which(RegData$ReshId %in% c(105593, 106896, 107627)), ] #dplyr::filter(RegData, ReshId %in% c(105593, 106896, 107627))
      #RegData <- RegData[-which(is.na(RegData$AdmitDt)), ]
      
            # Riktig format
      #	RegData$alder <- as.numeric(RegData$decimalAge)	#
      RegData$ShNavn <- as.factor(RegData$ShNavn)
      

      # Nye variable:
      RegData$MndNum <- RegData$AdmitDt$mon +1
      head(format(RegData$AdmitDt, '%b'))
      RegData$MndAar <- format(RegData$AdmitDt, '%b%y')
      RegData$Kvartal <- ceiling(RegData$MndNum/3)
      RegData$Halvaar <- ceiling(RegData$MndNum/6)
      RegData$Aar <- 1900 + RegData$AdmitDt$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year
      
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
