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
      #Kun ferdigstilte registreringer:
      # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.
      
  #   RegData$ShNavn <- factor(RegData$ReshId, levels=c(105593, 106896, 107627), 
  #                                        labels=c('Haukeland', 'Sunnaas', 'St.Olavs'))
      
     #med bruk av ekstra pakker kan dette gjøres mer elegant 
      #NYE VARIABLE
      #Kjønn
      RegData$erMann <- NULL
      RegData$erMann[RegData$PatientGender == 'Female'] <- 0
      RegData$erMann[RegData$PatientGender == 'Male'] <- 1
      
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
      #Riktig navn på regions-variabel:
      #Ingen ønsketregionsinndeling!
      #RegData$Region <- RegData$RHF
      
      names(RegData)[which(names(RegData) == 'UnitId')] <- 'ReshId'
      names(RegData)[which(names(RegData) == 'HealthUnitName')] <- 'ShNavn'
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      names(RegData)[which(names(RegData) == 'PlaceDis')] <- 'UtTil' 
      names(RegData)[which(names(RegData) == 'Scietiol')] <- 'SkadeArsak' 
      names(RegData)[which(names(RegData) == 'VentAssi')] <- 'Pustehjelp' 
      names(RegData)[which(names(RegData) == 'HosptlDy')] <- 'OpphTot'  #Sjekk forskjell HosptlDy og ..2
      names(RegData)[which(names(RegData) == 'RehabDy')] <- 'DagerRehab' 
      names(RegData)[which(names(RegData) == 'BeforeRehDy')] <- 'DagerTilRehab' 
      #names(RegData)[which(names(RegData) == 'OutOfHosptlDy')] <- 'Permisjon'
      #RegData$Permisjon <- with(RegData, OutOfHosptlDy+OutOfHosptlDy2+OutOfRehabDy)
      
      # Riktig format
      #	RegData$alder <- as.numeric(RegData$decimalAge)	#
      
      #Riktig format på datovariable:
      RegData$InnDato <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")
      RegData$Aar <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")$year +1900
      
      #Konvertere boolske variable fra tekst til boolske variable...
      TilLogiskeVar <- function(Skjema){
            verdiGML <- c('True','False')
            verdiNY <- c(TRUE,FALSE)
            mapping <- data.frame(verdiGML,verdiNY)
            LogVar <- names(Skjema)[which(Skjema[1,] %in% verdiGML)]
            for (k in 1:length(LogVar)) {
                  Skjema[,LogVar[k]] <- mapping$verdiNY[match(Skjema[,LogVar[k]], mapping$verdiGML)]
            }
            return(Skjema)
      }
      
      RegData <- TilLogiskeVar(RegData)
      
      
      return(invisible(RegData))
}
