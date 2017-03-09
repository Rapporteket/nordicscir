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
      
     #Med bruk av ekstra pakker kan dette gjøres mer elegant 
      
      #Kjønn
      RegData$erMann <- NULL
      RegData$erMann[RegData$PatientGender == 'Female'] <- 0
      RegData$erMann[RegData$PatientGender == 'Male'] <- 1
      
      #Riktig navn på regions-variabel:
      #Ingen ønsketregionsinndeling!
      #RegData$Region <- RegData$RHF
      
      # Endre variabelnavn:
      names(RegData)[which(names(RegData) == 'PatientAge')] <- 'Alder'
      names(RegData)[which(names(RegData) == 'HosptlDy')] <- 'OpphTot'  #Sjekk forskjell HosptlDy og ..2
      names(RegData)[which(names(RegData) == 'PlaceDis')] <- 'UtTil' 
      names(RegData)[which(names(RegData) == 'Scietiol')] <- 'SkadeArsak' 
      names(RegData)[which(names(RegData) == 'VentAssi')] <- 'Pustehjelp' 
      names(RegData)[which(names(RegData) == 'RehabDy')] <- 'DagerRehab' 
      names(RegData)[which(names(RegData) == 'BeforeRehDy')] <- 'DagerTilRehab' 
      #names(RegData)[which(names(RegData) == 'OutOfHosptlDy')] <- 'Permisjon'
      RegData$Permisjon <- with(RegData, OutOfHosptlDy+OutOfHosptlDy2+OutOfRehabDy)
      
      # Riktig format
      #	RegData$alder <- as.numeric(RegData$decimalAge)	#
      
      #Riktig format på datovariable:
      RegData$InnDato <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")
      RegData$Aar <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")$year +1900
      
      #RegData <- RegData[which(RegData$DateAdmittedIntensive!=''),]	#Tar ut registreringer som ikke har innleggelsesdato
      #RegData$InnDato <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d") 
      #RegData$Innleggelsestidspunkt <- as.POSIXlt(RegData$DateAdmittedIntensive, format="%Y-%m-%d %H:%M:%S" )
      #RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
      #RegData$Aar <- 1900 + strptime(RegData$DateAdmittedIntensive, format="%Y")$year
      
      return(invisible(RegData))
}
