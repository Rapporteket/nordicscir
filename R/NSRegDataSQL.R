#' Provide global dataframe for NorScir
#'
#' Provides NorScir data from staging
#'
#' @return RegData data frame
#' @export

NSRegDataSQL <- function() {

  registryName <- "nordicscir"
  dbType <- "mysql"

  query <- "SELECT
      AAis,
      AdmitDt,
      AdmitRehDt,
      AMtrLvlAreaL,
      AMtrLvlAreaR,
      ANeuExmDt,
      ANeuNoMeasure,
      ASensLvlAreaL,
      ASensLvlAreaR,
      BeforeRehDy,
      DischgDt,
      FAis,
      FMtrLvlAreaL,
      FMtrLvlAreaR,
      FNeuExmDt,
      FNeuNoMeasure,
      FSensLvlAreaL,
      FSensLvlAreaR,
      HosptlDy,
      InjuryDateUnknown,
      InjuryDt,
      OutOfHosptlDy,
      OutOfHosptlDy2,
      OutOfRehabDy,
      PasientGUID,
      PatientAge,
      PatientGender,
      PlaceDis,
      RehabDy,
      ReshId,
      Scietiol,
      SkjemaGUID,
      VentAssi
FROM
     MainFormDataContract"

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

 #Tidligere med:  
  #isVrtbrInj,
  #isAssocInj,
  #isSpnlSurg,
  #AlderAar,
  #ShNavn,
  #BirthDt,
  #isMale,
  #SkjemaID,
  #DagerRehab,
  #DagerTilRehab,
  #NevrNivaaInn,
  #NevrNivaaUt
  #QolDt,
  #SatGenrl,
  #SatPhys,
  #SatPsych,
  
  
  return(RegData)
}
