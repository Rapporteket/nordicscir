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
     Scietiol,
     VentAssi,
     PlaceDis,
     ASensLvlAreaL,
     ASensLvlAreaR,
     AMtrLvlAreaL,
     AMtrLvlAreaR,
     AAis,
     FSensLvlAreaL,
     FSensLvlAreaR,
     FMtrLvlAreaL,
     FMtrLvlAreaR,
     FAis,
     InjuryDt,
     AdmitDt,
     DischgDt,
     ANeuExmDt,
     FNeuExmDt,
     QolDt,
     AdmitRehDt,
     ANeuNoMeasure,
     FNeuNoMeasure,
     InjuryDateUnknown,
     OutOfHosptlDy,
     HosptlDy,
     SatGenrl,
     SatPhys,
     SatPsych,
     ReshId
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
  
  
  return(RegData)
}
