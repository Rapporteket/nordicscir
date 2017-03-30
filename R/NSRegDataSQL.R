#' Henter data for NordicScir fra  staging
#'
#' Spørring som henter og kobler sammen data fra ulike skjema i NordicScir
#'
#' Aktuelle skjema: 
#' ActivityAndParticipationPerformanceFormDataContract: Aktivitetsfunksjon
#' ActivityAndParticipationSatisfactionFormDataContract: Aktivitetsfornøydhet
#' BowelFunctionFormDataContract: Tarmfunksjon (?)
#' ControlFormDataContract: Kontroll. Noen variable er oppfølging av målinger i hovedskjema
#' LifeQualityFormDataContract: Livskvalitet
#' MainFormDataContract: HOVEDSKJEMA
#' UrinaryTractFunctionFormDataContract: Urinfunksjon
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
      HealthUnitName,
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
      UnitId AS ReshId,
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
