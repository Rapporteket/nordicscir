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

NSRegDataSQL <- function(valgtVar='Alder') {
      
registryName <- 'nordicscir' #"nordicscir"
dbType <- "mysql"
   
#HovedSkjema: MainFormDataContract
#Livs: LifeQualityFormDataContract
#Urin: UrinaryTractFunctionFormDataContract
#Tarm: BowelFunctionFormDataContract
#Sati: ActivityAndParticipationSatisfactionFormDataContract
#Perf: ActivityAndParticipationPerformanceFormDataContract
#Kont: ControlFormDataContract


   
varHoved <- paste0("
      h.AAis,
      h.AdmitDt,
      h.AdmitRehDt,
      h.AMtrLvlAreaL,
      h.AMtrLvlAreaR,
      h.ANeuExmDt,
      h.ANeuNoMeasure,
      h.ASensLvlAreaL,
      h.ASensLvlAreaR,
      h.BeforeRehDy,
      h.DischgDt,
      h.FAis,
      h.FirstTimeClosed,
      h.FMtrLvlAreaL,
      h.FMtrLvlAreaR,
      h.FNeuExmDt,
      h.FNeuNoMeasure,
      h.FSensLvlAreaL,
      h.FSensLvlAreaR,
      h.HealthUnitShortName,
      h.HosptlDy,
      h.InjuryDateUnknown,
      h.InjuryDt,
      h.Ntsci,
      h.OutOfHosptlDy,
      h.OutOfHosptlDy2,
      h.OutOfRehabDy,
      h.PatientAge,
      h.PatientGender,
      h.PlaceDis,
      h.PPlacedis,
      h.RehabDy,
      h.UnitId AS ReshId,
      h.Scietiol,
      h.SkjemaGUID,
      h.VentAssi
")
#h.PasientGUID,

varLivs <- c('
,Livs.FormDate
,Livs.HovedskjemaGUID
,Livs.QolDt
,Livs.SatGenrl
,Livs.SatPhys
,Livs.SatPsych
,Livs.SkjemaGUID
')
#,Livs.PasientGUID

varFunk <- c('
,Funk.DataClDt	
,Funk.Dreslbdy	
,Funk.Feeding	
,Funk.FirstTimeClosed
,Funk.HovedskjemaGUID
,Funk.Mobilmod	
,Funk.SkjemaGUID	
,Funk.Toiletin	
')

varTilf <- c('
,Tilf.DataClDtS	
,Tilf.DreslbdyS	
,Tilf.FeedingS	
,Tilf.FirstTimeClosed
,Tilf.HovedskjemaGUID
,Tilf.MobilmodS	
,Tilf.SkjemaGUID	
,Tilf.ToiletinS	
')
varUrin <- c("
,Urin.Antiprop
,Urin.Antiuti
,Urin.AnyDrugs
,Urin.Artsph
,Urin.ArtsphDt
,Urin.Avbladem
,Urin.Awarblad
,Urin.Bladag
,Urin.BladagDt
,Urin.Bladrelx
,Urin.Botox
,Urin.BotoxDt
,Urin.Bstnrm
,Urin.BstnrmDt
,Urin.Ccathv
,Urin.CcathvDt
,Urin.Collect
,Urin.Condcath
,Urin.Diaperpd
,Urin.DrugsAnti
,Urin.EmbladM1
,Urin.EmbladM10
,Urin.EmbladM11
,Urin.EmbladM12
,Urin.EmbladM2
,Urin.EmbladM3
,Urin.EmbladM4
,Urin.EmbladM5
,Urin.EmbladM6
,Urin.EmbladM7
,Urin.EmbladM8
,Urin.EmbladM9
,Urin.EmbladS1
,Urin.EmbladS10
,Urin.EmbladS11
,Urin.EmbladS12
,Urin.EmbladS2
,Urin.EmbladS3
,Urin.EmbladS4
,Urin.EmbladS5
,Urin.EmbladS6
,Urin.EmbladS7
,Urin.EmbladS8
,Urin.EmbladS9
,Urin.EmbladUn
,Urin.FormDate
,Urin.HealthUnitId
,Urin.HealthUnitName
,Urin.HovedskjemaGUID
,Urin.Ilurts
,Urin.IlurtsDt
,Urin.Ilvscs
,Urin.IlvscsDt
,Urin.Incontnc
,Urin.LastUpdate
,Urin.LutfxnDt
,Urin.MajorVersion
,Urin.MinorVersion
,Urin.Ostmybag
,Urin.Othcolap
,Urin.Othdrg
,Urin.Othsrg
,Urin.OthsrgDt
,Urin.Sarstm
,Urin.SarstmDt
,Urin.SkjemaGUID
,Urin.Spcath
,Urin.SpcathDt
,Urin.Spncrelx
,Urin.Surgicalpr
,Urin.UnitId
,Urin.Ursxchly
,Urin.Ustent
,Urin.UstentDt
,Urin.Ustnrm
,Urin.UstnrmDt
,Urin.Utimprun
")
#,Urin.PasientGUID

varTarm <- c('
,Tarm.Antichol
,Tarm.Apndec
 ,Tarm.ApndecDt
 ,Tarm.ApndecDtUnknown
 ,Tarm.Avdeftm
 ,Tarm.BfxnbaDt
 ,Tarm.Chcyec
 ,Tarm.ChcyecDt
 ,Tarm.ChcyecDtUnknown
 ,Tarm.Colost
 ,Tarm.ColostDt
 ,Tarm.ColostDtUnknown
 ,Tarm.Defawrns
 ,Tarm.DefcmthM1
 ,Tarm.DefcmthM10
 ,Tarm.DefcmthM2
 ,Tarm.DefcmthM3
 ,Tarm.DefcmthM4
 ,Tarm.DefcmthM5
 ,Tarm.DefcmthM6
 ,Tarm.DefcmthM7
 ,Tarm.DefcmthM8
 ,Tarm.DefcmthM9
 ,Tarm.DefcmthUn
 ,Tarm.Deffrq
 ,Tarm.DrugUse
 ,Tarm.Fcincfrq
 ,Tarm.Fissures
 ,Tarm.FormDate
 ,Tarm.FormTypeId
 ,Tarm.Gifxnun
 ,Tarm.Hemrhoid
 ,Tarm.HovedskjemaGUID
 ,Tarm.Ileost
 ,Tarm.IleostDt
 ,Tarm.IleostDtUnknown
 ,Tarm.Irrtdrp
 ,Tarm.Irrttab
 ,Tarm.Narcotic
 ,Tarm.OralLaxatives
 ,Tarm.Osmodrp
 ,Tarm.Osmotab
 ,Tarm.Otgisurg
 ,Tarm.OtgisurgDt
 ,Tarm.OtgisurgDtUnknown
 ,Tarm.Othbfmed
 ,Tarm.OthdefS1
 ,Tarm.OthdefS10
 ,Tarm.OthdefS2
 ,Tarm.OthdefS3
 ,Tarm.OthdefS4
 ,Tarm.OthdefS5
 ,Tarm.OthdefS6
 ,Tarm.OthdefS7
 ,Tarm.OthdefS8
 ,Tarm.OthdefS9
 ,Tarm.Othorlax
 ,Tarm.Panloth
 ,Tarm.Panlsore
 ,Tarm.PerianalProblems
 ,Tarm.Prokinet
 ,Tarm.Recprlps
 ,Tarm.SkjemaGUID
 ,Tarm.SurgicalIntervention
 ,Tarm.Wrpadplg
 ')
# ,Tarm.PasientGUID


valgtSkjema <- substr(valgtVar,1,4)

variable <- ''
qSkjema <- ''
if (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Tilf', 'Funk', 'Kont')) { 
      variable <- switch(valgtSkjema,
                         Livs = varLivs,
                         Urin = varUrin, 
                         Tarm = varTarm,
                         Funk = varFunk,
                         Tilf = varTilf,
                         Kont = varKont)
      
      qSkjema <- paste0(switch(valgtSkjema, #Dette vil bare fungere hvis konsekvent med navngiving i valgtVar
           Livs = 'INNER JOIN LifeQualityFormDataContract Livs ',
           Urin = 'INNER JOIN UrinaryTractFunctionFormDataContract Urin ',
           Tarm = 'INNER JOIN BowelFunctionFormDataContract Tarm ',
           Funk = 'INNER JOIN ActivityAndParticipationPerformanceFormDataContract Funk ',
           Kont = 'INNER JOIN ControlFormDataContract k '
           ),
           'ON UPPER(h.SkjemaGUID) = UPPER(',valgtSkjema , '.HovedskjemaGUID) ')
      #qSkjema er NULL hvis ingen treff 
      if (valgtSkjema=='Tilf') {
            qSkjema <- 'INNER JOIN ActivityAndParticipationPerformanceFormDataContract Funk 
                        ON UPPER(h.SkjemaGUID) = UPPER(Funk.HovedskjemaGUID) 
                        INNER JOIN ActivityAndParticipationSatisfactionFormDataContract Tilf
                        ON UPPER(Funk.SkjemaGUID) = UPPER(Tilf.HovedskjemaGUID)'
                  }
      }

query <- paste0('SELECT ',
               varHoved,
               variable,
            ' FROM
            MainFormDataContract h ',
            qSkjema
            )


RegData <- rapbase::LoadRegData(registryName, query, dbType)
return(RegData)
}
