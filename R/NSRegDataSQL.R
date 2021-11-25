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
      
#HovedSkjema: MainFormDataContract
#Livs: LifeQualityFormDataContract
#Urin: UrinaryTractFunctionFormDataContract
#Tarm: BowelFunctionFormDataContract
#Sati: ActivityAndParticipationSatisfactionFormDataContract
#Perf: ActivityAndParticipationPerformanceFormDataContract
#Kont: ControlFormDataContract


   
varHoved <- c("
      h.AAis,
      h.AdmitDt,
      h.AdmitRehDt,
      h.AMtrLvlAreaL,
      h.AMtrLvlAreaR,
      h.ANeuExmDt,
      h.ANeuNoMeasure,
      h.ASensLvlAreaL,
      h.ASensLvlAreaR,
      h.ASensLvlLC,
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
      h.RecCtrl,
      h.Scietiol,
      h.SkjemaGUID,
--      h.SkjemaGUID AS SkjemaGUIDhoved,
      h.UnitId,
      h.VentAssi
")
#h.PasientGUID,
#h.UnitId AS ReshId,

varLivs <- c('
,Livs.FormDate
,UPPER(Livs.HovedskjemaGUID) AS HovedskjemaGUID
,Livs.QolDt
,Livs.SatGenrl
,Livs.SatPhys
,Livs.SatPsych
-- ,Livs.SkjemaGUID AS SkjemaGUID
-- ,Livs.SkjemaGUID AS SkjemaGUIDLivs
')
#,Livs.PasientGUID

varFunk <- c('
,Funk.DataClDt	
,Funk.Dreslbdy	
,Funk.Feeding	
,Funk.FirstTimeClosed
,UPPER(Funk.HovedskjemaGUID) AS HovedskjemaGUID
,Funk.Mobilmod	
-- ,Funk.SkjemaGUID
-- ,Funk.SkjemaGUID AS SkjemaGUIDFunk
,Funk.Toiletin	
')

varTilf <- c('
,Tilf.DataClDtS	
,Tilf.DreslbdyS	
,Tilf.FeedingS	
,Tilf.FirstTimeClosed
-- ,UPPER(Tilf.HovedskjemaGUID) AS HovedskjemaGUID
,Tilf.MobilmodS
,UPPER(Funk.HovedskjemaGUID) AS HovedskjemaGUID
-- ,Tilf.SkjemaGUID
-- ,Tilf.SkjemaGUID AS SkjemaGUIDTilf
,Tilf.ToiletinS	
')
varUrin <- c("
,Urin.Antiprop
,Urin.Antiuti
,Urin.AnyDrugs
,Urin.AnyDrugs2
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
,UPPER(Urin.HovedskjemaGUID) AS HovedskjemaGUID
,Urin.Ilurts
,Urin.IlurtsDt
,Urin.Ilvscs
,Urin.IlvscsDt
,Urin.Incontnc
,Urin.Incontnc2
,Urin.LastUpdate
,Urin.LutfxnDt
-- ,Urin.MajorVersion #Fjernet nov. 2021?
-- ,Urin.MinorVersion #Fjernet nov. 2021?
,Urin.Ostmybag
,Urin.Othcolap
,Urin.Othdrg
,Urin.Othsrg
,Urin.OthsrgDt
,Urin.Sarstm
,Urin.SarstmDt
-- ,Urin.SkjemaGUID
-- ,Urin.SkjemaGUID AS SkjemaGUIDUrin
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
 ,Tarm.Apndic
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
 ,Tarm.DefcmthNa
 ,Tarm.DefcmthUn
 ,Tarm.Deffrq
 ,Tarm.DrugUse
 ,Tarm.Fcincfrq
 ,Tarm.Fcincfrq2
 ,Tarm.Fcincfrq3
 ,Tarm.Fissures
 ,Tarm.FormDate
 ,Tarm.FormTypeId
 ,Tarm.Gifxnun
 ,Tarm.Hemec 
 ,Tarm.Hemrhoid
 ,UPPER(Tarm.HovedskjemaGUID) AS HovedskjemaGUID
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
-- ,Tarm.SkjemaGUID
-- ,Tarm.SkjemaGUID AS SkjemaGUIDTarm
 ,Tarm.SurgicalIntervention
 ,Tarm.Wrpadplg
 ')
# ,Tarm.PasientGUID

varKont <- c('
      ,Kont.CAis
,Kont.CMtrLvlAreaL
,Kont.CMtrLvlAreaR
,Kont.CMtrLvlLC
,Kont.CMtrLvlLL
,Kont.CMtrLvlLS
,Kont.CMtrLvlLT
,Kont.CMtrLvlRC
,Kont.CMtrLvlRL            
,Kont.CMtrLvlRS
,Kont.CMtrLvlRT
,Kont.CNeuExmDt
,Kont.CNeuNoMeasure
,Kont.CNum                 
,Kont.CPlaceDis
,Kont.CSensLvlAreaL
,Kont.CSensLvlAreaR
,Kont.CSensLvlLC
,Kont.CSensLvlLL
,Kont.CSensLvlLS
,Kont.CSensLvlLT
,Kont.CSensLvlRC
,Kont.CSensLvlRL
,Kont.CSensLvlRS           
,Kont.CSensLvlRT
,Kont.CVentAssi
,Kont.FirstTimeClosed
,Kont.FormDate      
,Kont.FormStatus
,Kont.FormTypeId
,UPPER(Kont.HovedskjemaGUID) AS HovedskjemaGUID
,Kont.LastUpdate
,Kont.NoControl
,Kont.NoControlReason      
,Kont.ProceedingID
-- ,Kont.SkjemaGUID
-- ,Kont.SkjemaGUID AS SkjemaGUIDKont
,Kont.UnitId
')
#"HealthUnitId","HealthUnitName","HealthUnitShortName","HF" ,"Hospital","RHF"          


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
           Kont = 'INNER JOIN ControlFormDataContract Kont '
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


#query <- 'select * from MainFormDataContract'


RegData <- rapbase::loadRegData(registryName = 'nordicscir', query=query, dbType="mysql")
return(RegData)
}
