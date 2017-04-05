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
      
registryName <- "nordicscir"
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
      h.FMtrLvlAreaL,
      h.FMtrLvlAreaR,
      h.FNeuExmDt,
      h.FNeuNoMeasure,
      h.FSensLvlAreaL,
      h.FSensLvlAreaR,
      h.HealthUnitName,
      h.HosptlDy,
      h.InjuryDateUnknown,
      h.InjuryDt,
      h.OutOfHosptlDy,
      h.OutOfHosptlDy2,
      h.OutOfRehabDy,
      h.PasientGUID,
      h.PatientAge,
      h.PatientGender,
      h.PlaceDis,
      h.RehabDy,
      h.UnitId AS ReshId,
      h.Scietiol,
      h.SkjemaGUID,
     u.HovedskjemaGUID,
      h.VentAssi
")

varLivs <- c('
,l.FormDate
,l.HovedskjemaGUID
,l.PasientGUID
,l.QolDt
,l.SatGenrl
,l.SatPhys
,l.SatPsych
,l.SkjemaGUID
')

varUrin <- c("
,u.Antiprop
,u.Antiuti
,u.AnyDrugs
,u.Artsph
,u.ArtsphDt
,u.Avbladem
,u.Awarblad
,u.Bladag
,u.BladagDt
,u.Bladrelx
,u.Botox
,u.BotoxDt
,u.Bstnrm
,u.BstnrmDt
,u.Ccathv
,u.CcathvDt
,u.Collect
,u.Condcath
,u.Diaperpd
,u.DrugsAnti
,u.EmbladM1
,u.EmbladM10
,u.EmbladM11
,u.EmbladM12
,u.EmbladM2
,u.EmbladM3
,u.EmbladM4
,u.EmbladM5
,u.EmbladM6
,u.EmbladM7
,u.EmbladM8
,u.EmbladM9
,u.EmbladS1
,u.EmbladS10
,u.EmbladS11
,u.EmbladS12
,u.EmbladS2
,u.EmbladS3
,u.EmbladS4
,u.EmbladS5
,u.EmbladS6
,u.EmbladS7
,u.EmbladS8
,u.EmbladS9
,u.EmbladUn
,u.FormDate
,u.HealthUnitId
,u.HealthUnitName
,u.HovedskjemaGUID
,u.Ilurts
,u.IlurtsDt
,u.Ilvscs
,u.IlvscsDt
,u.Incontnc
,u.LastUpdate
,u.LutfxnDt
,u.MajorVersion
,u.MinorVersion
,u.Ostmybag
,u.Othcolap
,u.Othdrg
,u.Othsrg
,u.OthsrgDt
,u.PasientGUID
,u.Sarstm
,u.SarstmDt
,u.SkjemaGUID
,u.Spcath
,u.SpcathDt
,u.Spncrelx
,u.Surgicalpr
,u.UnitId
,u.Ursxchly
,u.Ustent
,u.UstentDt
,u.Ustnrm
,u.UstnrmDt
,u.Utimprun
")

varTarm <- c('
,t.Antichol
,t.Apndec
 ,t.ApndecDt
 ,t.ApndecDtUnknown
 ,t.Avdeftm
 ,t.BfxnbaDt
 ,t.Chcyec
 ,t.ChcyecDt
 ,t.ChcyecDtUnknown
 ,t.Colost
 ,t.ColostDt
 ,t.ColostDtUnknown
 ,t.Defawrns
 ,t.DefcmthM1
 ,t.DefcmthM10
 ,t.DefcmthM2
 ,t.DefcmthM3
 ,t.DefcmthM4
 ,t.DefcmthM5
 ,t.DefcmthM6
 ,t.DefcmthM7
 ,t.DefcmthM8
 ,t.DefcmthM9
 ,t.DefcmthUn
 ,t.Deffrq
 ,t.DrugUse
 ,t.Fcincfrq
 ,t.Fissures
 ,t.FormDate
 ,t.FormTypeId
 ,t.Gifxnun
 ,t.Hemrhoid
 ,t.HovedskjemaGUID
 ,t.Ileost
 ,t.IleostDt
 ,t.IleostDtUnknown
 ,t.Irrtdrp
 ,t.Irrttab
 ,t.Narcotic
 ,t.OralLaxatives
 ,t.Osmodrp
 ,t.Osmotab
 ,t.Otgisurg
 ,t.OtgisurgDt
 ,t.OtgisurgDtUnknown
 ,t.Othbfmed
 ,t.OthdefS1
 ,t.OthdefS10
 ,t.OthdefS2
 ,t.OthdefS3
 ,t.OthdefS4
 ,t.OthdefS5
 ,t.OthdefS6
 ,t.OthdefS7
 ,t.OthdefS8
 ,t.OthdefS9
 ,t.Othorlax
 ,t.Panloth
 ,t.Panlsore
 ,t.PasientGUID
 ,t.PerianalProblems
 ,t.Prokinet
 ,t.Recprlps
 ,t.SkjemaGUID
 ,t.SurgicalIntervention
 ,t.Wrpadplg
 ')


valgtSkjema <- substr(valgtVar,1,4)
variable <- ifelse (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Sati', 'Perf', 'Kont'), paste0('var',valgtSkjema),'')

qSkjema <- switch(valgtSkjema, #Dette vil bare fungere hvis konsekvent med navngiving i valgtVar
           Livs = 'INNER JOIN LifeQualityFormDataContract l',
           Urin = 'INNER JOIN UrinaryTractFunctionFormDataContract u 
                        ON UPPER(h.SkjemaGUID) = UPPER(u.HovedskjemaGUID)',
           Tarm = 'INNER JOIN BowelFunctionFormDataContract t
                       ON UPPER(h.SkjemaGUID) = UPPER(t.HovedskjemaGUID)',
           Sati = 'INNER JOIN ActivityAndParticipationSatisfactionFormDataContract s',
           Perf = 'INNER JOIN ActivityAndParticipationPerformanceFormDataContract p',
           Kont = 'INNER JOIN ControlFormDataContract k'
           )
#qSkjema er NULL hvis ingen treff


query <- paste0('SELECT ',
               varHoved,
               variable,
            'FROM
            MainFormDataContract h ',
            qSkjema
            )


RegData <- rapbase::LoadRegData(registryName, query, dbType)


#HovedSkjema <- rapbase::LoadRegData(registryName, qAkutt, dbType) #qAkuttskjema(datoFra = datoFra, datoTil = datoTil)   
#OppfSkjema <- rapbase::LoadRegData(registryName, qOppf, dbType) #qOppfskjema(datoFra = datoFra, datoTil = datoTil) 
#RegData <- merge(HovedSkjema, OppfSkjema, by.x='SkjemaGUID',by.y="HovedskjemaGUID", all.x = TRUE, all.y = FALSE)


      return(RegData)
}
