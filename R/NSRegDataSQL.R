#' Henter data for NorScir eller NordicScir fra database
#'
#' Spørring som henter og kobler sammen data fra ulike skjema i valgt register
#'
#' Aktuelle skjema:
#' MainFormDataContract: HOVEDSKJEMA
#' BowelFunctionFormDataContract: Tarmfunksjon (?)
#' LifeQualityFormDataContract: Livskvalitet
#' UrinaryTractFunctionFormDataContract: Urinfunksjon
#' ActivityAndParticipationPerformanceFormDataContract: Aktivitetsfunksjon.
#'      Skjema kun i det norske registeret
#' ActivityAndParticipationSatisfactionFormDataContract: Aktivitetsfornøydhet.
#'      Skjema kun i det norske registeret
#' ControlFormDataContract: Kontroll. Noen variable er oppfølging av målinger i hovedskjema.
#'      Skjema kun i det norske registeret
#' @param valgtVar Valg av variabel angir hvilket skjema som skal knyttes til hovedskjema
#'        siden de fire første bokstavene i variabelnavnet utgjør et prefiks for
#'        å identifisere hvilken tabell variabelen skal hentes fraaktuell tabell. Eks LivsAlder
#'        Variabler uten prefiks hentes fra hovedtabellen (Main...)
#' @param register Hvilket register det skal hentes data for: 'norscir' (standard) eller 'nordicscir'
#' @return RegData data frame
#' @export

NSRegDataSQL <- function(valgtVar='Alder', register='norscir',...) {

   #HovedSkjema: MainFormDataContract
   #Livs: LifeQualityFormDataContract
   #Urin: UrinaryTractFunctionFormDataContract
   #Tarm: BowelFunctionFormDataContract
   #Sati: ActivityAndParticipationSatisfactionFormDataContract (bare NorScir)
   #Perf: ActivityAndParticipationPerformanceFormDataContract (bare NorScir)
   #Kont: ControlFormDataContract (bare NorScir)
   #Eq5d: Eq5dlFormDataContract (bare NorScir)

   if ("session" %in% names(list(...))) {
      raplog::repLogger(session = list(...)[["session"]],
                        msg = "Starter SQL-funksjon")
   }
   # rapbase::autLogger(name='test',  pkg = 'nordicscir', user = 'dummy', registryName = register, reshId = 0,
   #                    fun=0, param=0, type=0,
   #                    msg = "Starter SQL-funksjon")

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
,UPPER(Funk.HovedskjemaGUID) AS HovedskjemaGUID
,Funk.DataClDt
,Funk.Dreslbdy
,Funk.Feeding
,Funk.FirstTimeClosed
,Funk.Mobilmod
-- ,Funk.SkjemaGUID
-- ,Funk.SkjemaGUID AS SkjemaGUIDFunk
,Funk.Toiletin
')

   varTilf <- c('
-- ,UPPER(Funk.HovedskjemaGUID) AS HovedskjemaGUID
 ,UPPER(Tilf.HovedskjemaGUID) AS HovedskjemaGUID
,Tilf.DataClDtS
,Tilf.DreslbdyS
,Tilf.FeedingS
,Tilf.FirstTimeClosed
,Tilf.MobilmodS
-- ,Tilf.SkjemaGUID
-- ,Tilf.SkjemaGUID AS SkjemaGUIDTilf
,Tilf.ToiletinS
')
   varUrin <- c("
,UPPER(Urin.HovedskjemaGUID) AS HovedskjemaGUID
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
 ,UPPER(Tarm.HovedskjemaGUID) AS HovedskjemaGUID
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
 ,Tarm.Ileost
 ,Tarm.IleostDt
 ,Tarm.IleostDtUnknown
 ,Tarm.Irrtdrp
 ,Tarm.Irrttab
 ,Tarm.Narcotic
 ,Tarm.NBD
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

   varEQ5D <- c('
,Eq5d.FormDate
,Eq5d.SkjemaGUID
,Eq5d.Eq5dQ1Mobility
,Eq5d.Eq5dQ2Selfcare
,Eq5d.Eq5dQ3UsualActivities
,Eq5d.Eq5dQ4PainDiscomfort
,Eq5d.Eq5dQ5AnxietyDepression
,Eq5d.Eq5dQ6HealthToday
,Eq5d.Eq5d5lDt
,Eq5d.ProceedingID
,Eq5d.ParentCNum
,UPPER(Eq5d.HovedskjemaGUID) AS HovedskjemaGUID
')

   varKont <- c('
 ,UPPER(Kont.HovedskjemaGUID) AS HovedskjemaGUID
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
   if (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Funk', 'Eq5d', 'Kont')) { #'Tilf'
      variable <- switch(valgtSkjema,
                         Livs = varLivs,
                         Urin = varUrin,
                         Tarm = varTarm,
                         Funk = varFunk,
                         #Tilf = varTilf,
                         Eq5d = varEQ5D,
                         Kont = varKont)

      qSkjema <- paste0(switch(valgtSkjema, #Dette vil bare fungere hvis konsekvent med navngiving i valgtVar
                               Livs = 'INNER JOIN LifeQualityFormDataContract Livs ',
                               Urin = 'INNER JOIN UrinaryTractFunctionFormDataContract Urin ',
                               Tarm = 'INNER JOIN BowelFunctionFormDataContract Tarm ',
                               Funk = 'INNER JOIN ActivityAndParticipationPerformanceFormDataContract Funk ',
                               Eq5d = 'INNER JOIN Eq5dlFormDataContract Eq5d ',
                               Kont = 'INNER JOIN ControlFormDataContract Kont '
      ),
      'ON UPPER(h.SkjemaGUID) = UPPER(',valgtSkjema , '.HovedskjemaGUID) ')

      #qSkjema er NULL hvis ingen treff
      # if (valgtSkjema=='Tilf') {
      #    qSkjema <- 'INNER JOIN ActivityAndParticipationPerformanceFormDataContract Funk
      #                   ON UPPER(h.SkjemaGUID) = UPPER(Funk.HovedskjemaGUID)
      #                   INNER JOIN ActivityAndParticipationSatisfactionFormDataContract Tilf
      #                   ON UPPER(Funk.SkjemaGUID) = UPPER(Tilf.HovedskjemaGUID)'
      # }
   }


   query <- paste0('SELECT ',
                   varHoved,
                   variable,
                   ' FROM
            MainFormDataContract h ',
                   qSkjema
   )


   #query <- 'select * from MainFormDataContract'
   #query <- paste0('SELECT ', variable, ' FROM Eq5dlFormDataContract Eq5d ')

   RegData <- rapbase::loadRegData(registryName = register, query=query, dbType="mysql")

   if (valgtSkjema=='Kont'){
     RegData <- RegData[RegData$NoControl=='False', ]
   }

   if (valgtSkjema=='Tilf') {
     #RegData er nå Hovedskjema
     qTilf <- 'SELECT UPPER(HovedskjemaGUID) AS FunkskjemaGUID,
      DataClDtS, DreslbdyS, FeedingS, FirstTimeClosed, MobilmodS, ToiletinS
                     FROM ActivityAndParticipationSatisfactionFormDataContract'
     TilfData <- rapbase::loadRegData(registryName = 'norscir', query=qTilf, dbType="mysql")

     qFunkTilf <- 'SELECT UPPER(HovedskjemaGUID) AS HovedskjemaGUID, SkjemaGUID AS FunkskjemaGUID FROM
     ActivityAndParticipationPerformanceFormDataContract'
     FunkVarKobl <- rapbase::loadRegData(registryName = 'norscir', query=qFunkTilf, dbType="mysql")
     FunkTilf <- FunkVarKobl %>%
       dplyr::inner_join(TilfData, by = dplyr::join_by(FunkskjemaGUID))

     RegData <- RegData %>%
       dplyr::inner_join(FunkTilf, by = dplyr::join_by(SkjemaGUID == HovedskjemaGUID))
   }

   if ("session" %in% names(list(...))) {
      raplog::repLogger(session = list(...)[["session"]],
                        msg = paste0('Har hentet skjema ', valgtSkjema, 'fra database'))
   }
   # rapbase::autLogger(name='test',  pkg = 'nordicscir', user = 'dummy', registryName = register, reshId = 0,
   #                    fun=0, param=0, type=0,
   #                    msg = paste0('Har hentet skjema ', valgtSkjema, 'fra database'))

   return(RegData)
}
