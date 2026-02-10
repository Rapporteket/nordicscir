#' Henter data for NorScir eller NordicScir fra database
#'
#' Spørring som henter og kobler sammen data fra ulike skjema i valgt register
#'
#' Aktuelle skjema:
#' spinal_cord_injury_core_data_set_1: HOVEDSKJEMA
#' bowel_function_5: Tarmfunksjon (?)
#' registration_of_quality_of_life_3: Livskvalitet
#' lower_urinary_tract_function_4: Urinfunksjon
#' activities_and_participation_performance_6: Aktivitetsfunksjon.
#'      Skjema kun i det norske registeret
#' activities_and_participation_satisfaction_7: Aktivitetsfornøydhet.
#'      Skjema kun i det norske registeret
#' control_form_2: Kontroll. Noen variable er oppfølging av målinger i hovedskjema.
#'      Skjema kun i det norske registeret
#' @param valgtVar Valg av variabel angir hvilket skjema som skal knyttes til hovedskjema
#'        siden de fire første bokstavene i variabelnavnet utgjør et prefiks for
#'        å identifisere hvilken tabell variabelen skal hentes fraaktuell tabell. Eks LivsAlder
#'        Variabler uten prefiks hentes fra hovedtabellen (Main...)
#' @param register Hvilket register det skal hentes data for: 'norscir' (standard) eller 'nordicscir'
#' @param koblSkjema Hvilket skjema skal regnes som "hovedskjema". Standard: 'Hoved' (spinal_cord_injury_core_data_set_1),
#'  'Kont' (control_form_2). Bare NorScir har kontrollskjema
#' @return RegData data frame
#' @export

NSRegDataSQL <- function(valgtVar='Alder', register='norscir', koblSkjema = 'Hoved', ...) {

  #HovedSkjema: spinal_cord_injury_core_data_set_1
  #Livs: registration_of_quality_of_life_3
  #Urin: lower_urinary_tract_function_4
  #Tarm: bowel_function_5
  #Sati: activities_and_participation_satisfaction_7 (bare NorScir)
  #Perf: activities_and_participation_performance_6 (bare NorScir)
  #Kont: control_form_2 (bare NorScir)
  #Eq5d: eq_5d_5l_9 (bare NorScir)

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = "Starter SQL-funksjon")
  }

  valgtSkjema <- substr(valgtVar,1,4)

  try(if(register == 'nordicscir' & koblSkjema == 'Kont') stop("NordicScir har ikke koblingsskjema"))
  try(if(koblSkjema == 'Kont' & valgtSkjema=='Kont') stop("Ingen vits i å koble kontrollskjema til kontrollskjema!"))

  varHoved <- c("
      Hoved.AAis,
      Hoved.ABMI,
      Hoved.AdmitDt,
      Hoved.AdmitRehDt,
      Hoved.AMtrLvlAreaL,
      Hoved.AMtrLvlAreaR,
      Hoved.ANeuExmDt,
      Hoved.ANeuNoMeasure,
      Hoved.ASensLvlAreaL,
      Hoved.ASensLvlAreaR,
      Hoved.ASensLvlLC,
      Hoved.BeforeRehDy,
      Hoved.DischgDt,
      Hoved.FAis,
      Hoved.FBMI,
      Hoved.FirstTimeClosed,
      Hoved.FMtrLvlAreaL,
      Hoved.FMtrLvlAreaR,
      Hoved.FNeuExmDt,
      Hoved.FNeuNoMeasure,
      Hoved.FSensLvlAreaL,
      Hoved.FSensLvlAreaR,
      Hoved.HealthUnitShortName,
      Hoved.HosptlDy,
      Hoved.InjuryDateUnknown,
      Hoved.InjuryDt,
      Hoved.Ntsci,
      Hoved.OutOfHosptlDy,
      Hoved.OutOfHosptlDy2,
      Hoved.OutOfRehabDy,
      Hoved.PatientAge,
      Hoved.PatientGender,
      Hoved.PlaceDis,
      Hoved.PPlacedis,
      Hoved.RehabDy,
      Hoved.RecCtrl,
      Hoved.Scietiol,
      Hoved.SpnlSurg2,
      Hoved.VentAssi2,
      -- Komplikasjoner:
      Hoved.PressureUlcer,
      Hoved.VTE,
      Hoved.UTI,
      Hoved.Sepsis,
      Hoved.Pneumonia,
      Hoved.Spasticity,
      Hoved.Syringomyelia,
      Hoved.HeterotopicOssification,
      Hoved.AutonomicDysreflexia,
      Hoved.OrthostaticHypotension,
      Hoved.Osteoporosis,
      Hoved.ComplicOther,
      Hoved.ComplicNone,
      -- UPPER(Hoved.SkjemaGUID) AS SkjemaGUIDHoved,
      Hoved.SkjemaGUID AS SkjemaGUIDHoved,
      Hoved.UnitId
")
  #Hoved.SkjemaGUID,
  #Hoved.PasientGUID,
  #Hoved.UnitId AS ReshId,

  varLivs <- c("
    Livs.FormDate
    -- ,UPPER(Livs.HovedskjemaGUID) AS HovedskjemaGUID
    ,Livs.HovedskjemaGUID
    ,Livs.QolDt
    ,Livs.SatGenrl
    ,Livs.SatPhys
    ,Livs.SatPsych
    ,Livs.SatSocIf
    -- ,Livs.SkjemaGUID AS SkjemaGUID
    -- ,Livs.SkjemaGUID AS SkjemaGUIDLivs
")
  #,Livs.PasientGUID

  varFunk <- c('
Funk.HovedskjemaGUID
-- UPPER(Funk.HovedskjemaGUID) AS HovedskjemaGUID
,Funk.DataClDt
,Funk.Dreslbdy
,Funk.Feeding
,Funk.FirstTimeClosed
,Funk.Mobilmod
-- ,Funk.SkjemaGUID
-- ,Funk.SkjemaGUID AS SkjemaGUIDFunk
,Funk.Toiletin
')

  varUrin <- c("
Urin.HovedskjemaGUID
-- UPPER(Urin.HovedskjemaGUID) AS HovedskjemaGUID
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
 Tarm.HovedskjemaGUID
 -- UPPER(Tarm.HovedskjemaGUID) AS HovedskjemaGUID
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


  varEQ5D <- c('
Eq5d.FormDate
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
,Eq5d.HovedskjemaGUID
-- ,UPPER(Eq5d.HovedskjemaGUID) AS HovedskjemaGUID
')

  varKont <- c('
Kont.CAis
,Kont.CAutonomicDysreflexia
,Kont.CHeterotopicOssification
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
,Kont.CComplicNone
,Kont.CComplicOther
,Kont.ControlInterruptedReason
,Kont.ControlPerformed
,Kont.ControlStatus
,Kont.COrthostaticHypotension
,Kont.COsteoporosis
,Kont.CPlaceDis
,Kont.CPneumonia
,Kont.CPressureUlcer
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
,Kont.CSepsis
,Kont.CSpasticity
,Kont.CSyringomyelia
,Kont.CUTI
,Kont.CVentAssi
,Kont.CVTE
,Kont.CreationDate AS KontCreationDate
,Kont.FirstTimeClosed
,Kont.FormDate
,Kont.FormStatus
,Kont.FormTypeId
,Kont.LastUpdate
,Kont.PatientAge AS PatientAgeKont
,Kont.ProceedingID
-- ,Kont.SkjemaGUID
,Kont.SkjemaGUID AS SkjemaGUIDKont
,Kont.UnitId
')
  #"HealthUnitId","HealthUnitName","HealthUnitShortName","HF" ,"Hospital","RHF"

  variable <- ''
  qSkjema <- ''

  if (valgtSkjema %in% c('Livs', 'Urin', 'Tarm', 'Funk', 'Eq5d', 'Kont')) { # 'Tilf'
    variable <- switch(valgtSkjema,
                       Livs = varLivs,
                       Urin = varUrin,
                       Tarm = varTarm,
                       Funk = varFunk,
                       #Tilf = varTilf,
                       Eq5d = varEQ5D,
                       Kont = varKont)
    variable <- paste0(', ', variable)
    qSkjema <- paste0(switch(valgtSkjema, #Dette vil bare fungere hvis konsekvent med navngiving i valgtVar
                             Livs = 'INNER JOIN registration_of_quality_of_life_3 Livs ',
                             Urin = 'INNER JOIN lower_urinary_tract_function_4 Urin ',
                             Tarm = 'INNER JOIN bowel_function_5 Tarm ',
                             Funk = 'INNER JOIN activities_and_participation_performance_6 Funk ',
                             Eq5d = 'INNER JOIN eq_5d_5l_9 Eq5d ',
                             Kont = 'INNER JOIN control_form_2 Kont '
    ),
    'ON ',koblSkjema ,'.SkjemaGUID = ',valgtSkjema , '.HovedskjemaGUID ')
  }
  #KontData <- rapbase::loadRegData(registryNam register, query='select * from control_form_2', dbType="mysql")
  #TilfData <-  rapbase::loadRegData(registryName = register, query='select * from activities_and_participation_satisfaction_7', dbType="mysql")
  #HovedSkjema <- rapbase::loadRegData(registryName = register, query='select * from spinal_cord_injury_core_data_set_1', dbType="mysql")
  #LivsSkjema <- rapbase::loadRegData(registryName = register, query='select * from registration_of_quality_of_life_3', dbType="mysql")

  if (koblSkjema=='Hoved'){
    query <- paste0('SELECT ',
                    varHoved,
                    variable,
                    ' FROM spinal_cord_injury_core_data_set_1 Hoved ',
                    qSkjema)
  }
  if (koblSkjema=='Kont'){
    query <- paste0('SELECT ',
                    varKont,
                    variable,
                    ' FROM control_form_2 Kont ',
                    qSkjema
    )
  }

  RegData <- rapbase::loadRegData(registryName = 'data', query = query, dbType="mysql")

  if (valgtSkjema=='Kont' | koblSkjema=='Kont'){
    RegData <- RegData[!(is.na(RegData$CNum) & RegData$ControlStatus==0), ]
  # RegData <- RegData[RegData$ControlStatus==0, ] #Bare de med gjennomført kontroll, 0-Aktiv, 1-Avbrutt
}

  if (valgtSkjema=='Tilf') {
    #RegData er nå Hovedskjema eller Kontrollskjema
    qTilf <- 'SELECT HovedskjemaGUID AS FunkskjemaGUID,
                    DataClDtS, DreslbdyS, FeedingS, FirstTimeClosed, MobilmodS, ToiletinS
              FROM activities_and_participation_satisfaction_7'
    TilfData <- rapbase::loadRegData(registryName = 'data', query = qTilf, dbType = "mysql")

    qFunkTilf <- 'SELECT HovedskjemaGUID, SkjemaGUID AS FunkskjemaGUID FROM
                          activities_and_participation_performance_6'
    FunkVarKobl <- rapbase::loadRegData(registryName = 'data', query = qFunkTilf, dbType = "mysql")

    FunkTilf <- FunkVarKobl %>%
      dplyr::inner_join(TilfData, by = dplyr::join_by(FunkskjemaGUID))

    RegData$SkjemaGUID <- RegData[ , paste0('SkjemaGUID', koblSkjema)]
    RegData <- RegData %>%
      dplyr::inner_join(FunkTilf, by = dplyr::join_by(SkjemaGUID == HovedskjemaGUID))
  }

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                       msg = paste0('Har hentet skjema ', valgtSkjema, 'fra database'))
  }

  return(RegData)
}
