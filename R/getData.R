#' Get Data for NordicScir
#'
#' Functions to obtain and process data for NordicScir, both real and fake.
#'
#' Detajer
#'
#' @param data A data object to be processed by the function.
#' @param register Hvilket register det skal hentes data for
#'        'norscir' (standard) eller 'nordicscir'
#' @return A list of data objects (data frames).  If not successful in query for
#' real data, a warning is issued and NULL will be will be returned.
#' @aliases getRealData getFakeData processAllData
#' @name getData
NULL

#' @rdname getData
#' @export
getRealData <- function(register='norscir', ...) {

   tryCatch({
    HovedSkjema <- NSRegDataSQL(register=register)
    LivskvalH <- NSRegDataSQL(register=register, valgtVar = "LivsXX")
    UrinH <- NSRegDataSQL(register=register, valgtVar = "UrinXX")
    TarmH <- NSRegDataSQL(register=register, valgtVar = "TarmXX")
    data <- list(
      HovedSkjema = HovedSkjema,
      LivskvalH = LivskvalH,
      UrinH = UrinH,
      TarmH = TarmH)

    if (register=='norscir'){
      data <- append(
        data,
        list(EQ5DH = NSRegDataSQL(valgtVar = "Eq5dXX"),
             AktivFunksjonH = NSRegDataSQL(valgtVar = "FunkXX"),
             AktivTilfredshetH = NSRegDataSQL(valgtVar = "TilfXX"),
             KontrollH = NSRegDataSQL(valgtVar = "KontXX"),
             LivskvalK = NSRegDataSQL(valgtVar = "LivsXX", koblSkjema = 'Kont'),
             UrinK = NSRegDataSQL(valgtVar = "UrinXX", koblSkjema = 'Kont'),
             TarmK = NSRegDataSQL(valgtVar = "TarmXX", koblSkjema = 'Kont'),
             EQ5DK = NSRegDataSQL(valgtVar = "Eq5dXX", koblSkjema = 'Kont'),
             AktivFunksjonK = NSRegDataSQL(valgtVar = "FunkXX", koblSkjema = 'Kont'),
             AktivTilfredshetK = NSRegDataSQL(valgtVar = "TilfXX", koblSkjema = 'Kont')
             ))
      }

    if ("session" %in% names(list(...))) {
      rapbase::repLogger(session = list(...)[["session"]],
                        msg = 'Har hentet alle data fra database')
    }

    return(data)},
    error = function(e) {
      warning(paste("Could not get real data:", e))
      return(NULL)}
  )
}


#' @rdname getData
#' @export
getFakeData <- function(register = 'norscir') { #Denne må muligens tilpasses nordiske data
#Har foreløpbig bare norske, fiktive data.

  data('NordicScirFIKTIVEdata', package = 'nordicscir', envir = environment())

  Livskval$HovedskjemaGUID <- toupper(Livskval$HovedskjemaGUID)
  Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
  Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)

  LivskvalH <- KobleMedHoved(HovedSkjema, Livskval)
  KontrollH <- KobleMedHoved(HovedSkjema, Kontroll)
  UrinH <- KobleMedHoved(HovedSkjema, Urin)
  TarmH <- KobleMedHoved(HovedSkjema, Tarm)

  if (register == 'norscir') {
    Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
    AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
    AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)

    AktivFunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
    Aktivitet <- KobleMedHoved(
      HovedSkjema = AktivFunksjon, Skjema2 = AktivTilfredshet
    )
    AktivTilfredshetH <- KobleMedHoved(HovedSkjema, Aktivitet)
  }

  Skjemaer <- list(
    HovedSkjema = HovedSkjema,
    LivskvalH = LivskvalH,
    KontrollH = KontrollH,
    UrinH = UrinH,
    TarmH = TarmH)

  if (register =='norscir'){
    Aktiv <- list(
      AktivFunksjonH = AktivFunksjonH,
      AktivTilfredshetH = AktivTilfredshetH)
    Skjemaer <- append(Skjemaer, Aktiv)
  }
}


#' @rdname getData
#' @export
processAllData <- function(data, register = 'norscir', ...) {
  # Prosesserer data som er koblet med hovedskjema, dvs. ikke de som er koblet bare med kontrollskjema
  #Vurder å koble på hovedskjema til disse kontrollskjemaene også.

  if ("session" %in% names(list(...))) {
    rapbase::repLogger(session = list(...)[["session"]],
                      msg = 'Starter prosessering av data')
  }
  tryCatch({
    HovedSkjema <- NSPreprosesser(data$HovedSkjema)
    LivskvalH <- NSPreprosesser(data$LivskvalH)
    UrinH <- NSPreprosesser(data$UrinH)
    TarmH <- NSPreprosesser(data$TarmH)

    Skjemaer <- list(
      HovedSkjema = HovedSkjema,
      LivskvalH = LivskvalH,
      UrinH = UrinH,
      TarmH = TarmH)

    if (register == 'norscir'){
      NorskeSkjemaer <- list(
        EQ5DH = NSPreprosesser(data$EQ5DH),
        KontrollH = NSPreprosesser(data$KontrollH),
        AktivFunksjonH = NSPreprosesser(data$AktivFunksjonH),
        AktivTilfredshetH = NSPreprosesser(data$AktivTilfredshetH),
        LivskvalK = data$LivskvalK,
        UrinK = data$UrinK,
        TarmK = data$TarmK,
        EQ5DK = data$EQ5DK,
        AktivFunksjonK = data$AktivFunksjonK,
        AktivTilfredshetK = data$AktivTilfredshetK
        )

      Skjemaer <- append(Skjemaer, NorskeSkjemaer)

if ("session" %in% names(list(...))) {
  rapbase::repLogger(session = list(...)[["session"]],
                    msg = paste0('Har prosessert kontroll og aktivitetstabeller'))
}
    }

    return(Skjemaer)

  }, error = function(e) {
    warning("Something went wrong processing the data:", e)
    return(NULL)
  })
}
