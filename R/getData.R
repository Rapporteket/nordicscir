#' Get Data for NordicScir
#'
#' Functions to obtain and process data for NordicScir, both real and fake.
#'
#' @param data A data object to be processed by the function.
#' @return A list of data objects (data frames).  If not successful in query for
#' real data, a warning is issued and NULL will be will be returned.
#' @aliases getRealData getFakeData processData
#' @name getData
NULL

#' @rdname getData
#' @export
getRealData <- function() {

  tryCatch({
    HovedSkjema <- NSRegDataSQL()
    LivskvalH <- NSRegDataSQL(valgtVar = "LivsXX")
    KontrollH <- NSRegDataSQL(valgtVar = "KontXX")
    UrinH <- NSRegDataSQL(valgtVar = "UrinXX")
    TarmH <- NSRegDataSQL(valgtVar = "TarmXX")
    AktivFunksjonH <- NSRegDataSQL(valgtVar = "FunkXX")
    AktivTilfredshetH <- NSRegDataSQL(valgtVar = "TilfXX")
    return(
      list(
        HovedSkjema = HovedSkjema,
        LivskvalH = LivskvalH,
        KontrollH = KontrollH,
        UrinH = UrinH,
        TarmH = TarmH,
        AktivFunksjonH = AktivFunksjonH,
        AktivTilfredshetH = AktivTilfredshetH
      )
    )
  },
  error = function(e) {
    warning(paste("Could not get real data:", e))
    return(NULL)
  })
}


#' @rdname getData
#' @export
getFakeData <- function() {

  data('NordicScirFIKTIVEdata', package = 'nordicscir', envir = environment())

  Livskval$HovedskjemaGUID <- toupper(Livskval$HovedskjemaGUID)
  Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
  Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
  Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
  AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
  AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)

  LivskvalH <- KobleMedHoved(HovedSkjema, Livskval)
  KontrollH <- KobleMedHoved(HovedSkjema, Kontroll)
  UrinH <- KobleMedHoved(HovedSkjema, Urin)
  TarmH <- KobleMedHoved(HovedSkjema, Tarm)
  AktivFunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
  Aktivitet <- KobleMedHoved(
    HovedSkjema = AktivFunksjon, Skjema2 = AktivTilfredshet
  )
  AktivTilfredshetH <- KobleMedHoved(HovedSkjema, Aktivitet)

  list(
    HovedSkjema = HovedSkjema,
    LivskvalH = LivskvalH,
    KontrollH = KontrollH,
    UrinH = UrinH,
    TarmH = TarmH,
    AktivFunksjonH = AktivFunksjonH,
    AktivTilfredshetH = AktivTilfredshetH
  )
}


#' @rdname getData
#' @export
processData <- function(data) {

  HovedSkjema <- NSPreprosesser(data$HovedSkjema)
  LivskvalH <- NSPreprosesser(data$LivskvalH)
  KontrollH <- NSPreprosesser(data$KontrollH)
  UrinH <- NSPreprosesser(data$UrinH)
  TarmH <- NSPreprosesser(data$TarmH)
  AktivFunksjonH <- NSPreprosesser(data$AktivFunksjonH)
  AktivTilfredshetH <- NSPreprosesser(data$AktivTilfredshetH)

  list(
    HovedSkjema = HovedSkjema,
    LivskvalH = LivskvalH,
    KontrollH = KontrollH,
    UrinH = UrinH,
    TarmH = TarmH,
    AktivFunksjonH = AktivFunksjonH,
    AktivTilfredshetH = AktivTilfredshetH
  )
}