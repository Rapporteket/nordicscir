# Resultattjeneste for NordicScir

#' Brukergrensesnitt (ui) til nordscir-appen
#'
#' @return Brukergrensesnittet (ui) til nordscir-appen
#' @export
ui_nordicscir <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  startDato <- as.Date(
    paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01')
  )

  context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  regTitle = "Nordisk ryggmargsskaderegister"

  #----Valg

  valgAIS <- 0:5
  names(valgAIS) <- c("Alle", "A", "B", "C", "D", "E")

  valgNivaaUt <- c(99, 0, 1, 2, 3, 9)
  names(valgNivaaUt) <- c("Alle", "Paraplegi", "Tetraplegi", "C1-4", "C5-8",
                          "Ukjent")

  tidsenheter <- rev(
    c("År" = "Aar", "Halvår" = "Halvaar", "Kvartal" = "Kvartal",
      "Måned" = "Mnd")
  )

  enhetsUtvalg <- 0:5
  names(enhetsUtvalg) <- c("Hele Norden",
                           "Egen enhet mot alle andre",
                           "Egen enhet",
                           "Egen enhet mot eget land forøvrig",
                           "Eget land",
                           "Eget land mot Norden forøvrig")

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::navbarPage(
      id = "hovedark",
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        regTitle),
      # sett inn tittel også i browser-vindu
      windowTitle = regTitle,
      # velg css (foreløpig den eneste bortsett fra "naken" utgave)
      theme = "rap/bootstrap.css",

      #----startside--------
      shiny::tabPanel(
        "Startside",
        shiny::br(),
        shiny::tags$head(
          shiny::tags$style(
            ".butt{background-color:#6baed6;} .butt{color: white;}"
          )
        ),
        shiny::sidebarPanel(
          width = 3,
          shiny::conditionalPanel(
            condition = "input.startside == 'Status'",
            shiny::h4(
              "Velg tidsperiode for nevrologisk klassifikasjon og liggetider"
            ),
            shiny::dateRangeInput(
              inputId = "datovalgDash", start = startDato, end = Sys.Date(),
              label = "Tidsperiode", separator = "t.o.m.", language = "nb")
          )
        ),
        shiny::mainPanel(
          width = 8,
          if (context %in% c("DEV", "TEST", "QA", "PRODUCTION", "QAC", "PRODUCTIONC")) {
            rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
          },
          shiny::h2("Velkommen til Rapporteket - Nordisk Ryggmargsskaderegister!",
                    align='center'),
          shiny::br(),
          shiny::tabsetPanel(
            id = "startside",
            shiny::tabPanel(
              "Brukerveiledning",
              shiny::htmlOutput("guide", inline = TRUE)
            ),
            shiny::tabPanel(
              "Status",
              shiny::h4("Antall registreringer siste år:"),
              shiny::tableOutput("tabAntOpphShMnd12startside"),
              shiny::h5(paste("(Mer informasjon om registreringsstatus finner",
                              "du under fanen 'Registreringsoversikter')")),
              shiny::br(),
              shiny::br(),
              shiny::fluidRow(
                shiny::h3("Liggetider, egen avdeling", align = "left"),
                shiny::tableOutput("tabLiggetider")
              ),
              shiny::fluidRow(
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("alle pasienter", align = "center"),
                  #shiny::br(),
                  shiny::tableOutput("tabNevrKlass")),
                shiny::fluidRow(
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("pasienter med liggetid over 28 dager i
                                   ryggmargsskadeavdeling", align = "center"),
                  shiny::tableOutput("tabNevrKlass28")
                )
              )
          )
        ) #main
      ), #tab

      #--------Fordelinger-----------
      shiny::tabPanel(
        "Fordelinger",
        shiny::sidebarPanel(
          width = 3,
          shiny::selectInput(
            inputId = "valgtVar",
            label="Velg variabel",
            choices = c(
              "Alder" = "Alder",
              "Ais ved innleggelse" = "AAis" ,
              "Ais ved utskriving" = "FAis",
             # "Anbefalt tid til kontroll" = "AnbefTidKtr",
              "Lengde på rehab.opphold" = "DagerRehab",
              "Opphold, totalt antall dager" = "OpphTot",
              "Planlagt utskrevet til" = "PPlaceDis",
              "Registreringsforsinkelse" = "RegForsinkelse",
              "Skadeårsak " = "SkadeArsak",
              "Skadeårsak, ikke-traumatisk" = "Ntsci",
              "Tid fra skade til oppstart rehab." = "DagerTilRehab",
              "Tid med rehabilitering" = "DagerRehab",
              "Utskrevet til" = "UtTil",
              "Livskval.: Tilfredshet med livet" = "LivsGen",
              "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
              "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk",
              "Urin: Ufrivillig urinlekkasje (fra 2019)" = "UrinInkontinens",
            #  "Urin: Ufrivillig urinlekkasje (t.o.m. 2018)" = "UrinInkontinensTom2018",
              "Urin: Kirurgiske inngrep" = "UrinKirInngr",
              "Urin: Legemiddelbruk (fra 2019)" = "UrinLegemidler",
             # "Urin: Legemiddelbruk (t.o.m. 2018)" = "UrinLegemidlerTom2018",
              "Urin: Legemiddelbruk, hvilke" = "UrinLegemidlerHvilke",
              "Urin: Blæretømming, hovedmetode" = "UrinTomBlareHoved",
              "Urin: Blæretømming, tilleggsmetode" = "UrinTomBlareTillegg",
              "Tarm: Avføring, hovedmetode" = "TarmAvfHoved",
              "Tarm: Avføring, tilleggsmetode" = "TarmAvfTillegg",
              "Tarm: Avføringsmiddelbruk" = "TarmAvfmiddel",
              "Tarm: Avføringsmidler, hvilke" = "TarmAvfmiddelHvilke",
              "Tarm: Fekal inkontinens (fra 2019)" = "TarmInkontinensFra2019",
            #  "Tarm: Fekal inkontinens (t.o.m. 2018)" = "TarmInkontinensTom2018",
              "Tarm: Kirurgisk inngrep" = "TarmKirInngrep",
              "Tarm: Kirurgiske inngrep, hvilke" = "TarmKirInngrepHvilke",
            "Tarm: NBD" = "TarmNBD"
            ),
            selected = c("Registreringsforsinkelse" = "RegForsinkelse")
          ),
          shiny::dateRangeInput(
            inputId = "datovalg",
            start = startDato,
            end = Sys.Date(),
            label = "Tidsperiode",
            separator="t.o.m.",
            language="nb"
          ),
          shiny::radioButtons(
            inputId = "datoUt",
            label = "Bruk utskrivingsdato til datofiltrering?",
            choiceNames = c("nei", "ja"),
            choiceValues = 0:1,
            selected = 0
          ),
          shiny::selectInput(
            inputId = "erMann",
            label = "Kjønn",
            choices = c("Begge" = 2, "Menn" = 1, "Kvinner" = 0)
          ),
          shiny::sliderInput(
            inputId = "alder",
            label = "Alder",
            min = 0,
            max = 110,
            value = c(0, 110)
          ),


          shiny::conditionalPanel(
            condition = "input.fordeling == 'Figur' | input.fordeling == 'Tabell' ",
            shiny::selectInput(
              inputId = "enhetsUtvalg",
              label = "Egen enhet og/eller landet",
              choices = enhetsUtvalg,
              selected = 1)
            ),

          shiny::selectInput(
            inputId = "AIS",
            label = "AIS-grad ved utreise",
            multiple = T, #selected=0,
            choices = valgAIS
          ),
          shiny::selectInput(
            inputId = "traume",
            label="Traume",
            choices = c("Alle" = " ", #"ikke"
                        "Traume" = "ja",
                        "Ikke traume" = "nei")
          ),
          shiny::selectInput(
            inputId = "nivaaUt",
            label = "Nivå ved utreise",
            choices = valgNivaaUt
          ),
          shiny::selectInput(
            inputId = "bildeformatFord",
            label = "Velg format for nedlasting av figur",
            choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
          )
        ),
        shiny::mainPanel(
          width = 6,
          shiny::tabsetPanel(
            id="fordeling",
            shiny::tabPanel(
              "Figur",
              shiny::br(),
              em("(Høyreklikk på figuren for å laste den ned)"),
              shiny::br(),
              shiny::br(),
              shiny::plotOutput("fordelinger", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figFord", label="Last ned figur"
              ),
              hr()
            ),
            shiny::tabPanel(
              "Figur, alle sykehus",
              shiny::plotOutput("fordelingPrSh", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figFordSh", label = "Last ned figur")
            ),
            shiny::tabPanel(
              "Tabell",
              shiny::uiOutput("tittelFord"),
              shiny::br(),
              shiny::tableOutput("fordelingTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabFord", label = "Last ned tabell"
              ),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::br(),
              shiny::tableOutput("fordelingShTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabFordSh", label="Last ned"
              )
            )
          )
        ) #mainPanel
      ), #tab Fordelinger


      #------------ Gjennomsnitt ------------
      # shiny::tabPanel(
      #   "Gjennomsnitt per sykehus og over tid",
      #   shiny::sidebarPanel(
      #     width = 3,
      #     shiny::selectInput(
      #       inputId = "valgtVarGjsn",
      #       label="Velg variabel",
      #       choices = c("Alder" = "Alder",
      #                   "Lengde på rehab.opphold" = "DagerRehab",
      #                   "Opphold, totalt antall dager" = "OpphTot",
      #                   "Registreringsforsinkelse" = "RegForsinkelse",
      #                   "Tid fra skade til oppstart rehab." = "DagerTilRehab",
      #                   "Livskval.: Tilfredshet med livet" = "LivsGen",
      #                   "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
      #                   "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk"
      #       ),
      #       selected = c("Registreringsforsinkelse" = "RegForsinkelse")
      #     ),
      #     shiny::dateRangeInput(
      #       inputId = "datovalgGjsn",
      #       start = startDato,
      #       end = Sys.Date(),
      #       label = "Tidsperiode", separator="t.o.m.", language="nb"
      #     ),
      #     shiny::radioButtons(
      #       inputId = "datoUtGjsn",
      #       "Bruk utskrivingsdato til datofiltrering?",
      #       choiceNames = c("nei", "ja"),
      #       choiceValues = 0:1,
      #       selected = 0
      #     ),
      #     shiny::selectInput(inputId = "erMannGjsn",
      #                        label="Kjønn",
      #                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
      #     ),
      #     shiny::sliderInput(
      #       inputId="alderGjsn",
      #       label = "Alder",
      #       min = 0,
      #       max = 110,
      #       value = c(0, 110)
      #     ),
      #     shiny::selectInput(
      #       inputId = "AISGjsn",
      #       label="AIS-grad ved utreise",
      #       multiple = TRUE, #selected=0,
      #       choices = valgAIS
      #     ),
      #     shiny::selectInput(
      #       inputId = "traumeGjsn",
      #       label="Traume",
      #       choices = c("Alle"=" ", #"ikke"
      #                   "Traume"="ja",
      #                   "Ikke traume"="nei")
      #     ),
      #     shiny::selectInput(
      #       inputId = "paratetraGjsn",
      #       label="Nivå ved utreise",
      #       choices = c("Alle" = 99,
      #                   "Paraplegi" = 0,
      #                   "Tetraplegi" = 1,
      #                   "Ukjent" = 9)
      #     ),
      #     shiny::selectInput(
      #       inputId = "sentralmaal",
      #       label="Velg gjennomsnitt/median ",
      #       choices = c("Gjennomsnitt"="gjsn", "Median"="med")
      #     ),
      #     shiny::br(),
      #     shiny::p(
      #       shiny::em(paste("Følgende utvalg gjelder bare figuren/tabellen som",
      #                       "viser utvikling over tid"))
      #     ),
      #     shiny::selectInput(
      #       inputId = "enhetsUtvalgGjsn",
      #       label="Egen enhet og/eller landet",
      #       choices = enhetsUtvalg,
      #       selected = 1
      #     ),
      #     shiny::selectInput(
      #       inputId = "tidsenhetGjsn",
      #       label = "Velg tidsenhet",
      #       choices = tidsenheter
      #     ),
      #     shiny::selectInput(
      #       inputId = "bildeformatGjsn",
      #       label = "Velg format for nedlasting av figur",
      #       choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
      #     )
      #   ),
      #   shiny::mainPanel(
      #     shiny::tabsetPanel(
      #       shiny::tabPanel(
      #         "Figur",
      #         shiny::br(),
      #         shiny::h3(shiny::em("Utvikling over tid")),
      #         shiny::plotOutput("gjsnTid", height = "auto"),
      #         shiny::downloadButton(
      #           outputId = "lastNed_figGjsnTid", label="Last ned figur"
      #         ),
      #         shiny::br(),
      #         shiny::h3(em("Sykehusvise resultater")),
      #         shiny::plotOutput("gjsnGrVar", height = "auto"),
      #         shiny::downloadButton(
      #           outputId = "lastNed_figGjsnGrVar", label = "Last ned figur")
      #       ),
      #       shiny::tabPanel(
      #         "Tabell",
      #         shiny::uiOutput("tittelGjsnGrVar"),
      #         shiny::br(),
      #         shiny::tableOutput("gjsnTidTab"),
      #         shiny::br(),
      #         shiny::tableOutput("gjsnGrVarTab"),
      #         shiny::downloadButton(
      #           outputId = "lastNed_tabGjsnGrVar", label = "Last ned"
      #         )
      #       )
      #     )
      #   )
      # ), #Gjsn


      #-----Registreringsoversikter------------
      # shiny::tabPanel(
      #   "Registreringsoversikter",
      #   shiny::sidebarPanel(
      #     width=3,
      #     shiny::h3("Utvalg"),
      #     shiny::conditionalPanel(
      #       condition = "input.ark == 'Antall personer med ryggmargsskade'",
      #       shiny::dateInput(
      #         inputId = "sluttDatoReg",
      #         label = "Velg sluttdato",
      #         language="nb",
      #         value = Sys.Date(),
      #         max = Sys.Date()
      #       )
      #     ),
      #     shiny::conditionalPanel(
      #       condition = "input.ark == 'Antall personer med ryggmargsskade'",
      #       shiny::selectInput(
      #         inputId = "tidsenhetReg",
      #         label="Velg tidsenhet",
      #         choices = rev(c("År"= "Aar", "Måned"="Mnd"))
      #       )
      #     ),
      #     shiny::conditionalPanel(
      #       condition = "input.ark == 'Antall personer med ryggmargsskade'",
      #       shiny::selectInput(
      #         inputId = "traumeReg",
      #         label="Traume",
      #         choices = c("Alle"=" ", #"ikke"
      #                     "Traume"="ja",
      #                     "Ikke traume"="nei"))
      #     ),
      #     shiny::conditionalPanel(
      #       condition = paste0(
      #         "input.ark == 'Antall hovedskjema med tilknyttede skjema' "
      #       ),
      #       shiny::dateRangeInput(
      #         inputId = "datovalgReg",
      #         start = startDato,
      #         end = Sys.Date(),
      #         label = "Tidsperiode",
      #         separator="t.o.m.",
      #         language="nb"
      #       ),
      #       h5('Tidsperioden er basert på innleggelsesdato')
      #     )
      #   ),
      #
      #   shiny::mainPanel(
      #     shiny::tabsetPanel(
      #       id = "ark",
      #       shiny::tabPanel(
      #         "Antall personer med ryggmargsskade",
      #         shiny::uiOutput("undertittelReg"),
      #         shiny::p(paste("Velg tidsperiode ved å velge sluttdato/tidsenhet",
      #                        "i menyen til venstre")),
      #         shiny::br(),
      #         shiny::fluidRow(
      #           shiny::tableOutput("tabAntOpphShMnd12"),
      #           shiny::downloadButton(
      #             outputId = "lastNed_tabAntOpph", label="Last ned"
      #           )
      #         )
      #       ),
      #       shiny::tabPanel(
      #         "Antall hovedskjema med tilknyttede skjema",
      #         shiny::h3("Antall hovedskjema med tilknyttede skjema"),
      #         shiny::tableOutput("tabAntTilknyttedeHovedSkjema"),
      #         shiny::downloadButton(
      #           outputId = "lastNed_tabOppfHovedAnt", label = "Last ned"
      #         ),
      #         shiny::br(),
      #         shiny::h3("Andel (%) hovedskjema med tilknyttede skjema"),
      #         shiny::tableOutput("tabAndelTilknyttedeHovedSkjema"),
      #         shiny::downloadButton(
      #           outputId = "lastNed_tabOppfHovedPst", label="Last ned"
      #         )
      #       )
      #       #,
      #       # shiny::tabPanel(
      #       #   "Antall kontrollskjema med tilknyttede skjema",
      #       #   shiny::h3("Antall kontrollskjema med tilknyttede skjema"),
      #       #   shiny::h5("Datoutvalg er basert på dato for kontroll"),
      #       #   shiny::tableOutput("tabAntTilknyttedeKtrSkjema"),
      #       #   shiny::downloadButton(
      #       #     outputId = "lastNed_tabOppfKtrAnt", label="Last ned"
      #       #   ),
      #       #   shiny::br(),
      #       #   shiny::h3("Andel (%) kontrollskjema med tilknyttede skjema"),
      #       #   shiny::tableOutput("tabAndelTilknyttedeKtrSkjema"),
      #       #   shiny::downloadButton(
      #       #     outputId = "lastNed_tabOppfKtrPst", label="Last ned"
      #       #   )
      #       # )
      #     )
      #   )
      # ), #tab Registreringsoversikter

      #----------------------Registeradministrasjon-----------------------------

      shiny::tabPanel(
        "Registeradministrasjon",
        shiny::h2("Fane som bare er synlig for SC-bruker."),
        h3('NB: Samlerapporten er ikke tilpasset nordiske data'),
        h3('Månedsrapporten er tilpasset nordiske data'),

        shiny::tabsetPanel(
          id = "ark",

          shiny::tabPanel(
            "Utsendinger",
            title = "Utsending av rapporter"
#            shiny::sidebarLayout(
#              shiny::sidebarPanel(
#                rapbase::autoReportOrgInput("NSuts"),
#                rapbase::autoReportInput("NSuts")
#              ),
#              shiny::mainPanel(
#                rapbase::autoReportUI("NSuts")
#              )
#            )
          ),
          shiny::tabPanel(
            "Eksport, krypterte data",
            shiny::sidebarPanel(
              rapbase::exportUCInput("nordicscirExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("nordicscirExportGuide")
            )
          ) #Eksport-tab
        ) #tabsetPanel
      ) #Registeradm-tab

    ) #navbar
  ) #tagList
}


#' Server-del til appen
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return Server-delen til nordicscir-appen
#' @export
server_nordicscir <- function(input, output, session) {
#print(session)
  rapbase::appLogger(
    session = session,
    msg = "Starter nordicscir-app'en"
  )

  context <- Sys.getenv("R_RAP_INSTANCE")
  if (context == 'QAC'){
  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "nordicscir",
    caller = "nordicscir"
  )
  }

  # session persistent objects
  # if (rapbase::isRapContext()) {
    reshIDHelreg <- as.numeric(rapbase::getUserReshId(session))
    rolleHelreg <- rapbase::getUserRole(session)
    brukernavnHelreg <- rapbase::getUserName(session)
  # } else {
  #   reshID <- 0
  #   rolle <- 'ukjent'
  #   brukernavn <- 'ukjent'
  # }

  isGetDataOk <- TRUE
  isProcessDataOk <- TRUE
  AlleTab <- getRealData(register = 'nordicscir')
  if (is.null(AlleTab)) {
    warning("Not able to get real data. Applying fake data instead!")
    isGetDataOk <- FALSE
    AlleTab <- getFakeData()
  }
  AlleTab <- processAllData(AlleTab, register = 'nordicscir')
  if (is.null(AlleTab)) {
    warning("Not able to process data.")
    isProcessDataOk <- FALSE
  }
  isDataOk <- all(c(isGetDataOk, isProcessDataOk))
  attach(AlleTab)
  # enhet <- ifelse(exists('reshID'),
  #                 as.character(AlleTab$HovedSkjema$ShNavn[match(reshID, AlleTab$HovedSkjema$ReshId)]),
  #                 'Uidentifisert enhet')



  #--------------Startside------------------------------

  output$guide <- shiny::renderText(
    rapbase::renderRmd(
      system.file("brukerveiledningNordisk.Rmd", package = "nordicscir"),
      outputType = "html_fragment",
      params = list(isDataOk = isDataOk)
    )
  )
  if (isDataOk) {
    output$tabAntOpphShMnd12startside <-
      shiny::renderTable(
        tabAntOpphShMnd(RegData = HovedSkjema, antMnd = 12),
        rownames = T, digits=0, spacing="xs"
      )
  } else {
    output$tabAntOpphShMnd12startside <- NULL
  }
  observe({
    if (isDataOk) {
      output$tabNevrKlass <- shiny::renderTable(
        lagTabNevrKlass(
          HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        ),
        rownames = TRUE
      )
      output$tabNevrKlass28 <- shiny::renderTable({
        HovedSkjema28 <- HovedSkjema[which(HovedSkjema$DagerRehab > 28), ]
        lagTabNevrKlass(
          HovedSkjema28,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        )
      },
      rownames = TRUE
      )
      output$tabLiggetider <- shiny::renderTable({
        tabLiggetider(
          RegData = HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2],
          enhetsUtvalg=2,
          reshID=ifelse(context %in% c('QAC', 'PROCDUCTIONC', user$org(), reshIDHelreg))
        )
      },
      rownames = TRUE,
      digits = 0
      )
    } else {
      output$tabNevrKlass <- NULL
      output$tabNevrKlass28 <- NULL
      output$tabLiggetider <- NULL
    }
  })

  #----------Tabeller, registreringsoversikter ----------------------
  output$undertittelReg <- shiny::renderUI({
    shiny::br()
    t1 <- "Tabellen viser innleggelser "
    t2 <- ", basert på første akutte innleggelse"
    shiny::h4(shiny::HTML(
      switch(
        input$tidsenhetReg,
        Mnd = paste0(t1, "siste 12 måneder før ", input$sluttDatoReg, t2,
                     "<br />"),
        Aar = paste0(t1, "siste 5 år før ", input$sluttDatoReg, t2, "<br />")
      )
    ))
  })
  shiny::observe({
    if (isDataOk) {
      tabAntOpphShMndAar <-
        switch(
          input$tidsenhetReg,
          Mnd = tabAntOpphShMnd(
            RegData = HovedSkjema,
            datoTil = input$sluttDatoReg,
            traume = input$traumeReg,
            antMnd = 12
          ),
          Aar = tabAntOpphSh5Aar(
            RegData = HovedSkjema,
            datoTil = input$sluttDatoReg,
            traume = input$traumeReg
          )
        )

      output$tabAntOpphShMnd12 <- shiny::renderTable(
        tabAntOpphShMndAar, rownames = TRUE, digits = 0, spacing = "xs"
      )
      output$lastNed_tabAntOpph <- shiny::downloadHandler(
        filename = function() {paste0("tabAntOpph.csv")},
        content = function(file, filename) {
          write.csv2(tabAntOpphShMndAar, file, row.names = TRUE, na = "")
        }
      )

      #Antall skjema av alle typer.
      tabTilknHovedSkjema <- tabSkjemaTilknyttet(
        Data = AlleTab,
        moderSkjema = "Hoved",
        datoFra = input$datovalgReg[1],
        datoTil = input$datovalgReg[2]
      )

      #Hovedskjema som har tilknyttede skjema av ulik type
      output$tabAntTilknyttedeHovedSkjema <- shiny::renderTable(
        tabTilknHovedSkjema$Antall,
        rownames = TRUE,
        digits = 0,
        spacing = "xs"
      )

      output$lastNed_tabOppfHovedAnt <- shiny::downloadHandler(
        filename = function() {'tabOppfHovedAnt.csv'},
        content = function(file, filename) {
          write.csv2(
            tabTilknHovedSkjema$Antall, file, row.names = TRUE, na = ""
          )
        }
      )
      #Andel (prosent) av registreringsskjemaene som har oppfølgingsskjema.
      output$tabAndelTilknyttedeHovedSkjema <- shiny::renderTable(
        tabTilknHovedSkjema$Andeler,
        rownames = TRUE,
        digits = 0,
        spacing = "xs"
      )

      output$lastNed_tabOppfHovedPst <- shiny::downloadHandler(
        filename = function() {'tabOppfHovedPst.csv'},
        content = function(file, filename) {
          write.csv2(
            tabTilknHovedSkjema$Andeler, file, row.names = TRUE, na = ""
          )
        }
      )

    } else {
      output$tabAntOpphShMnd12 <- NULL
      output$lastNed_tabAntOpph <- NULL
      output$tabAntTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedAnt <- NULL
      output$tabAndelTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedPst <- NULL
    }
  })


  #---------Fordelinger:--fordelingsfigurer og tabeller----------
  shiny::observe({
    if (isDataOk) {
      RegData <- finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
      RegData <- TilLogiskeVar(RegData)

      output$fordelinger <- shiny::renderPlot({
        NSFigAndeler(
          RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          reshID = ifelse(context %in% c('QAC', 'PROCDUCTIONC', user$org(), reshIDHelreg)),
          AIS = as.numeric(input$AIS), traume = input$traume,
          nivaaUt = as.numeric(input$nivaaUt),
          minald = as.numeric(input$alder[1]),
          maxald = as.numeric(input$alder[2]),
          erMann = as.numeric(input$erMann),
          enhetsUtvalg = as.numeric(input$enhetsUtvalg),
          datoUt = as.numeric(input$datoUt),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figFord <- shiny::downloadHandler(
        filename = function() {
          paste0("Fordeling_", valgtVar = input$valgtVar, "_", Sys.time(), ".",
                 input$bildeformatFord)
        },
        content = function(file) {
          NSFigAndeler(
            RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt),
            reshID = ifelse(context %in% c('QAC', 'PROCDUCTIONC', user$org(), reshIDHelreg)),
            AIS = as.numeric(input$AIS), traume = input$traume,
            nivaaUt = as.numeric(input$nivaaUt),
            minald = as.numeric(input$alder[1]),
            maxald = as.numeric(input$alder[2]),
            erMann = as.numeric(input$erMann),
            enhetsUtvalg = as.numeric(input$enhetsUtvalg),
            session = session,
            outfile = file
          )
        }
      )

      UtDataFord <- NSFigAndeler(
        RegData = RegData, preprosess = 0, valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        reshID = ifelse(context %in% c('QAC', 'PROCDUCTIONC', user$org(), reshIDHelreg)),
        AIS = as.numeric(input$AIS), traume = input$traume,
        nivaaUt = as.numeric(input$nivaaUt),
        minald = as.numeric(input$alder[1]),
        maxald = as.numeric(input$alder[2]),
        erMann = as.numeric(input$erMann),
        enhetsUtvalg = as.numeric(input$enhetsUtvalg),
        session = session
      )

      output$tittelFord <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(UtDataFord$tittel),
          shiny::h5(shiny::HTML(paste0(UtDataFord$utvalgTxt, "<br />")))
        )
      })

      tabFord <- lagTabavFigAndeler(UtDataFraFig = UtDataFord)

      output$fordelingTab <- function() {
        antKol <- ncol(tabFord)
        kableExtra::kable(
          tabFord, format = "html",
          full_width = FALSE,
          digits = c(0, 1, 0, 1)[1:antKol]
        ) %>%
          kableExtra::add_header_above(
            c(" " = 1, "Egen enhet/gruppe" = 2, "Resten" = 2)[1:(antKol/2 + 1)]
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(
            column = 2:(ncol(tabFord) + 1), width = "7em"
          ) %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabFord <- shiny::downloadHandler(
        filename = function() {paste0(input$valgtVar, '_fordeling.csv')},
        content = function(file, filename) {
          write.csv2(tabFord, file, row.names = FALSE, na = "")
        }
      )

      output$fordelingPrSh <- shiny::renderPlot({
        NSFigAndelerSh(
          RegData = RegData, preprosess = 0,
          register = 'nordicscir',
          valgtVar = input$valgtVar,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          datoUt = as.numeric(input$datoUt),
          AIS = as.numeric(input$AIS), traume = input$traume,
          nivaaUt = as.numeric(input$nivaaUt),
          minald = as.numeric(input$alder[1]),
          maxald = as.numeric(input$alder[2]),
          erMann = as.numeric(input$erMann),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figFordSh <- shiny::downloadHandler(
        filename = function() {
          paste0("FordelingPrSh_", valgtVar = input$valgtVar, "_", Sys.time(),
                 ".", input$bildeformatFord)
        },
        content = function(file) {
          NSFigAndelerSh(
            RegData = RegData, preprosess = 0,
            register = 'nordicscir',
            valgtVar = input$valgtVar,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt),
            AIS = as.numeric(input$AIS), traume = input$traume,
            nivaaUt = as.numeric(input$nivaaUt),
            minald = as.numeric(input$alder[1]),
            maxald = as.numeric(input$alder[2]),
            erMann = as.numeric(input$erMann),
            session = session,
            outfile = file
          )
        }
      )

      UtDataFordSh <- NSFigAndelerSh(
        RegData = RegData, preprosess = 0,
        register = 'nordicscir',
        valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        AIS = as.numeric(input$AIS), traume = input$traume,
        nivaaUt = as.numeric(input$nivaaUt),
        minald = as.numeric(input$alder[1]),
        maxald = as.numeric(input$alder[2]),
        erMann = as.numeric(input$erMann),
        session = session
      )

      tabFordSh <- lagTabavFigAndelerSh(UtDataFraFig = UtDataFordSh)

      output$fordelingShTab <- function() {
        antKol <- ncol(tabFordSh)/2
        kableExtra::kable(
          tabFordSh,
          format = "html",
          full_width = FALSE,
          digits = c(rep(0, antKol), rep(1, antKol))
        ) %>%
          kableExtra::add_header_above(
            header = c(" " = 1, "Antall" = antKol, "Andel" = antKol)
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(
            column = 2:(ncol(tabFordSh) + 1), width = "7em"
          ) %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabFordSh <- shiny::downloadHandler(
        filename = function() {paste0(input$valgtVar, "_fordelingSh.csv")},
        content = function(file, filename) {
          write.csv2(tabFordSh, file, row.names = FALSE, na = "")
        }
      )
    } else {
      output$fordelinger <- NULL
      output$lastNed_figFord <- NULL
      output$tittelFord <- NULL
      output$fordelingTab <- NULL
      output$lastNed_tabFord <- NULL
      output$fordelingPrSh <- NULL
      output$lastNed_figFordSh <- NULL
      output$fordelingShTab <- NULL
      output$lastNed_tabFordSh <- NULL
    }
  }) #observe Fordeling




  #-------Samlerapporter--------------------

  #----------- Eksport ----------------
  registryName <- "nordicscir"
  ## brukerkontroller
  rapbase::exportUCServer("nordicscirExport", registryName)

  ## veileding
  rapbase::exportGuideServer("nordicscirExportGuide", registryName)
}

# Run the application
#shiny::shinyApp(ui = ui_nordicscir, server = server_nordicscir)
