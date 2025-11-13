#Resultattjeneste for NorScir

#' Brukergrensesnitt (ui) til nordscir-appen
#'
#' @return Brukergrensesnittet (ui) til nordscir-appen
#' @export
ui_norscir <- function() {

  # shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  startDato <- as.Date(
    paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01')
  )
  startDato2 <- as.Date(
    paste0(as.numeric(format(Sys.Date()-700, "%Y")), '-01-01')
  )

  # context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
  regTitle = "Norsk ryggmargsskaderegister"

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

 enhetsUtvalg <- list("Egen mot resten av landet" = 1,
                      "Hele landet" = 0,
                      "Egen enhet" = 2)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::navbarPage(
      id = "hovedark",
      title = rapbase::title(regTitle),
      windowTitle = regTitle,
      theme = rapbase::theme(),  # "rap/bootstrap.css",

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
          shiny::br(),
          shiny::h3("Månedsrapport"), #),
          shiny::downloadButton(
            outputId = "mndRapp.pdf",
            label = "Last ned MÅNEDSRAPPORT",
            class = "butt"
          ),
          shiny::br(),
          shiny::br(paste("NB: Nedlasting tar litt tid. I mellomtida får man",
                          "ikke sett på andre resultater.")),
          shiny::br(),
          shiny::br(paste("Hvis du ønsker månedsrapporten regelmessig tilsendt",
                          "på e-post, kan du gå til fanen 'Abonnement' og",
                          "bestille dette.")),
          shiny::br(),
          shiny::br(),
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
          # if (context %in% c("DEV", "TEST", "QA", "PRODUCTION", "QAC", "PRODUCTIONC")) {
            rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
          #},
          shiny::h2("Velkommen til Rapporteket - Norsk Ryggmargsskaderegister!",
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
                shiny::column(
                  width = 6,
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("alle pasienter", align = "center"),
                  shiny::br(),
                  shiny::tableOutput("tabNevrKlass")),
                shiny::column(
                  width = 6,
                  shiny::h3("Nevrologisk klassifikasjon", align = "center"),
                  shiny::h4("pasienter med liggetid over 28 dager i
                                   ryggmargsskadeavdeling", align = "center"),
                  shiny::tableOutput("tabNevrKlass28")
                )
              ),

              shiny::fluidRow(
                shiny::h3("Liggetider, egen avdeling", align = "left"),
                shiny::tableOutput("tabLiggetider")
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
          selectInput(
            inputId = "valgtVar",
            label="Velg variabel",
            choices = c(
              "Alder" = "Alder",
              "Ais ved innleggelse" = "AAis" ,
              "Ais ved utskriving" = "FAis",
              "Anbefalt tid til kontroll" = "AnbefTidKtr",
              'BMI ved innleggelse' = 'ABMI',
              'BMI ved utskriving' = 'FBMI',
              "Lengde på rehab.opphold" = "DagerRehab",
              "Opphold, totalt antall dager" = "OpphTot",
              "Operasjon på ryggsøylen" = "SpnlSurg2",
              "Planlagt utskrevet til" = "PPlaceDis",
              "Ventilasjonsstøtte (f.o.m 2024)" = "VentAssi2",
              "Registreringsforsinkelse" = "RegForsinkelse",
              "Komplikasjoner, primæropph" = "KomplPrim",
              "Skadeårsak " = "SkadeArsak",
              "Skadeårsak, ikke-traumatisk" = "Ntsci",
              "Tid fra skade til oppstart rehab." = "DagerTilRehab",
              "Tid med rehabilitering" = "DagerRehab",
              "Utskrevet til" = "UtTil",
              "A&D Funksjon: Mobilitet" = "FunkMob",
              "A&D Funksjon: Påkledning" = "FunkKler",
              "A&D Funksjon: Spising" = "FunkSpis",
              "A&D Funksjon: Toalett" = "FunkDo",
              "A&D Tilfredshet: Mobilitet" = "TilfMob",
              "A&D Tilfredshet: Påkledning" = "TilfKler",
              "A&D Tilfredshet: Spising" = "TilfSpis",
              "A&D Tilfredshet: Toalett" = "TilfDo",
              "Livskval.: Tilfredshet med livet" = "LivsGen",
              "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
              "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk",
              "Livskval.: Tilfredshet med sosialt liv" = "LivsSosLiv",
              "Urin: Ufrivillig urinlekkasje (fra 2019)" = "UrinInkontinens",
              "Urin: Ufrivillig urinlekkasje (t.o.m. 2018)" = "UrinInkontinensTom2018",
              "Urin: Kirurgiske inngrep" = "UrinKirInngr",
              "Urin: Legemiddelbruk (fra 2019)" = "UrinLegemidler",
              "Urin: Blæretømming, hovedmetode" = "UrinTomBlareHoved",
              "Urin: Blæretømming, tilleggsmetode" = "UrinTomBlareTillegg",
              "Tarm: Avføring, hovedmetode" = "TarmAvfHoved",
              "Tarm: Avføring, tilleggsmetode" = "TarmAvfTillegg",
              "Tarm: Avføringsmiddelbruk" = "TarmAvfmiddel",
              "Tarm: Avføringsmidler, hvilke" = "TarmAvfmiddelHvilke",
              "Tarm: Fekal inkontinens (fra 2019)" = "TarmInkontinensFra2019",
              "Tarm: Fekal inkontinens (før 2019)" = "TarmInkontinensTom2018",
              "Tarm: Kirurgisk inngrep" = "TarmKirInngrep",
              "Tarm: Kirurgiske inngrep, hvilke" = "TarmKirInngrepHvilke",
              "Tarm: NBD" = "TarmNBD",
              "EQ5D: Mobilitet" = "Eq5dQ1Mobility",
              "EQ5D: Personlig stell" = "Eq5dQ2Selfcare",
              "EQ5D: Daglige gjøremål" = "Eq5dQ3UsualActivities",
              "EQ5D: Smerter, ubehag" = "Eq5dQ4PainDiscomfort",
              "EQ5D: Angst og depresjon" = "Eq5dQ5AnxietyDepression",
              "EQ5D: Generell helsetilstand" = "Eq5dQ6HealthToday",
              "Kontroll: Hvordan ble kontrollen gjennomført?" = "KontUtfHvordan",
              "Kontroll: Komplikasjoner" = "KontrKompl",
              "Kontroll: Årsak, ikke gj.ført kontroll" = "KontControlInterruptedReason"
            ),
             selected = c("Registreringsforsinkelse" = "RegForsinkelse")
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
              choices = enhetsUtvalg)
              # , selected = 1)
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
      ), #Sidebar

        shiny::mainPanel(
          width = 6,
          shiny::tabsetPanel(
            id="fordeling",
            shiny::tabPanel(
              "Figur",
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
          ) #tabset
        ) #mainPanel
      ), #tab Fordelinger


      #--------Før/etter----------------
      tabPanel('Resultater over tid',
               h3('Sammenligne resultater ved innkomst/utreise eller utreise/kontroll'),
               sidebarPanel(
                 width = 3,
                 conditionalPanel(
                   condition = "input.PP == 'Fordeling før/etter'",
                   selectInput(
                     inputId = "valgtVarPP",
                     label="Velg variabel ",
                     choices = c("AIS, utskriving/kontroll"="KontFAis",
                                 "Utskrevet til "="KontUtTil")
                   )),
                 conditionalPanel(
                   condition = "input.PP == 'Stabelplott, før/etter'",
                   selectInput(
                     inputId = "valgtVarStabelPP",
                     label="Velg variabel",
                     choices = c("AIS inn/ut" = "AAisFAis",
                                 "AIS ut/1.kontroll" = "KontFAis"),
                     selected = "KontFAis"
                   )),
                 selectInput(
                     inputId = "enhetsUtvalgPP",
                     label="Egen enhet/hele landet",
                     choices = c("Hele landet" = 0,
                                 "Egen enhet" = 2)
                   ),
                 shiny::dateRangeInput(
                   inputId = "datovalgPP",
                   start = startDato,
                   end = Sys.Date(),
                   label = "Tidsperiode", separator="t.o.m.", language="nb"
                 ),
                 shiny::radioButtons(
                   inputId = "datoUtPP",
                   "Bruk utskrivingsdato til datofiltrering?",
                   choiceNames = c("nei", "ja"),
                   choiceValues = 0:1,
                   selected = 0
                 ),
                 shiny::selectInput(
                   inputId = "bildeformatPP",
                   label = "Velg format for nedlasting av figur",
                   choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   id = 'PP',
                   tabPanel( 'Fordeling før/etter',
                             br(),
                             h3("Endring fra utskriving til kontroll"), #),
                             br(),
                             plotOutput("fordPrePost", height = "auto"),
                             downloadButton(
                               outputId = "lastNed_figfordPrePost", label="Last ned figur"
                             )
                   ),
                   tabPanel('Stabelplott, før/etter',
                            plotOutput("figStabelPrePost", height = "auto"),
                            downloadButton(
                              outputId = "lastNed_figStabelPrePost", label="Last ned figur"
                            )
                   )
                 ) ) #main
      ), #Resultater over tid


      #----------Andeler-----------------------------
      tabPanel(p("Andeler: per sykehus og tid", title='Alder, antibiotika, ASA, fedme, gjennomføringsgrad, komplikasjoner,
           konvertering, oppfølging, registreringsforsinkelse, komplikasjoner, TSS2, utdanning'),
               h2("Sykehusvise andeler og utvikling over tid for valgt variabel", align='center'),
               h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
            til venstre. Man kan også gjøre ulike filtreringer.", align='center'),
               br(),
               sidebarPanel(
                 width=3,
                 h3('Utvalg'),

                 selectInput(
                   inputId = "valgtVarAndel", label="Velg variabel",
                   choices = c('Målt både høyde og vekt ved innleggelse?' = 'ABMI',
                               'Målt både høyde og vekt ved utskriving?' = 'FBMI'
                   ),
                   selected = 'ABMI'
                 ),
                 dateRangeInput(inputId = 'datovalgAndel', start = startDato, end = Sys.Date(),
                                label = "Tidsperiode", separator="t.o.m.", language="nb"),
                 # selectInput(inputId = "bildeformatAndel",
                 #             label = "Velg format for nedlasting av figur",
                 #             choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
                 br(),
                 br(),
                 p(em('Følgende utvalg gjelder bare figuren/tabellen som viser utvikling over tid')),
                 selectInput(inputId = 'enhetsUtvalgAndel', label='Egen enhet og/eller landet',
                             choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)),
                 selectInput(inputId = "tidsenhetAndel", label="Velg tidsenhet",
                             choices = rev(tidsenheter))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Figurer",
                            h3(em("Utvikling over tid")),
                            br(),
                            h4('Kommer...'),
                            #plotOutput("andelTid", height = 'auto'),
                            #downloadButton('LastNedFigAndelTid', label='Velg format (til venstre) og last ned figur'),
                            br(),
                            h3(em("Sykehusvise resultater")),
                            plotOutput("andelerGrVar", height='auto'),
                            #downloadButton('LastNedFigAndelGrVar', label='Velg format (til venstre) og last ned figur')
                   ),
                    tabPanel("Tabeller",
                             uiOutput("tittelAndel"),
                             br(),
                             h3('Her kan det komme en tabell med verdier fra figuren på forrige side')
                   #          column(width = 4,
                   #                 h3("Sykehusvise resultater"),
                   #                 tableOutput("andelerGrVarTab"),
                   #                 downloadButton(outputId = 'lastNed_tabAndelGrVar', label='Last ned tabell')),
                   #          column(width = 1),
                   #          column(width = 6,
                   #                 h3("Utvikling over tid"),
                   #                 tableOutput("andelTidTab"),
                   #                 downloadButton(outputId = 'lastNed_tabAndelTid', label='Last ned tabell'))
                    ))
               ) #mainPanel

      ), #tab




      #------------ Gjennomsnitt ------------
      shiny::tabPanel(
        "Gj.sn./median per sykehus og over tid",
        shiny::sidebarPanel(
          width = 3,
          shiny::selectInput(
            inputId = "valgtVarGjsn",
            label="Velg variabel",
            choices = c("Alder" = "Alder",
                        "Lengde på rehab.opphold" = "DagerRehab",
                        "Opphold, totalt antall dager" = "OpphTot",
                        "Registreringsforsinkelse" = "RegForsinkelse",
                        "Tid fra skade til oppstart rehab." = "DagerTilRehab",
                        "Livskval.: Tilfredshet med livet" = "LivsGen",
                        "Livskval.: Tilfredshet med fysisk helse" = "LivsFys",
                        "Livskval.: Tilfredshet med psykisk helse" = "LivsPsyk",
                        "Livskval.: Tilfredshet med sosialt liv" = "LivsSosLiv"
            ),
            selected = c("Registreringsforsinkelse" = "RegForsinkelse")
          ),
          shiny::dateRangeInput(
            inputId = "datovalgGjsn",
            start = startDato,
            end = Sys.Date(),
            label = "Tidsperiode", separator="t.o.m.", language="nb"
          ),
          shiny::radioButtons(
            inputId = "datoUtGjsn",
            "Bruk utskrivingsdato til datofiltrering?",
            choiceNames = c("nei", "ja"),
            choiceValues = 0:1,
            selected = 0
          ),
          shiny::selectInput(inputId = "erMannGjsn",
                             label="Kjønn",
                             choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
          ),
          shiny::sliderInput(
            inputId="alderGjsn",
            label = "Alder",
            min = 0,
            max = 110,
            value = c(0, 110)
          ),
          shiny::selectInput(
            inputId = "AISGjsn",
            label="AIS-grad ved utreise",
            multiple = TRUE, #selected=0,
            choices = valgAIS
          ),
          shiny::selectInput(
            inputId = "traumeGjsn",
            label="Traume",
            choices = c("Alle"=" ", #"ikke"
                        "Traume"="ja",
                        "Ikke traume"="nei")
          ),
          shiny::selectInput(
            inputId = "paratetraGjsn",
            label="Nivå ved utreise",
            choices = c("Alle" = 99,
                        "Paraplegi" = 0,
                        "Tetraplegi" = 1,
                        "Ukjent" = 9)
          ),
          shiny::selectInput(
            inputId = "sentralmaal",
            label="Velg gjennomsnitt/median ",
            choices = c("Median"="med", "Gjennomsnitt"="gjsn")
          ),
          shiny::br(),
          shiny::p(
            shiny::em(paste("Følgende utvalg gjelder bare figuren/tabellen som",
                            "viser utvikling over tid"))
          ),
          shiny::selectInput(
            inputId = "enhetsUtvalgGjsn",
            label="Egen enhet og/eller landet",
            choices = c("Egen mot resten av landet"=1,
                        "Hele landet" = 0,
                        "Egen enhet" = 2)
          ),
          shiny::selectInput(
            inputId = "tidsenhetGjsn",
            label = "Velg tidsenhet",
            choices = tidsenheter,
            selected = 'Kvartal'
          ),
          shiny::selectInput(
            inputId = "bildeformatGjsn",
            label = "Velg format for nedlasting av figur",
            choices = c("pdf", "png", "jpg", "bmp", "tif", "svg")
          )
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Figur",
              shiny::br(),
              shiny::h3(shiny::em("Utvikling over tid")),
              shiny::plotOutput("gjsnTid", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figGjsnTid", label="Last ned figur"
              ),
              shiny::br(),
              shiny::h3(em("Sykehusvise resultater")),
              shiny::plotOutput("gjsnGrVar", height = "auto"),
              shiny::downloadButton(
                outputId = "lastNed_figGjsnGrVar", label = "Last ned figur")
            ),
            shiny::tabPanel(
              "Tabell",
              shiny::uiOutput("tittelGjsnGrVar"),
              shiny::br(),
              shiny::tableOutput("gjsnTidTab"),
              shiny::br(),
              shiny::tableOutput("gjsnGrVarTab"),
              shiny::downloadButton(
                outputId = "lastNed_tabGjsnGrVar", label = "Last ned"
              )
            )
          )
        )
      ), #Gjsn


      #-----Registreringsoversikter------------
      shiny::tabPanel(
        "Registreringsoversikter",
        shiny::sidebarPanel(
          width=3,
          shiny::h3("Utvalg"),
          shiny::conditionalPanel(
            condition = "input.reg == 'Antall personer med ryggmargsskade'",
            shiny::dateInput(
              inputId = "sluttDatoReg",
              label = "Velg sluttdato",
              language="nb",
              value = Sys.Date(),
              max = Sys.Date()
            ),
            shiny::radioButtons(
              inputId = "datoUtReg",
              label = "Bruk utskrivingsdato til datofiltrering?",
              choiceNames = c("nei", "ja"),
              choiceValues = 0:1,
              selected = 0
            ),
            shiny::selectInput(
              inputId = "tidsenhetReg",
              label="Velg tidsenhet",
              choices = tidsenheter
          ),
          shiny::selectInput(
            inputId = "antTidsenhReg",
            label="Antall tidsenheter",
            choices = rev(c(5:12))
          ),
            shiny::selectInput(
              inputId = "traumeReg",
              label="Traume",
              choices = c("Alle"=" ", #"ikke"
                          "Traume"="ja",
                          "Ikke traume"="nei"))
          ),
          shiny::conditionalPanel(
            condition = paste0(
              "input.reg == 'Antall hovedskjema med tilknyttede skjema' | ",
              "input.reg == 'Antall kontrollskjema med tilknyttede skjema' "
            ),
            shiny::dateRangeInput(
              inputId = "datovalgReg",
              start = startDato,
              end = Sys.Date(),
              label = "Tidsperiode",
              separator="t.o.m.",
              language="nb"
            )
          )
        ),

        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "reg",
            shiny::tabPanel(
              "Antall personer med ryggmargsskade",
              shiny::uiOutput("undertittelReg"),
              shiny::p(paste("Velg tidsperiode ved å velge sluttdato/tidsenhet",
                             "i menyen til venstre")),
              shiny::br(),
              shiny::fluidRow(
                shiny::tableOutput("tabAntOpphTid"),
                shiny::downloadButton(
                  outputId = "lastNed_tabAntOpph", label="Last ned"
                )
              )
            ),
            shiny::tabPanel(
              "Antall hovedskjema med tilknyttede skjema",
              shiny::h3("Antall hovedskjema med tilknyttede skjema"),
              h5('Tidsperioden er basert på innleggelsesdato'),
              shiny::tableOutput("tabAntTilknyttedeHovedSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfHovedAnt", label = "Last ned"
              ),
              shiny::br(),
              shiny::h3("Andel (%) hovedskjema med tilknyttede skjema"),
              shiny::tableOutput("tabAndelTilknyttedeHovedSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfHovedPst", label="Last ned"
              )
            ),
            shiny::tabPanel(
              "Antall kontrollskjema med tilknyttede skjema",
              shiny::h3("Antall kontrollskjema med tilknyttede skjema"),
              shiny::h5("Datoutvalg er basert på dato for kontroll"),
              shiny::tableOutput("tabAntTilknyttedeKtrSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfKtrAnt", label="Last ned"
              ),
              shiny::br(),
              shiny::h3("Andel (%) kontrollskjema med tilknyttede skjema"),
              shiny::tableOutput("tabAndelTilknyttedeKtrSkjema"),
              shiny::downloadButton(
                outputId = "lastNed_tabOppfKtrPst", label="Last ned"
              )
            )
          )
        )
      ), #tab Registreringsoversikter

      #----------------------Registeradministrasjon-----------------------------

      shiny::tabPanel(
        "Registeradministrasjon",
        shiny::h2("Fane som bare er synlig for SC-bruker."),

        shiny::tabsetPanel(
          id = "ark",
          shiny::tabPanel(
            "Samlerapporter",
            shiny::sidebarPanel(
              width=3,
              shiny::h3("Utvalg"),
              shiny::dateRangeInput(
                inputId = "datovalgSamleRapp",
                start = startDato-150,
                end = Sys.Date(),
                label = "Tidsperiode",
                separator="t.o.m.",
                language="nb"
              ),
              br()
            ),

            shiny::mainPanel(
              shiny::br(),
              shiny::br(),
              shiny::h3("Resultater, hele landet"), #),
              shiny::downloadButton(
                outputId = "samleRappLand.pdf",
                label="Last ned samlerapport, hele landet", class = "butt"
              ),
              shiny::br(),
              shiny::h3("Resultater eget sykehus"), #),
              shiny::downloadButton(
                outputId = "samleRappEgen.pdf",
                label="Last ned egen samlerapport", class = "butt"
              ),
              shiny::br()

            )
          ),
          shiny::tabPanel(
            "Utsendinger",
            title = "Utsending av rapporter",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::autoReportOrgInput("NSuts"),
                rapbase::autoReportInput("NSuts"),
                br(),
                br()
                # shiny::actionButton(inputId = "run_autoreport",
                #                     label = "Kjør autorapporter"),
                # shiny::dateInput(inputId = "rapportdato",
                #                  label = "Kjør rapporter med dato:",
                #                  value = Sys.Date(),
                #                  min = Sys.Date(),
                #                  max = Sys.Date() + 366
                # )
                #, shiny::checkboxInput(inputId = "dryRun", label = "Send e-post")
              ),
              shiny::mainPanel(
                rapbase::autoReportUI("NSuts"),
                shiny::br()
              #     p(em("System message:")),
              #     verbatimTextOutput("sysMessage"),
              #     p(em("Function message:")),
              #     verbatimTextOutput("funMessage")
              # #  )
              )
            )
          ),
          shiny::tabPanel(
            "Eksport, krypterte data",
            shiny::sidebarPanel(
              rapbase::exportUCInput("norscirExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("norscirExportGuide")
            )
          ) #Eksport-tab
        ) #tabsetPanel
      ), #Registeradm-tab

      #------------------Abonnement------------------------
      shiny::tabPanel(
        shiny::p(
          "Abonnement",
          title="Bestill automatisk utsending av månedsrapport på e-post"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("ns-subscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("ns-subscription")
          )
        )
      )
    ) #navbar
  ) #tagList
}


#' Server-del til norscir-appen
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return Server-delen til norscir-appen
#' @export
server_norscir <- function(input, output, session) {

rapbase::appLogger(
  session = session,
  msg = "Starter norscir-app'en"
)

  isGetDataOk <- TRUE
  isprocessAllDataOk <- TRUE
  AlleTab <- nordicscir::getRealData(register = 'norscir')
  if (is.null(AlleTab)) {
    warning("Not able to get real data!")
    isGetDataOk <- FALSE
    #AlleTab <- nordicscir::getFakeData() #Har foreløpig bare norske, fiktive data. Men blir de hentet...?
  }
  AlleTab <- nordicscir::processAllData(AlleTab, register = 'norscir')
  if (is.null(AlleTab)) {
    warning("Not able to process data.")
    isprocessAllDataOk <- FALSE
  }


  isDataOk <- all(c(isGetDataOk, isprocessAllDataOk))
  attach(AlleTab)

  map_avdeling <- data.frame(
    UnitId = unique(HovedSkjema$ReshId),
    orgname = HovedSkjema$ShNavn[match(unique(HovedSkjema$ReshId),
                                               HovedSkjema$ReshId)])

  #user inneholder både reshID: user$org() og  rolle: user$role()
  user <- rapbase::navbarWidgetServer2(
    id = "navbar-widget",
    orgName = "norscir", #nordicscir
    map_orgname = shiny::req(map_avdeling),
    caller = "norscir"
  )

  observeEvent(user$role(), {
    if (user$role() == 'SC') {
      showTab(inputId = "hovedark", target = "Registeradministrasjon")
    } else {
      hideTab(inputId = "hovedark", target = "Registeradministrasjon")
    }
  })

  #--------------Startside------------------------------

  output$guide <- shiny::renderText(
    rapbase::renderRmd(
      system.file("brukerveiledning.Rmd", package = "nordicscir"),
      outputType = "html_fragment",
      params = list(isDataOk = isDataOk)
    )
  )
  if (isDataOk) {
    output$tabAntOpphShMnd12startside <-
      shiny::renderTable(
        nordicscir::tabAntOpphShMnd(RegData = HovedSkjema, antMnd = 12),
        rownames = T, digits=0, spacing="xs"
      )
  } else {
    output$tabAntOpphShMnd12startside <- NULL
  }
  observe({
    if (isDataOk) {
      output$tabNevrKlass <- shiny::renderTable(
        nordicscir::lagTabNevrKlass(
          HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        ),
        rownames = TRUE
      )
      output$tabNevrKlass28 <- shiny::renderTable({
        HovedSkjema28 <- HovedSkjema[which(HovedSkjema$DagerRehab > 28), ]
        nordicscir::lagTabNevrKlass(
          HovedSkjema28,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2]
        )
      },
      rownames = TRUE
      )
      output$tabLiggetider <- shiny::renderTable({
        nordicscir::tabLiggetider(
          RegData = HovedSkjema,
          datoFra = input$datovalgDash[1],
          datoTil = input$datovalgDash[2],
          enhetsUtvalg=2,
          reshID = user$org()
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
    t1 <- paste0("Tabellen viser antall opphold basert på ",
                 c("første akutte innleggelse.", "utskrivingsdato.")[as.numeric(input$datoUtReg)+1])
    h4(HTML(t1))
    # shiny::h4(shiny::HTML(
    #   switch(
    #     input$tidsenhetReg,
    #     Mnd = paste0(t1, "siste 12 måneder før ", input$sluttDatoReg, t2,
    #                  "<br />"),
    #     Aar = paste0(t1, "siste 10 år før ", input$sluttDatoReg, t2, "<br />")
    #   )
    # ))
  })
  shiny::observe({
    if (isDataOk) {
      tabAntOpphShTid <- tabAntOpphShTid(RegData=HovedSkjema,
                                         datoTil=input$sluttDatoReg,
                                         tidsenhet = input$tidsenhetReg,
                                         antTidsenh = as.numeric(input$antTidsenhReg),
                                         datoUt = as.numeric(input$datoUtReg),
                                         traume=input$traumeReg)

      output$tabAntOpphTid <- shiny::renderTable(
        tabAntOpphShTid, rownames = TRUE, digits = 0, spacing = "xs"
      )

      output$lastNed_tabAntOpph <- shiny::downloadHandler(
        filename = function() {paste0("tabAntOpph.csv")},
        content = function(file, filename) {
          write.csv2(tabAntOpphShTid, file, row.names = TRUE, fileEncoding = 'latin1', na = "")}
      )

      #Antall skjema av alle typer.
      tabTilknHovedSkjema <- nordicscir::tabSkjemaTilknyttet(
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
            tabTilknHovedSkjema$Antall, file, row.names = TRUE, fileEncoding = 'latin1', na = ""
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
            tabTilknHovedSkjema$Andeler, file, row.names = TRUE, fileEncoding = 'latin1', na = ""
          )
        }
      )

      #Kontrollskjema som har tilknyttede skjema av ulik type
      tabTilknKtrSkjema <- nordicscir::tabSkjemaTilknyttet(
        Data = AlleTab,
        moderSkjema = "Kont",
        datoFra = input$datovalgReg[1],
        datoTil = input$datovalgReg[2]
      )

      output$tabAntTilknyttedeKtrSkjema <- shiny::renderTable(
        tabTilknKtrSkjema$Antall, rownames = TRUE, digits = 0, spacing = "xs"
      )

      output$lastNed_tabOppfKtrAnt <- shiny::downloadHandler(
        filename = function() {'tabOppfKtrAnt.csv'},
        content = function(file, filename) {
          write.csv2(tabTilknKtrSkjema$Antall, file, row.names = TRUE, fileEncoding = 'latin1', na = "")
        }
      )
      #Andel (prosent) av kontrollskjemaene som har oppfølgingsskjema.
      output$tabAndelTilknyttedeKtrSkjema <- shiny::renderTable(
        tabTilknKtrSkjema$Andeler, rownames = TRUE, digits = 0, spacing = "xs" )

      output$lastNed_tabOppfKtrPst <- shiny::downloadHandler(
        filename = function() {"tabOppfKtrPst.csv"},
        content = function(file, filename) {
          write.csv2(tabTilknKtrSkjema$Andeler, file, row.names = TRUE, fileEncoding = 'latin1', na = "")
        }
      )
    } else {
      output$tabAntOpphTid <- NULL
      output$lastNed_tabAntOpph <- NULL
      output$tabAntTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedAnt <- NULL
      output$tabAndelTilknyttedeHovedSkjema <- NULL
      output$lastNed_tabOppfHovedPst <- NULL
      output$lastNed_tabOppfKtrAnt <- NULL
      output$tabAndelTilknyttedeKtrSkjema <- NULL
      output$lastNed_tabOppfKtrPst <- NULL
    }
  })


  #---------Fordelinger:--fordelingsfigurer og tabeller----------
  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVar, Data = AlleTab)

      output$fordelinger <- shiny::renderPlot({
        nordicscir::NSFigAndeler(
          RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          reshID = user$org(),
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
          nordicscir::NSFigAndeler(
            RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
            datoFra = input$datovalg[1], datoTil = input$datovalg[2],
            datoUt = as.numeric(input$datoUt), reshID = user$org(),
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

      UtDataFord <-
        nordicscir::NSFigAndeler(
        RegData = RegData, preprosess = 0, valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        reshID = user$org(),
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

      tabFord <- nordicscir::lagTabavFigAndeler(UtDataFraFig = UtDataFord)

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
          write.csv2(tabFord, file, row.names = FALSE, fileEncoding = 'latin1', na = "")
        }
      )

      output$fordelingPrSh <- shiny::renderPlot({
        nordicscir::NSFigAndelerSh(
          RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
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
          nordicscir::NSFigAndelerSh(
            RegData = RegData, valgtVar = input$valgtVar, preprosess = 0,
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

      UtDataFordSh <-
        nordicscir::NSFigAndelerSh(
        RegData = RegData, preprosess = 0, valgtVar = input$valgtVar,
        datoFra = input$datovalg[1], datoTil = input$datovalg[2],
        datoUt = as.numeric(input$datoUt),
        AIS = as.numeric(input$AIS), traume = input$traume,
        nivaaUt = as.numeric(input$nivaaUt),
        minald = as.numeric(input$alder[1]),
        maxald = as.numeric(input$alder[2]),
        erMann = as.numeric(input$erMann),
        session = session
      )

      tabFordSh <- nordicscir::lagTabavFigAndelerSh(UtDataFraFig = UtDataFordSh)

      output$fordelingShTab <- function() {
        antKol <- ncol(tabFordSh)
        kableExtra::kable(
          tabFordSh,
          format = "html",
          full_width = FALSE,
          digits = c(0, 0, 0, 1, 1, 1)[1:antKol]
        ) %>%
          kableExtra::add_header_above(
            header = c(" " = 1, "Antall" = 3, "Andel" = 3)
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
          write.csv2(tabFordSh, file, row.names = FALSE, fileEncoding = 'latin1', na = "")
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



  #----------Før/etter--------------

  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarPP, Data = AlleTab)
      #RegData <- nordicscir::TilLogiskeVar(RegData)

      output$fordPrePost <- shiny::renderPlot({
        nordicscir::NSFigPrePost(
          RegData = RegData, preprosess = 0,
          valgtVar = input$valgtVarPP,
          datoFra = input$datovalgPP[1], datoTil = input$datovalgPP[2],
          reshID = user$org(),
          # AIS = as.numeric(input$AIS), traume = input$traume,
          # nivaaUt = as.numeric(input$nivaaUt),
          # minald = as.numeric(input$alder[1]),
          # maxald = as.numeric(input$alder[2]),
          # erMann = as.numeric(input$erMann),
          enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
          datoUt = as.numeric(input$datoUtPP),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figfordPrePost <- shiny::downloadHandler(
        filename = function() {
          paste0("FigPreKtr_", input$valgtVarPP, "_", Sys.time(), #input$valgtVarKtr
                 ".", input$bildeformatPP)
        },
        content = function(file) {
          nordicscir::NSFigPrePost(
            RegData = RegData, reshID = user$org(), preprosess = 0,
            valgtVar = input$valgtVarPP,
            datoFra = input$datovalgPP[1], datoTil = input$datovalgPP[2],
            datoUt = as.numeric(input$datoUtPP),
            enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
            session = session,
            outfile = file
          )
        }
      )

    } else {
      output$fordPrePost <- NULL
      output$lastNed_figfordPrePost <- NULL}
  })

  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarStabelPP, Data = AlleTab)
     # RegData <- nordicscir::TilLogiskeVar(RegData)

      output$figStabelPrePost <- shiny::renderPlot({
        nordicscir::NSFigStabelAnt(
          RegData = RegData, preprosess = 0,
          valgtVar = input$valgtVarStabelPP,
          datoUt = as.numeric(input$datoUtPP),
          datoFra = input$datovalgPP[1], datoTil = input$datovalgPP[2],
          reshID = user$org(),
          enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
          session = session
        )},
        height = 800, width = 800
      )

      output$lastNed_figStabelPrePost <- shiny::downloadHandler(
        filename = function() {
          paste0("FigFordPP_", input$valgtVarPP, "_", Sys.time(),
                 ".", input$bildeformatPP)
        },
        content = function(file) {
            nordicscir::NSFigStabelAnt(
              RegData = RegData, preprosess = 0,
              valgtVar = input$valgtVarStabelPP,
              datoUt = as.numeric(input$datoUtPP),
              datoFra = input$datovalgPP[1], datoTil = input$datovalgPP[2],
              reshID = user$org(),
              enhetsUtvalg = as.numeric(input$enhetsUtvalgPP),
            session = session,
            outfile = file
          )
        }
      )

    } else {
      output$figStabelPrePost <- NULL
      #output$lastNed_figfordPrePost <- NULL
    }
  })









  #--------------Andeler-----------------------------------
  shiny::observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarAndel, Data = AlleTab)

  output$andelerGrVar <- renderPlot({
    NSFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
                        datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
                        #minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
                        #erMann=as.numeric(input$erMannAndel),
                        hovedkat = as.numeric(input$hovedInngrepAndel),
                        session=session)
  }, height = 800, width=700 #height = function() {session$clientData$output_andelerGrVarFig_width} #})
  )

  # output$LastNedFigAndelGrVar <- downloadHandler(
  #   filename = function(){
  #     paste0('AndelTid_', valgtVar=input$valgtVarAndel, '_', Sys.Date(), '.', input$bildeformatAndel)
  #   },
  #   content = function(file){
  #     NSFigAndelerGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
  #                         datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
  #                         #minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
  #                         #erMann=as.numeric(input$erMannAndel),
  #                         session=session,
  #                         outfile = file)
  #   })

  # output$andelTid <- renderPlot({
  #
  #   NSFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
  #                   reshID = user$org(),
  #                   datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
  #                   # minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
  #                   # erMann=as.numeric(input$erMannAndel),
  #                   tidsenhet = input$tidsenhetAndel,
  #                   enhetsUtvalg = input$enhetsUtvalgAndel,
  #                   session=session)
  # }, height = 300, width = 1000
  # )

#  observe({
    # AndelerTid <-
    #   NSFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
    #                   reshID = user$org(),
    #                   datoFra=as.Date(input$datovalgAndel[1]), datoTil=input$datovalgAndel[2],
    #                   # minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
    #                   # erMann=as.numeric(input$erMannAndel),
    #                   enhetsUtvalg = input$enhetsUtvalgAndel,
    #                   tidsenhet = input$tidsenhetAndel,
    #                   session=session) #,lagFig=0)
    # tabAndelTid <- lagTabavFig(UtDataFraFig = AndelerTid, figurtype = 'andelTid')


    # output$andelTidTab <- function() {
    #   antKol <- ncol(tabAndelTid)
    #   kableExtra::kable(tabAndelTid, format = 'html'
    #                     , full_width=F
    #                     , digits = c(0,0,1,0,0,1)[1:antKol]
    #   ) %>%
    #     kableExtra::add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
    #     kableExtra::column_spec(column = 1, width_min = '7em') %>%
    #     kableExtra::column_spec(column = 2:(antKol+1), width = '7em') %>%
    #     kableExtra::row_spec(0, bold = T)
    # }

    # output$lastNed_tabAndelTid <- downloadHandler(
    #   filename = function(){
    #     paste0(input$valgtVar, '_andelTid.csv')
    #   },
    #   content = function(file, filename){
    #     write.csv2(tabAndelTid, file, row.names = T, fileEncoding = 'latin1', na = '')
    #   })

    # output$LastNedFigAndelTid <- downloadHandler(
    #   filename = function(){
    #     paste0('AndelTid_', valgtVar=input$valgtVarAndel, '_', Sys.Date(), '.', input$bildeformatAndel)
    #   },
    #   content = function(file){
    #     NSFigAndelTid(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
    #                     reshID = user$org(),
    #                     datoFra=as.Date(input$datovalgAndel[1]), datoTil=input$datovalgAndel[2],
    #                     #minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
    #                     #erMann=as.numeric(input$erMannAndel),
    #                     enhetsUtvalg = input$enhetsUtvalgAndel,
    #                     tidsenhet = input$tidsenhetAndel,
    #                     session=session,
    #                     outfile = file)
    #   })
#  })

#  observe({    #AndelGrVar
    AndelerShus <- NSFigAndelerGrVar(
      RegData=RegData, preprosess = 0, valgtVar=input$valgtVarAndel,
      datoFra=input$datovalgAndel[1], datoTil=input$datovalgAndel[2],
      minald=as.numeric(input$alderAndel[1]), maxald=as.numeric(input$alderAndel[2]),
      erMann=as.numeric(input$erMannAndel),
      session=session) #, lagFig = 0))

    tabAndelerShus <- cbind('Antall (n)' = round(AndelerShus$Ngr*AndelerShus$AggVerdier/100),
                            'Antall (N)' = AndelerShus$Ngr,
                            Andeler = AndelerShus$AggVerdier)

    # output$andelerGrVarTab <- function() {
    #   antKol <- ncol(tabAndelerShus)
    #   kableExtra::kable(tabAndelerShus, format = 'html'
    #                     , digits = c(0,0,1)
    #   ) %>%
    #     kableExtra::column_spec(column = 1:(antKol+1), width = '5em') %>%
    #     kableExtra::row_spec(0, bold = T)
    # }
    # output$lastNed_tabAndelGrVar <- downloadHandler(
    #   filename = function(){
    #     paste0(input$valgtVar, '_andelGrVar.csv')
    #   },
    #   content = function(file, filename){
    #     write.csv2(tabAndelerShus, file, row.names = T, fileEncoding = 'latin1', na = '')
    #   })


    output$tittelAndel <- renderUI({
      tagList(
        h3(AndelerShus$tittel),
        h5(HTML(paste0(AndelerShus$utvalgTxt, '<br />')))
      )}) #, align='center'

    } else {
      output$tittelAndel <- NULL
      output$andelerGrVar <- NULL
      }
  })

  #------------------ Abonnement ----------------------------------------------



  #-----------------Sykehusvise gjennomsnitt, figur og tabell-------------------
  observe({
    if (isDataOk) {
      RegData <- nordicscir::finnRegData(valgtVar = input$valgtVarGjsn, Data = AlleTab)
      output$gjsnGrVar <- shiny::renderPlot(
        nordicscir::NSFigGjsnGrVar(RegData = RegData, preprosess = 0,
                       valgtVar = input$valgtVarGjsn,
                       datoFra = input$datovalgGjsn[1],
                       datoTil = input$datovalgGjsn[2],
                       datoUt = as.numeric(input$datoUtGjsn),
                       AIS = as.numeric(input$AISGjsn),
                       traume = input$traumeGjsn,
                       nivaaUt = as.numeric(input$paratetraGjsn),
                       minald = as.numeric(input$alderGjsn[1]),
                       maxald = as.numeric(input$alderGjsn[2]),
                       erMann = as.numeric(input$erMannGjsn),
                       valgtMaal = input$sentralmaal, session=session
        ),
        width = 800, height = 600
      )

      output$lastNed_figGjsnGrVar <- shiny::downloadHandler(
        filename = function() {
          paste0("FigGjsn_", valgtVar = input$valgtVarGjsn, "_", Sys.time(),
                 ".", input$bildeformatGjsn)
        },
        content = function(file) {
          nordicscir::NSFigGjsnGrVar(RegData = RegData, preprosess = 0,
                         valgtVar = input$valgtVarGjsn,
                         datoFra = input$datovalgGjsn[1],
                         datoTil = input$datovalgGjsn[2],
                         datoUt = as.numeric(input$datoUtGjsn),
                         AIS = as.numeric(input$AISGjsn),
                         traume = input$traumeGjsn,
                         nivaaUt = as.numeric(input$paratetraGjsn),
                         minald = as.numeric(input$alderGjsn[1]),
                         maxald = as.numeric(input$alderGjsn[2]),
                         erMann = as.numeric(input$erMannGjsn),
                         valgtMaal = input$sentralmaal, session=session,
                         outfile = file)
        })

      UtDataGjsnGrVar <- nordicscir::NSFigGjsnGrVar(
        RegData = RegData, preprosess = 0,
        valgtVar = input$valgtVarGjsn,
        datoFra = input$datovalgGjsn[1],
        datoTil = input$datovalgGjsn[2],
        datoUt = as.numeric(input$datoUtGjsn),
        AIS = as.numeric(input$AISGjsn),
        traume = input$traumeGjsn,
        nivaaUt = as.numeric(input$paratetraGjsn),
        minald = as.numeric(input$alderGjsn[1]),
        maxald = as.numeric(input$alderGjsn[2]),
        erMann = as.numeric(input$erMannGjsn),
        valgtMaal = input$sentralmaal,
        session = session
      )

      tabGjsnGrVar <- nordicscir::lagTabavFigGjsnGrVar(UtDataFraFig = UtDataGjsnGrVar)

      output$tittelGjsnGrVar <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(UtDataGjsnGrVar$tittel),
          shiny::h5(shiny::HTML(paste0(UtDataGjsnGrVar$utvalgTxt, "<br />")))
        )
      })

      output$gjsnGrVarTab <- function() {
        antKol <- ncol(tabGjsnGrVar)
        kableExtra::kable(tabGjsnGrVar, format = "html", digits = c(0, 1)) %>%
          kableExtra::column_spec(column = 1, width_min = "5em") %>%
          kableExtra::column_spec(column = 2:(antKol + 1), width = "4em") %>%
          kableExtra::row_spec(0, bold = TRUE)
      }
      output$lastNed_tabGjsnGrVar <- shiny::downloadHandler(
        filename = function() {
          paste0(input$valgtVar, "_tabGjsnSh.csv")
        },
        content = function(file, filename) {
          write.csv2(tabGjsnGrVar, file, row.names = TRUE, fileEncoding = 'latin1', na = "")
        }
      )

      output$titteltabGjsnGrVar <- shiny::renderUI({
        shiny::tagList(
          shiny::h3(tabGjsnGrVar$tittel),
          shiny::h5(shiny::HTML(paste0(tabGjsnGrVar$utvalgTxt, "<br />")))
        )
      })

      #------gjsnTid
      output$gjsnTid <- shiny::renderPlot(
        nordicscir::NSFigGjsnTid(
          RegData = RegData, reshID = user$org(), preprosess = 0,
          valgtVar = input$valgtVarGjsn,
          datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
          datoUt = as.numeric(input$datoUtGjsn),
          minald = as.numeric(input$alderGjsn[1]),
          maxald = as.numeric(input$alderGjsn[2]),
          erMann = as.numeric(input$erMannGjsn),
          AIS = as.numeric(input$AISGjsn),
          traume = input$traumeGjsn,
          nivaaUt = as.numeric(input$paratetraGjsn),
          valgtMaal = input$sentralmaal,
          enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
          tidsenhet = input$tidsenhetGjsn,
          session = session
        ),
        width = 1000, height = 350
      )

      output$lastNed_figGjsnTid <- shiny::downloadHandler(
        filename = function() {
          paste0("FigGjsnTid_", valgtVar = input$valgtVarGjsn, "_", Sys.time(),
                 ".'", input$bildeformatGjsn)
        },
        content = function(file) {
          nordicscir::NSFigGjsnTid(
            RegData = RegData, reshID = user$org(), preprosess = 0,
            valgtVar = input$valgtVarGjsn,
            datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
            datoUt = as.numeric(input$datoUtGjsn),
            minald = as.numeric(input$alderGjsn[1]),
            maxald = as.numeric(input$alderGjsn[2]),
            erMann = as.numeric(input$erMannGjsn),
            AIS = as.numeric(input$AISGjsn),
            traume = input$traumeGjsn,
            nivaaUt = as.numeric(input$paratetraGjsn),
            valgtMaal = input$sentralmaal,
            enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
            tidsenhet = input$tidsenhetGjsn,
            session = session,
            outfile = file
          )
        }
      )

      UtDataGjsnTid <- nordicscir::NSFigGjsnTid(
        RegData = RegData, reshID = user$org(), preprosess = 0,
        valgtVar = input$valgtVarGjsn,
        datoFra = input$datovalgGjsn[1], datoTil = input$datovalgGjsn[2],
        datoUt = as.numeric(input$datoUtGjsn),
        minald = as.numeric(input$alderGjsn[1]),
        maxald = as.numeric(input$alderGjsn[2]),
        erMann = as.numeric(input$erMannGjsn),
        AIS = as.numeric(input$AISGjsn),
        traume = input$traumeGjsn,
        nivaaUt = as.numeric(input$paratetraGjsn),
        valgtMaal = input$sentralmaal,
        enhetsUtvalg = as.numeric(input$enhetsUtvalgGjsn),
        tidsenhet = input$tidsenhetGjsn,
        session = session
      )

      tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
      grtxt <- UtDataGjsnTid$grtxt
      if ((min(nchar(grtxt)) == 5) && (max(nchar(grtxt)) == 5)) {
        grtxt <- paste(substr(grtxt, 1, 3), substr(grtxt, 4, 5))
      }
      rownames(tabGjsnTid) <- grtxt

      antKol <- ncol(tabGjsnTid)
      navnKol <- colnames(tabGjsnTid)
      if (antKol == 6) {
        colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])
      }

      output$gjsnTidTab <- function() {
        kableExtra::kable(
          tabGjsnTid,
          format = "html",
          full_width = FALSE,
          digits = 1
        ) %>%
          kableExtra::add_header_above(
            c(" " = 1, "Egen enhet/gruppe" = 3, "Resten" = 3)[1:(antKol/3 + 1)]
          ) %>%
          kableExtra::column_spec(column = 1, width_min = "7em") %>%
          kableExtra::column_spec(column = 2:(antKol + 1), width = "7em") %>%
          kableExtra::row_spec(0, bold = TRUE)
      }

      output$lastNed_gjsnTidTab <- shiny::downloadHandler(
        filename = function() {
          paste0(input$valgtVarGjsn, "_tabGjsnTid.csv")
        },
        content = function(file, filename) {
          write.csv2(tabGjsnTid, file, row.names = TRUE, fileEncoding = 'latin1', na = "")
        }
      )
    } else {
      output$gjsnGrVar <- NULL
      output$lastNed_figGjsnGrVar <- NULL
      output$tittelGjsnGrVar <- NULL
      output$gjsnGrVarTab <- NULL
      output$lastNed_tabGjsnGrVar <- NULL
      output$titteltabGjsnGrVar <- NULL
      output$gjsnTid <- NULL
      output$lastNed_figGjsnTid <- NULL
      output$gjsnTidTab <- NULL
      output$lastNed_gjsnTidTab <- NULL
    }
  }) #observe gjsn

  #-------Samlerapporter--------------------
  if (isDataOk) {
    contentFile <- function(file, srcFil, tmpFile, reshID = 0,
                            datoFra = startDato, datoTil = Sys.Date()) {
      src <- normalizePath(system.file(srcFil, package="nordicscir"))
      # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, tmpFile, overwrite = TRUE)
      knitr::knit2pdf(tmpFile)
      file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile) - 3), "pdf"), file)
    }

    output$mndRapp.pdf <- shiny::downloadHandler(
      filename = function() { paste0("MndRapp", Sys.time(), ".pdf")},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSmndRapp.Rnw",
          tmpFile = "tmpNSmndRapp.Rnw",
          reshID = user$org()
        )
      }
    )
    output$samleRappLand.pdf <- shiny::downloadHandler(
      filename = function() {"NorScirSamleRapportLand.pdf"},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSsamleRappLand.Rnw",
          tmpFile = "tmpNSsamleRappLand.Rnw",
          reshID = user$org(),
          datoFra = as.Date(input$datovalgSamleRapp[1]),
          datoTil = as.Date(input$datovalgSamleRapp[2])
        )
      }
    )
    output$samleRappEgen.pdf <- shiny::downloadHandler(
      filename = function() {"NorScirSamleRapportEgen.pdf"},
      content = function(file) {
        contentFile(
          file,
          srcFil = "NSsamleRapp.Rnw",
          tmpFile = "tmpNSsamleRapp.Rnw",
          reshID = user$org(),
          datoFra = as.Date(input$datovalgSamleRapp[1]),
          datoTil = as.Date(input$datovalgSamleRapp[2])
        )
      }
    )
  } else {
    output$mndRapp.pdf <- NULL
    output$samleRappLand.pdf <- NULL
    output$samleRappEgen.pdf <- NULL
  }


  #---Utsendinger---------------
  if (isDataOk) {
    sykehusNavn <- sort(
      unique(as.character(HovedSkjema$ShNavn)),
      index.return = TRUE
    )
    orgs <- c(0, unique(HovedSkjema$ReshId)[sykehusNavn$ix])
    names(orgs) <- c("Alle", sykehusNavn$x)
    orgs <- as.list(orgs)
  } else {
    orgs <- list(`Alle` = 0)
  }


  org <- rapbase::autoReportOrgServer("NSuts", orgs[-1])

  # oppdatere reaktive parametre, for å få inn valgte verdier
  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))

  vis_rapp <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  rapbase::autoReportServer(
    id = "NSuts",
    registryName = "nordicscir",
    type = "dispatchment",
    org = org$value,
    paramNames = paramNames,
    paramValues = paramValues,
    reports = list(
      MndRapp = list(
        synopsis = "Rapporteket-NorSCIR: Månedsrapport",
        fun = "abonnement",
        paramNames = c('rnwFil', "reshID", "register"),
        paramValues = c('NSmndRapp.Rnw', 0, 'norscir')
      ),
      SamleRapp = list(
        synopsis = "Rapporteket-NorSCIR: Rapport, div. resultater",
        fun = "abonnement",
        paramNames = c("rnwFil", "reshID", "register"),
        paramValues = c("NSsamleRapp.Rnw", 0, 'norscir')
      )
    ),
    orgs = orgs,
    eligible = vis_rapp,
    user = user
  )


  #Tørrkjøring av abonnement

  # kjor_autorapport <- shiny::observeEvent(input$run_autoreport, {
  #   dato <- input$rapportdato
  #   dryRun <- !(input$dryRun)
  #   withCallingHandlers({
  #     shinyjs::html("sysMessage", "")
  #     shinyjs::html("funMessage", "")
  #     shinyjs::html("funMessage",
  #                   rapbase::runAutoReport(group = "nordicscir",
  #                                          dato = dato, dryRun = dryRun))
  #   },
  #   message = function(m) {
  #     shinyjs::html(id = "sysMessage", html = m$message, add = TRUE)
  #   })
  # })

  #------------------ Abonnement -----------------------------------------------
  paramNamesAbb <- shiny::reactive("reshID")
  paramValuesAbb <- shiny::reactive(user$org())

  rapbase::autoReportServer(
    id = "ns-subscription",
    registryName = "nordicscir", #Must correspond to the registry R package name.
    type = "subscription",
    paramNames = paramNamesAbb,
    paramValues = paramValuesAbb,
    reports = list(
      # `Månedsrapport` = list(
      Maanedsrapport = list(
          synopsis = "Rapporteket-NorSCIR: månedsrapport, abonnement",
        fun = "abonnement",
        paramNames = c("rnwFil", "reshID", "register"),
        paramValues = c("NSmndRapp.Rnw", 0, 'norscir')
      )
    ),
    user = user
  )

    #----------- Eksport ----------------
  ## brukerkontroller
  rapbase::exportUCServer(
    "norscirExport",
    "norscir", # databasenavn
    "nordicscir" # navn på team, for tilhørighet på github
  )
  ## veileding
  rapbase::exportGuideServer("norscirExportGuide",
                             "norscir")
}
