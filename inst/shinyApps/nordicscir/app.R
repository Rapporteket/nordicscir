#Resultattjeneste for NordicScir
library(nordicscir)
library(shiny)
#library(shinyjs)
library(knitr)
library(lubridate)
library(dplyr)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)


startDato <- as.Date(paste0(as.numeric(format(Sys.Date()-400, "%Y")), '-01-01')) #Sys.Date()-400

# gjør Rapportekets www-felleskomponenter tilgjengelig for applikasjonen
addResourcePath('rap', system.file('www', package='rapbase'))

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
paaServer <- context %in% c("DEV", "TEST", "QA", "PRODUCTION") #rapbase::isRapContext()


regTitle = ifelse(paaServer,'Norsk ryggmargsskaderegister',
                  'Norsk ryggmargsskaderegister med FIKTIVE data')

#----Valg

valgAIS <- 0:5
names(valgAIS) <- c("Alle","A","B","C","D","E")
#valgAIS <- as.character(0:5),
#names(valgAIS) <- c('Alle', LETTERS[1:5]),

valgNivaaUt <- c(99,0,1,2,3,9)
names(valgNivaaUt) <- c("Alle", "Paraplegi", "Tetraplegi", "C1-4", "C5-8", "Ukjent")

tidsenheter <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                     'Kvartal'='Kvartal', 'Måned'='Mnd'))

ui <- tagList(
   shinyjs::useShinyjs(),
   navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      id='toppPaneler',
            # lag logo og tittel som en del av navbar
            #title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
      title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                  regTitle),
            # sett inn tittel også i browser-vindu
      windowTitle = regTitle,
            # velg css (foreløpig den eneste bortsett fra "naken" utgave)
      theme = "rap/bootstrap.css",
#----startside--------            
      tabPanel("Startside",
               #fluidRow(
               #column(width=5,
               #shinyjs::useShinyjs(),
               br(),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               
               sidebarPanel(width = 3,
                            br(),
                            #h2('Nedlastbare dokumenter med samling av resultater'),
                            
                            h3("Månedsrapport"), #),
                            downloadButton(outputId = "mndRapp.pdf", label='Last ned MÅNEDSRAPPORT', class = "butt"),
                            br(),
                            br('NB: Nedlasting tar litt tid. I mellomtida får man ikke sett på andre resultater.'),
                            br(),
                            br('Hvis du ønsker månedsrapporten regelmessig tilsendt på e-post, 
                               kan du gå til fanen "Abonnement" og bestille dette.'),
                            br(),
                            br(),
                            conditionalPanel(
                               condition = "input.startside == 'Status'",
                               h4('Velg tidsperiode for nevrologisk klassifikasjon og liggetider'),
                               dateRangeInput(inputId = 'datovalgDash', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"))
               ),
               mainPanel(width = 8,
                         shinyalert::useShinyalert(),
                         tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico")),
                         if (paaServer){ 
                            rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                      organization = uiOutput("appOrgName"),
                                                      addUserInfo = TRUE)},
                         h2('Velkommen til Rapporteket - Norsk Ryggmargsskaderegister!', align='center'),
                         br(),
                         tabsetPanel(
                            id = 'startside',
                            
                         tabPanel('Brukerveiledning',
                                  h4('Du er nå inne på Rapporteket for NorSCIR. Rapporteket er registerets resultattjeneste. 
                            Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret. 
                            På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert 
                            på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database. 
                            Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                                  #h4('For mer info og veiledning, se neste fane "Brukerveiledning"'),
                                  #br(),
                                  h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                            Resultatene som vises er 
                              basert på AdmitDt, altså dato for første akutte innleggelse. Alle figurer og 
                            tabeller kan lastes ned.'),
                                  h4(paste0('Se "nabofanen" Status for å se på nøkkeltall.')),
                                  br(),
                                  h4(tags$b(tags$u('Innhold i de ulike hovedfanene:'))),
                                  h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variable. 
                              Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                                  h4(tags$b('Gjennomsnitt per sykehus og over tid'), 'viser gjennomsnittsverdier per sykehus. 
                            Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt eller median. 
                            Man kan også velge å filtrere data.'),
                                  h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret. Også her kan man gjøre filtreringer.'),
                                  h4(tags$b('Abonnement'), 'inneholder oversikt over rapporter du abbonerer på. Her kan du også bestille abonnement, 
                            dvs. rapporter tilsendt på e-post.'),
                                  
                         h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                  a("NorSCIR", href="https://www.kvalitetsregistre.no/registers/561/resultater"),
                                  target="_blank", align='center'),
                         br(),
                         h4('Alle pasienter med nyervervet ryggmargsskade eller Cauda equina syndrom som legges 
                            inn til spesialisert rehabilitering ved en ryggmargsskadeavdeling, blir forespurt 
                            om samtykke til å bli registrert i Norsk ryggmargsskaderegister. Dette registeret 
                            har til hensikt å sikre og forbedre ryggmargsskadeomsorgen i Norge. Mer informasjon 
                            om selve registeret finnes på NorSCIRs hjemmeside: ', align='center',
                                     a("www.norscir.no", href="http://www.norscir.no", target="_blank"))
                         #h4('Mer informasjon om selve registeret finnes på NorSCIRs hjemmeside: ', align='center',
                         #         a("www.norscir.no", href="http://www.norscir.no", target="_blank")) #target gjør at lenken åpnes i ny fane
           
               ),
               tabPanel('Status',
                        
                        h4('Antall registreringer siste år:'),
                        #fluidRow(
                        tableOutput("tabAntOpphShMnd12startside"), #),
                        #         downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned'))
                        h5('(Mer informasjon om registreringsstatus finner du under fanen "Registreringsoversikter")'),
                        br(),
                        br(),
                        fluidRow(
                           column(width=6,
                                  h3('Nevrologisk klassifikasjon', align='center'),
                                  h4('alle pasienter', align = 'center'),
                                  br(),
                                  tableOutput('tabNevrKlass')),
                           column(width=6,
                                  h3('Nevrologisk klassifikasjon', align = 'center'),
                                  h4('pasienter med liggetid over 28 dager i
                                   ryggmargsskadeavdeling', align='center'),
                                  tableOutput('tabNevrKlass28')
                           )),
                        
                        fluidRow( 
                           h3('Liggetider, egen avdeling', align = 'left'),
                           tableOutput("tabLiggetider")),
                        br(),
               ))
               ) #main
      ), #tab

#--------Fordelinger-----------
      tabPanel("Fordelinger",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder',
                                              'Ais ved innleggelse' = 'AAis' ,
                                              'Ais ved utskriving' = 'FAis',
                                              'Anbefalt tid til kontroll' = 'AnbefTidKtr',
                                              'Lengde på rehab.opphold' = 'DagerRehab',
                                              'Opphold, totalt antall dager' = 'OpphTot',
                                              'Planlagt utskrevet til' = 'PPlaceDis',
                                              #'Fjern? Permisjon (ant. døgn ute av sykehus) ' = 'Permisjon',
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'Skadeårsak ' = 'SkadeArsak',
                                              'Skadeårsak, ikke-traumatisk' = 'Ntsci',
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab',
                                              'Tid med rehabilitering' = 'DagerRehab',
                                              'Utskrevet til' = 'UtTil',
                                              #'Fjern? Pustehjelp' = 'Pustehjelp[VentAssi]',
                                              'A&D Funksjon: Mobilitet' = 'FunkMob',
                                              'A&D Funksjon: Påkledning' = 'FunkKler',
                                              'A&D Funksjon: Spising' = 'FunkSpis',
                                              'A&D Funksjon: Toalett' = 'FunkDo',
                                              'A&D Tilfredshet: Mobilitet' = 'TilfMob',
                                              'A&D Tilfredshet: Påkledning' = 'TilfKler',
                                              'A&D Tilfredshet: Spising' = 'TilfSpis',
                                              'A&D Tilfredshet: Toalett' = 'TilfDo',
                                              'Livskval.: Tilfredshet med livet' = 'LivsGen',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk',
                                              'Urin: Ufrivillig urinlekkasje (fra 2019)' = 'UrinInkontinens',
                                              'Urin: Ufrivillig urinlekkasje (t.o.m. 2018)' = 'UrinInkontinensTom2018',
                                              'Urin: Kirurgiske inngrep' = 'UrinKirInngr',
                                              'Urin: Legemiddelbruk (fra 2019)' = 'UrinLegemidler',
                                              'Urin: Legemiddelbruk (t.o.m. 2018)' = 'UrinLegemidlerTom2018',
                                              'Urin: Legemiddelbruk, hvilke' = 'UrinLegemidlerHvilke',
                                              'Urin: Blæretømming, hovedmetode' = 'UrinTomBlareHoved',
                                              'Urin: Blæretømming, tilleggsmetode' = 'UrinTomBlareTillegg',
                                              'Tarm: Avføring, hovedmetode' = 'TarmAvfHoved',
                                              'Tarm: Avføring, tilleggsmetode' = 'TarmAvfTillegg',
                                              'Tarm: Avføringsmiddelbruk' = 'TarmAvfmiddel',
                                              'Tarm: Avføringsmidler, hvilke' = 'TarmAvfmiddelHvilke',
                                              'Tarm: Fekal inkontinens (fra 2019)' = 'TarmInkontinensFra2019',
                                              'Tarm: Fekal inkontinens (t.o.m. 2018)' = 'TarmInkontinensTom2018',
                                              'Tarm: Kirurgisk inngrep' = 'TarmKirInngrep',
                                              'Tarm: Kirurgiske inngrep, hvilke' = 'TarmKirInngrepHvilke'
                                  )
                                  , selected = c('Registreringsforsinkelse' = 'RegForsinkelse')
                            ),
                            dateRangeInput(inputId = 'datovalg', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            radioButtons(inputId = 'datoUt', 'Bruk utskrivingsdato til datofiltrering?',
                                         choiceNames = c('nei','ja'), choiceValues = 0:1, selected = 0),
                            selectInput(inputId = "erMann", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1,
                                                    "Hele landet"=0,
                                                    "Egen enhet"=2)
                            ),
                            selectInput(inputId = 'AIS', label='AIS-grad ved utreise',
                                        multiple = T, #selected=0,
                                        choices = valgAIS
                             ),
                            selectInput(inputId = 'traume', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja',
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'nivaaUt', label='Nivå ved utreise',
                                        choices = valgNivaaUt
                            ),
                            selectInput(inputId = "bildeformatFord",
                                        label = "Velg format for nedlasting av figur",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')
                                        )
                            ),
               mainPanel(width = 6,
                         tabsetPanel( id='fordeling',
                                      tabPanel(
                                         'Figur',
                                         br(),
                                         em('(Høyreklikk på figuren for å laste den ned)'),
                                         br(),
                                         br(),
                                         plotOutput('fordelinger', height = 'auto'),
                                         downloadButton(outputId = 'lastNed_figFord', label='Last ned figur'),
                                         hr()
                                      ) ,
                                      tabPanel(
                                         'Figur, alle sykehus',
                                         plotOutput('fordelingPrSh', height = 'auto'),
                                         downloadButton(outputId = 'lastNed_figFordSh', label='Last ned figur')),
                                      tabPanel(
                                         'Tabell',
                                         uiOutput("tittelFord"),
                                         br(),
                                         tableOutput('fordelingTab'),
                                         downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell'),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         #hr(),
                                         #conditionalPanel(condition = (rolle=='SC'),
                                         tableOutput('fordelingShTab'),
                                         downloadButton(outputId = 'lastNed_tabFordSh', label='Last ned')
                                         #)
                                      )
                         )) #mainPanel
      ), #tab Fordelinger


#------------ Gjennomsnitt ------------
      tabPanel("Gjennomsnitt per sykehus og over tid",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVarGjsn", label="Velg variabel",
                                  choices = c('Alder' = 'Alder',
                                              'Lengde på rehab.opphold' = 'DagerRehab',
                                              'Opphold, totalt antall dager' = 'OpphTot',
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab',
                                              'Livskval.: Tilfredshet med livet' = 'LivsGen',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk'
                                             )
                                  , selected = c('Registreringsforsinkelse' = 'RegForsinkelse')
                            ),
                            dateRangeInput(inputId = 'datovalgGjsn', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            radioButtons(inputId = 'datoUtGjsn', 'Bruk utskrivingsdato til datofiltrering?',
                                         choiceNames = c('nei','ja'), choiceValues = 0:1, selected = 0),
                            selectInput(inputId = "erMannGjsn", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alderGjsn", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'AISGjsn', label='AIS-grad ved utreise',
                                        multiple = T, #selected=0,
                                         choices = valgAIS
                            ),
                            selectInput(inputId = 'traumeGjsn', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja',
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'paratetraGjsn', label='Nivå ved utreise',
                                        choices = c("Alle" = 99,
                                                    "Paraplegi" = 0,
                                                    "Tetraplegi" = 1,
                                                    "Ukjent" = 9)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med')),
                            br(),
                            p(em('Følgende utvalg gjelder bare figuren/tabellen som viser utvikling over tid')),
                            selectInput(inputId = 'enhetsUtvalgGjsn', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1, "Hele landet"=0, "Egen enhet"=2)
                            ),
                            selectInput(inputId = "tidsenhetGjsn", label="Velg tidsenhet",
                                        choices = tidsenheter
                            ),
                            selectInput(inputId = "bildeformatGjsn",
                                        label = "Velg format for nedlasting av figur",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')
                            )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 br(),
                                 h3(em("Utvikling over tid")),
                                 plotOutput("gjsnTid", height = 'auto'),
                                 downloadButton(outputId = 'lastNed_figGjsnTid', label='Last ned figur'),
                                 br(),
                                 h3(em("Sykehusvise resultater")),
                                 plotOutput('gjsnGrVar', height = 'auto'),
                                 downloadButton(outputId = 'lastNed_figGjsnGrVar', label='Last ned figur')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelGjsnGrVar"),
                                 br(),
                                 tableOutput('gjsnTidTab'),
                                 br(),
                                 tableOutput('gjsnGrVarTab'),
                                 downloadButton(outputId = 'lastNed_tabGjsnGrVar', label='Last ned') # , class = "butt"))
                           )
                     )
               )

), #Gjsn



#-----Registreringsoversikter------------
tabPanel("Registreringsoversikter",
         sidebarPanel(width=3,
                      h3('Utvalg'),
                      conditionalPanel(condition = "input.ark == 'Antall personer med ryggmargsskade'",
                                       dateInput(inputId = 'sluttDatoReg', label = 'Velg sluttdato', language="nb",
                                                 value = Sys.Date(), max = Sys.Date() )
                      ),
                      conditionalPanel(
                            condition = "input.ark == 'Antall personer med ryggmargsskade'",
                            selectInput(inputId = "tidsenhetReg", label="Velg tidsenhet",
                                        choices = rev(c('År'= 'Aar', 'Måned'='Mnd')))),
                      # conditionalPanel(
                      #       condition = "input.ark == 'Antall personer med ryggmargsskade'",
                      #       selectInput(inputId = 'enhetsNivaa', label='Enhetsnivå',
                      #                   choices = c("Hele landet" = 0, "Egen enhet" = 2))
                      # ),
                      conditionalPanel(
                            condition = "input.ark == 'Antall personer med ryggmargsskade'",
                            selectInput(inputId = 'traumeReg', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja',
                                                    "Ikke traume"='nei'))
                      ),
                      conditionalPanel(
                            condition = "input.ark == 'Antall hovedskjema med tilknyttede skjema' |
                            input.ark == 'Antall kontrollskjema med tilknyttede skjema' ",
                            dateRangeInput(inputId = 'datovalgReg', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb")
                      )
         ),

         mainPanel(
               tabsetPanel(id='ark',
                           tabPanel('Antall personer med ryggmargsskade',
                                    #p('Tabellen viser registreringer siste 12 måneder eller siste 5 år'),
                                    uiOutput("undertittelReg"),
                                    p("Velg tidsperiode ved å velge sluttdato/tidsenhet i menyen til venstre"), #em(
                                    br(),
                                    fluidRow(tableOutput("tabAntOpphShMnd12"),
                                             downloadButton(outputId = 'lastNed_tabAntOpph', label='Last ned')
                                    ),
                                    #                                          br(),
                                    # h3("Belegg FJERNES! på rehabiliteringsavdelinga - ønskes flere/andre variable?"),
                                    # #uiOutput("undertittelBelegg"),
 
                           ),
                           tabPanel('Antall hovedskjema med tilknyttede skjema',
                                    h3("Antall hovedskjema med tilknyttede skjema"),
                                    tableOutput('tabAntTilknyttedeHovedSkjema'),
                                    downloadButton(outputId = 'lastNed_tabOppfHovedAnt', label='Last ned'),
                                    br(),
                                    h3("Andel (%) hovedskjema med tilknyttede skjema"),
                                    tableOutput("tabAndelTilknyttedeHovedSkjema"),
                                    downloadButton(outputId = 'lastNed_tabOppfHovedPst', label='Last ned')
                           ),
                           tabPanel('Antall kontrollskjema med tilknyttede skjema',
                                    h3("Antall kontrollskjema med tilknyttede skjema"),
                                    h5('Datoutvalg er basert på dato for kontroll'),
                                    tableOutput('tabAntTilknyttedeKtrSkjema'),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrAnt', label='Last ned'),
                                    br(),
                                    h3("Andel (%) kontrollskjema med tilknyttede skjema"),
                                    tableOutput("tabAndelTilknyttedeKtrSkjema"),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrPst', label='Last ned')
                           )

               ))
), #tab Registreringsoversikter

#----------------------Registeradministrasjon--------------------------------

tabPanel("Registeradministrasjon",
         h2('Fane som bare er synlig for SC-bruker.'),
         
         tabsetPanel(id='ark',
                     tabPanel('Samlerapporter',
                              sidebarPanel(width=3,
                                           h3('Utvalg'),
                                           dateRangeInput(inputId = 'datovalgSamleRapp', start = startDato-150, end = Sys.Date(),
                                                          label = "Tidsperiode", separator="t.o.m.", language="nb")
                              ),
                              
                              mainPanel(
                                    br(),
                                    br(),
                                    h3("Resultater, hele landet"), #),
                                    #shinydashboard::infoBox("Resultater hele landet ",
                                    downloadButton(outputId = 'samleRappLand.pdf',
                                                   label='Last ned samlerapport, hele landet', class = "butt"),
                                    br(),
                                    h3("Resultater eget sykehus"), #),
                                    downloadButton(outputId = 'samleRappEgen.pdf',
                                                   label='Last ned egen samlerapport', class = "butt"),
                                    br(),
                                    br()
                              )
                     ),
                     tabPanel("Utsendinger",
                              title = "Utsending av rapporter",
                              sidebarLayout(
                                    sidebarPanel(
                                          rapbase::autoReportOrgInput("NSuts"),
                                          rapbase::autoReportInput("NSuts")
                                          ),
                                    mainPanel(
                                          rapbase::autoReportUI("NSuts")
                                          )
                                    )
                              ),
                     tabPanel("Eksport, krypterte data",
                           #shiny::sidebarLayout(
                           shiny::sidebarPanel(
                                 rapbase::exportUCInput("nordicscirExport")
                           ),
                           shiny::mainPanel(
                                 rapbase::exportGuideUI("nordicscirExportGuide")
                           )
                           #)
                     ) #Eksport-tab
         ) #tabsetPanel
), #Registeradm-tab

#------------------Abonnement------------------------
tabPanel(p("Abonnement",
           title='Bestill automatisk utsending av månedsrapport på e-post'),
         sidebarLayout(
            sidebarPanel(width = 3,
                         selectInput("subscriptionRep", "Rapport:",
                                     c("Månedsrapport")),
                         selectInput("subscriptionFreq", "Frekvens:",
                                     list(Årlig="Årlig-year",
                                           Kvartalsvis="Kvartalsvis-quarter",
                                           Månedlig="Månedlig-month",
                                           Ukentlig="Ukentlig-week",
                                     Daglig="Daglig-DSTday"),
                                     selected = "Månedlig-month"),
                         #selectInput("subscriptionFileFormat", "Format:",
                         #            c("html", "pdf")),
                         actionButton("subscribe", "Bestill!")
            ),
            mainPanel(
               uiOutput("subscriptionContent")
            )
         )
) #Tab abonnement

#-----------------slutt-----------------------------
) #navbar
) #tagList




#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {
      
   #-----------Div serveroppstart------------------  
   raplog::appLogger(session = session, msg = "Starter nordicscir-app'en. Data fra NorSCIR vil bli hentet")
      #system.file('NSmndRapp.Rnw', package='nordicscir')
      #system.file('NSsamleRapp.Rnw', package='nordicscir')
   
      #hospitalName <-getHospitalName(rapbase::getUserReshId(session))
      #reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 107627)}) 
      reshID <- ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 107627)
      rolle <- ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC') #reactive({})
      brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(shinySession=session), 'tullebukk')})
      #output$reshID <- renderText({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)}) #evt renderUI
      
      observe({if (rolle != 'SC') {
            
      #NB: Må aktiveres i ui-del også OK
            shinyjs::hide(id = 'samleRappLand.pdf')
         hideTab(inputId = "toppPaneler", target = "Registeradministrasjon")
         hideTab(inputId = "fordeling", target = "Figur, alle sykehus")
         shinyjs::hide(id = 'fordelingShTab')
         shinyjs::hide(id = 'lastNed_tabFordSh')
         #shinyjs::hide(id = 'fordelingPrSh')
            #hideTab(inputId = "tabs_andeler", target = "Figur, sykehusvisning")
      }
      })
      
      # widget
      if (paaServer) {
         output$appUserName <- renderText(rapbase::getUserFullName(session))
         output$appOrgName <- renderText(paste0('rolle: ', rolle, '<br> ReshID: ', reshID) )}
      
      # User info in widget
      userInfo <- rapbase::howWeDealWithPersonalData(session)
      observeEvent(input$userInfo, {
         shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                                type = "", imageUrl = "rap/logo.svg",
                                closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                                html = TRUE, confirmButtonText = rapbase::noOptOutOk())
      })
      
 #NB: Skal bare forholde oss til oppfølgingsskjema som er tilknyttet et gyldig Hovedskjema
      #paaServer <- (context == "TEST" | context == "QA" | context == "PRODUCTION")
      if (paaServer) {
                  HovedSkjema <- NSRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
                  LivskvalH <- NSRegDataSQL(valgtVar='LivsXX')
                  KontrollH <- NSRegDataSQL(valgtVar='KontXX')
                  UrinH <- NSRegDataSQL(valgtVar='UrinXX')
                  TarmH <- NSRegDataSQL(valgtVar='TarmXX')
                  AktivFunksjonH <- NSRegDataSQL(valgtVar='FunkXX')
                  AktivTilfredshetH <- NSRegDataSQL(valgtVar='TilfXX')
      } 

      if (!exists('HovedSkjema')){
            #Tulledata:
            data('NordicScirFIKTIVEdata', package = 'nordicscir') #NB: Ikke koblede data

            Livskval$HovedskjemaGUID <- toupper(Livskval$HovedskjemaGUID)
            Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
            Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
            Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
            AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
            AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)
           
            #HovedSkjema <- NSPreprosesser(HovedSkjema) 
            LivskvalH <- KobleMedHoved(HovedSkjema,Livskval)
            KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
            UrinH <- KobleMedHoved(HovedSkjema,Urin)
            TarmH <- KobleMedHoved(HovedSkjema,Tarm)
            AktivFunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
            Aktivitet <- KobleMedHoved(HovedSkjema = AktivFunksjon, Skjema2 = AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
            AktivTilfredshetH <- KobleMedHoved(HovedSkjema, Aktivitet)
      }
      
      
      HovedSkjema <- NSPreprosesser(HovedSkjema)
      LivskvalH <- NSPreprosesser(LivskvalH)
      KontrollH <- NSPreprosesser(KontrollH)
      UrinH <- NSPreprosesser(UrinH)
      TarmH <- NSPreprosesser(TarmH)
      AktivFunksjonH <- NSPreprosesser(AktivFunksjonH)
      AktivTilfredshetH <- NSPreprosesser(AktivTilfredshetH)
      
      AlleTab <- list(HovedSkjema=HovedSkjema, 
                      LivskvalH=LivskvalH, 
                      KontrollH=KontrollH, 
                      UrinH=UrinH, 
                      TarmH=TarmH, 
                      AktivFunksjonH = AktivFunksjonH, 
                      AktivTilfredshetH = AktivTilfredshetH)
      
 
#--------------Startside------------------------------
      
      output$lenkeNorScir <- renderUI({tagList("www.norscir.no", www.norscir.no)})
      output$tabAntOpphShMnd12startside <- renderTable({tabAntOpphShMnd(RegData=HovedSkjema, antMnd=12)}, 
                                                       rownames = T, digits=0, spacing="xs")
      observe({
      output$tabNevrKlass <- renderTable(
            lagTabNevrKlass(HovedSkjema, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2]),
            rownames=T
      )

      output$tabNevrKlass28 <- renderTable({
            HovedSkjema28 <- HovedSkjema[which(HovedSkjema$DagerRehab >28),]
            lagTabNevrKlass(HovedSkjema28, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2])},
            rownames=T
      )

      output$tabLiggetider <- renderTable({
            tabLiggetider(RegData = HovedSkjema, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2],
                          enhetsUtvalg=2, reshID=reshID)
         #tabLiggetider(RegData = HovedSkjema, datoFra = '2019-01-01', enhetsUtvalg=0, reshID=105593)
         },
            rownames=T,
            digits = 0
      )
      # output$tabLiggetider <- function() {
      #       tabLigget <- tabLiggetider(RegData = HovedSkjema, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2])
      #       kableExtra::kable(tabLigget, format = 'html'
      #                         , full_width=F
      #                         , digits = c(0,0,0,1,0)
      #       )}
      })
      
 #----------Tabeller, registreringsoversikter ----------------------           
            # output$tabLiggetider <- renderTable(
            # tabLiggetider(RegData = HovedSkjema, datoTil=input$sluttDatoReg, tidsenhet=input$tidsenhetReg, 
            #           enhetsUtvalg=as.numeric(input$enhetsNivaa), reshID=reshID)
            # , rownames = T, digits=0, spacing="xs"
            # )

            output$undertittelReg <- renderUI({
                  br()
                  t1 <- 'Tabellen viser innleggelser '
                  t2 <- ', basert på første akutte innleggelse'
                  h4(HTML(switch(input$tidsenhetReg, #undertittel <- 
                         Mnd = paste0(t1, 'siste 12 måneder før ', input$sluttDatoReg, t2, '<br />'),
                         Aar = paste0(t1, 'siste 5 år før ', input$sluttDatoReg, t2, '<br />'))
                  ))})
      observe({
         
            tabAntOpphShMndAar <- switch(input$tidsenhetReg,
             Mnd=tabAntOpphShMnd(RegData=HovedSkjema, datoTil=input$sluttDatoReg, traume=input$traumeReg, antMnd=12), #input$datovalgTab[2])  
             Aar=tabAntOpphSh5Aar(RegData=HovedSkjema, datoTil=input$sluttDatoReg, traume=input$traumeReg))
            
            
            output$tabAntOpphShMnd12 <- renderTable({tabAntOpphShMndAar}, rownames = T, digits=0, spacing="xs")
      output$lastNed_tabAntOpph <- downloadHandler(
            filename = function(){paste0('tabAntOpph.csv')},
            content = function(file, filename){write.csv2(tabAntOpphShMndAar, file, row.names = T, na = '')
            })
      
     
      
            #Antall skjema av alle typer.
            tabTilknHovedSkjema <- tabSkjemaTilknyttet(Data=AlleTab, moderSkjema = 'Hoved',
                                                       datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2])
            
      #Hovedskjema som har tilknyttede skjema av ulik type
            output$tabAntTilknyttedeHovedSkjema <- renderTable(
                  tabTilknHovedSkjema$Antall
                  ,rownames = T, digits=0, spacing="xs" )
            
            output$lastNed_tabOppfHovedAnt <- downloadHandler(
                  filename = function(){'tabOppfHovedAnt.csv'},
                  content = function(file, filename){write.csv2(tabTilknHovedSkjema$Antall, file, row.names = T, na = '')
                  })
            #Andel (prosent) av registreringsskjemaene som har oppfølgingsskjema.      
            output$tabAndelTilknyttedeHovedSkjema <- renderTable(
                  tabTilknHovedSkjema$Andeler
                  ,rownames = T, digits=0, spacing="xs" )
            
             output$lastNed_tabOppfHovedPst <- downloadHandler(
                   filename = function(){'tabOppfHovedPst.csv'},
                   content = function(file, filename){write.csv2(tabTilknHovedSkjema$Andeler, file, row.names = T, na = '')
                   })

             #Kontrollskjema som har tilknyttede skjema av ulik type
             tabTilknKtrSkjema <- tabSkjemaTilknyttet(Data=AlleTab, moderSkjema = 'Ktr',
                                                        datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2])
             
             output$tabAntTilknyttedeKtrSkjema <- renderTable(
                   tabTilknKtrSkjema$Antall
                   ,rownames = T, digits=0, spacing="xs" )
             
             output$lastNed_tabOppfKtrAnt <- downloadHandler(
                   filename = function(){'tabOppfKtrAnt.csv'},
                   content = function(file, filename){write.csv2(tabTilknKtrSkjema$Antall, file, row.names = T, na = '')
                   })
             #Andel (prosent) av kontrollskjemaene som har oppfølgingsskjema.      
             output$tabAndelTilknyttedeKtrSkjema <- renderTable(
                   tabTilknKtrSkjema$Andeler
                   ,rownames = T, digits=0, spacing="xs" )
             
             output$lastNed_tabOppfKtrPst <- downloadHandler(
                   filename = function(){'tabOppfKtrPst.csv'},
                   content = function(file, filename){write.csv2(tabTilknKtrSkjema$Andeler, file, row.names = T, na = '')
                   })
             
      })
 
      #Antall skjema av hver type
      # output$AntallSkjema <- renderTable(
      #       t(tabAntSkjema(Data=AlleTab, datoFra=input$datovalgReg[1], datoTil=input$datovalgReg[2]))
      #       ,rownames = T, digits=0, spacing="xs" )
     

#---------Fordelinger------------
            observe({   #Fordelingsfigurer og tabeller
            #RegData <- finnRegData(Data = AlleTab, valgtVar <-'UrinLegemidlerHvilke')
            RegData <- finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
            RegData <- TilLogiskeVar(RegData)
            
            #NSFigAndeler(RegData=RegData, valgtVar='UrinLegemidlerHvilke', preprosess = 0)
            
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID, 
                               AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                               datoUt=as.numeric(input$datoUt),
                               session=session)
            }, height=800, width=800 #height = function() 
            )
            
            output$lastNed_figFord <- downloadHandler(
               filename = function(){
                  paste0('Fordeling_', valgtVar=input$valgtVar, '_', Sys.time(), '.', input$bildeformatFord)
               },
               content = function(file){
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               datoUt=as.numeric(input$datoUt),
                               reshID = reshID, 
                               AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                               session=session,
                                   outfile = file)
               })
            
            
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       datoUt=as.numeric(input$datoUt),
                                       reshID = reshID, 
                                       AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                       session=session)
            
            output$tittelFord <- renderUI({
               tagList(
                  h3(UtDataFord$tittel),
                  h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
               )}) 
            
            tabFord <- lagTabavFigAndeler(UtDataFraFig = UtDataFord)
            
            output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
               antKol <- ncol(tabFord)
               kableExtra::kable(tabFord, format = 'html'
                                 , full_width=F
                                 , digits = c(0,1,0,1)[1:antKol]
               ) %>%
                  add_header_above(c(" "=1, 'Egen enhet/gruppe' = 2, 'Resten' = 2)[1:(antKol/2+1)]) %>%
                  column_spec(column = 1, width_min = '7em') %>%
                  column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
                  row_spec(0, bold = T)
            }
            output$lastNed_tabFord <- downloadHandler(
               filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
               content = function(file, filename){write.csv2(tabFord, file, row.names = F, na = '')
               })
            
            
            output$fordelingPrSh <- renderPlot({
                  NSFigAndelerSh(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], datoUt=as.numeric(input$datoUt),
                               AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               session=session)
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )

            output$lastNed_figFordSh <- downloadHandler(
               filename = function(){
                  paste0('FordelingPrSh_', valgtVar=input$valgtVar, '_', Sys.time(), '.', input$bildeformatFord)
               },
               
               content = function(file){
                  NSFigAndelerSh(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                                 datoFra=input$datovalg[1], datoTil=input$datovalg[2], datoUt=as.numeric(input$datoUt),
                                 AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                 minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                 erMann=as.numeric(input$erMann), 
                                 session=session,
                               outfile = file)
               }
            )
            
                        UtDataFordSh <- NSFigAndelerSh(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       datoUt=as.numeric(input$datoUt),
                                       AIS=as.numeric(input$AIS), traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       session=session)
            
             tabFordSh <- lagTabavFigAndelerSh(UtDataFraFig = UtDataFordSh)
             
            output$fordelingShTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
               antKol <- ncol(tabFordSh)
               kableExtra::kable(tabFordSh, format = 'html'
                                        , full_width=F
                                        , digits = c(0,0,0,1,1,1)[1:antKol]
               ) %>%
                  add_header_above(header= c(" "=1, 'Antall' = 3, 'Andel' = 3))  %>% #[1:(antKol/2+1)])
                  column_spec(column = 1, width_min = '7em') %>%
                  column_spec(column = 2:(ncol(tabFordSh)+1), width = '7em') %>%
                  row_spec(0, bold = T)
            }
            output$lastNed_tabFordSh <- downloadHandler(
               filename = function(){paste0(input$valgtVar, '_fordelingSh.csv')},
               content = function(file, filename){write.csv2(tabFordSh, file, row.names = F, na = '')
               })
            
      }) #observe Fordeling
      
      #------------------Sykehusvise gjennomsnitt, figur og tabell-------------------
      observe({ 
            RegData <- finnRegData(valgtVar = input$valgtVarGjsn, Data = AlleTab)
            output$gjsnGrVar <- renderPlot(
                  NSFigGjsnGrVar(RegData=RegData, preprosess = 0, 
                                 valgtVar=input$valgtVarGjsn,
                                 datoFra=input$datovalgGjsn[1], 
                                 datoTil=input$datovalgGjsn[2], 
                                 datoUt=as.numeric(input$datoUtGjsn),
                                 AIS=as.numeric(input$AISGjsn), 
                                 traume=input$traumeGjsn, 
                                 nivaaUt=as.numeric(input$paratetraGjsn),
                                 minald=as.numeric(input$alderGjsn[1]), 
                                 maxald=as.numeric(input$alderGjsn[2]), 
                                 erMann=as.numeric(input$erMannGjsn), 
                                 valgtMaal = input$sentralmaal, session=session
                  ),
                  width = 800, height = 600)

            output$lastNed_figGjsnGrVar <- downloadHandler(
               filename = function(){
                  paste0('FigGjsn_', valgtVar=input$valgtVarGjsn, '_', Sys.time(), '.', input$bildeformatGjsn)
               },
               content = function(file){
                  NSFigGjsnGrVar(RegData=RegData, preprosess = 0, 
                                 valgtVar=input$valgtVarGjsn,
                                 datoFra=input$datovalgGjsn[1], 
                                 datoTil=input$datovalgGjsn[2], 
                                 datoUt=as.numeric(input$datoUtGjsn),
                                 AIS=as.numeric(input$AISGjsn), 
                                 traume=input$traumeGjsn, 
                                 nivaaUt=as.numeric(input$paratetraGjsn),
                                 minald=as.numeric(input$alderGjsn[1]), 
                                 maxald=as.numeric(input$alderGjsn[2]), 
                                 erMann=as.numeric(input$erMannGjsn), 
                                 valgtMaal = input$sentralmaal, session=session,
                               outfile = file)
               })
            
                        UtDataGjsnGrVar <- NSFigGjsnGrVar(RegData=RegData, preprosess = 0, 
                                              valgtVar=input$valgtVarGjsn,
                                              datoFra=input$datovalgGjsn[1], 
                                              datoTil=input$datovalgGjsn[2], 
                                              datoUt=as.numeric(input$datoUtGjsn),
                                              AIS=as.numeric(input$AISGjsn), 
                                              traume=input$traumeGjsn, 
                                              nivaaUt=as.numeric(input$paratetraGjsn),
                                              minald=as.numeric(input$alderGjsn[1]), 
                                              maxald=as.numeric(input$alderGjsn[2]), 
                                              erMann=as.numeric(input$erMannGjsn), 
                                              valgtMaal = input$sentralmaal,
                                              session=session)
            tabGjsnGrVar <- lagTabavFigGjsnGrVar(UtDataFraFig = UtDataGjsnGrVar)
            
            output$tittelGjsnGrVar <- renderUI({
                  tagList(
                        h3(UtDataGjsnGrVar$tittel),
                        h5(HTML(paste0(UtDataGjsnGrVar$utvalgTxt, '<br />')))
                  )}) #, align='center'
            # output$gjsnGrVarTab <- renderTable(
            #       tabGjsnGrVar, rownames = T)
            
            output$gjsnGrVarTab <- function() { 
                  antKol <- ncol(tabGjsnGrVar)
                  kableExtra::kable(tabGjsnGrVar, format = 'html'
                                    #, full_width=T
                                    , digits = c(0,1) #,0,1)[1:antKol]
                  ) %>%
                        column_spec(column = 1, width_min = '5em') %>%
                        column_spec(column = 2:(antKol+1), width = '4em') %>%
                        row_spec(0, bold = T)
            }
            output$lastNed_tabGjsnGrVar <- downloadHandler(
                  filename = function(){
                        paste0(input$valgtVar, '_tabGjsnSh .csv')
                  },
                  content = function(file, filename){
                        write.csv2(tabGjsnGrVar, file, row.names = T, na = '')
                  })
            
            output$titteltabGjsnGrVar <- renderUI({
                  tagList(
                        h3(tabGjsnGrVar$tittel),
                        h5(HTML(paste0(tabGjsnGrVar$utvalgTxt, '<br />')))
                  )}) #, align='center'
            
            
            
            
            #------gjsnTid
            output$gjsnTid <- renderPlot(
               NSFigGjsnTid(RegData=RegData, reshID= reshID, preprosess = 0, valgtVar=input$valgtVarGjsn,
                              datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                            datoUt=as.numeric(input$datoUtGjsn),
                              minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                            erMann=as.numeric(input$erMannGjsn),
                            AIS=as.numeric(input$AISGjsn),  
                            traume=input$traumeGjsn, 
                            nivaaUt=as.numeric(input$paratetraGjsn),
                            valgtMaal = input$sentralmaal, 
                            enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                              tidsenhet = input$tidsenhetGjsn,
                              session = session
               ),
               width = 1000, height = 350)
            
            output$lastNed_figGjsnTid <- downloadHandler(
               filename = function(){
                  paste0('FigGjsnTid_', valgtVar=input$valgtVarGjsn, '_', Sys.time(), '.', input$bildeformatGjsn)
               },
               content = function(file){
                  NSFigGjsnTid(RegData=RegData, reshID= reshID, preprosess = 0, valgtVar=input$valgtVarGjsn,
                               datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                               datoUt=as.numeric(input$datoUtGjsn),
                               minald=as.numeric(input$alderGjsn[1]), maxald=as.numeric(input$alderGjsn[2]),
                               erMann=as.numeric(input$erMannGjsn),
                               AIS=as.numeric(input$AISGjsn),  
                               traume=input$traumeGjsn, 
                               nivaaUt=as.numeric(input$paratetraGjsn),
                               valgtMaal = input$sentralmaal, 
                               enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                               tidsenhet = input$tidsenhetGjsn,
                               session = session,
                                 outfile = file)
               })
            
            UtDataGjsnTid <- NSFigGjsnTid(RegData=RegData, reshID= reshID, preprosess = 0,
                                            valgtVar=input$valgtVarGjsn,
                                            datoFra=input$datovalgGjsn[1], datoTil=input$datovalgGjsn[2],
                                          datoUt=as.numeric(input$datoUtGjsn),
                                          minald=as.numeric(input$alderGjsn[1]),
                                            maxald=as.numeric(input$alderGjsn[2]),
                                          erMann=as.numeric(input$erMannGjsn),
                                          AIS=as.numeric(input$AISGjsn), 
                                          traume=input$traumeGjsn, 
                                          nivaaUt=as.numeric(input$paratetraGjsn),
                                          valgtMaal = input$sentralmaal,
                                            enhetsUtvalg =  as.numeric(input$enhetsUtvalgGjsn),
                                          tidsenhet = input$tidsenhetGjsn,
                                          session = session)
            
            tabGjsnTid <- t(UtDataGjsnTid$AggVerdier)
            grtxt <-UtDataGjsnTid$grtxt
            if ((min(nchar(grtxt)) == 5) & (max(nchar(grtxt)) == 5)) {
               grtxt <- paste(substr(grtxt, 1,3), substr(grtxt, 4,5))}
            rownames(tabGjsnTid) <- grtxt
            
            antKol <- ncol(tabGjsnTid)
            navnKol <- colnames(tabGjsnTid)
            if (antKol==6) {colnames(tabGjsnTid) <- c(navnKol[1:3], navnKol[1:3])}
            
            output$gjsnTidTab <- function() {
               kableExtra::kable(tabGjsnTid, format = 'html'
                                 , full_width=F
                                 , digits = 1 #c(0,1,1,1)[1:antKol]
               ) %>%
                  add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                  #add_header_above(c(" "=1, 'Egen enhet/gruppe' = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                  column_spec(column = 1, width_min = '7em') %>%
                  column_spec(column = 2:(antKol+1), width = '7em') %>%
                  row_spec(0, bold = T)
            }
            
            output$lastNed_gjsnTidTab <- downloadHandler(
               filename = function(){
                  paste0(input$valgtVarGjsn, '_tabGjsnTid .csv')
               },
               content = function(file, filename){
                  write.csv2(tabGjsnTid, file, row.names = T, na = '')
               })
            
            
            
            
      }) #observe gjsn
      
      

#-------Samlerapporter--------------------      
      contentFile <- function(file, srcFil, tmpFile, 
                              reshID=0, datoFra=startDato, datoTil=Sys.Date()) {
            src <- normalizePath(system.file(srcFil, package="nordicscir"))
            #dev.off()
            # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, tmpFile, overwrite = TRUE)
            knitr::knit2pdf(tmpFile)
            
            gc() #Opprydning gc-"garbage collection"
            file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
            # file.rename(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
      }
      
      output$mndRapp.pdf <- downloadHandler(
            filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, #'MndRapp.pdf',
            content = function(file){
                  contentFile(file, srcFil="NSmndRapp.Rnw", tmpFile="tmpNSmndRapp.Rnw",
                              reshID = reshID) #, datoFra = startDato, datoTil = Sys.Date())
            })
      output$samleRappLand.pdf <- downloadHandler(
            filename = function(){'NorScirSamleRapportLand.pdf'}, # downloadFilename('NorScirSamleRapport')
            content = function(file){
                  contentFile(file, srcFil="NSsamleRappLand.Rnw", tmpFile="tmpNSsamleRappLand.Rnw",
                              reshID=reshID, 
                              datoFra = as.Date(input$datovalgSamleRapp[1]), 
                              datoTil = as.Date(input$datovalgSamleRapp[2]))
            })
      output$samleRappEgen.pdf <- downloadHandler(
            filename = function(){'NorScirSamleRapportEgen.pdf'}, # downloadFilename('NorScirSamleRapport')
            content = function(file){
                  contentFile(file, srcFil="NSsamleRapp.Rnw", tmpFile="tmpNSsamleRapp.Rnw",
                              reshID=reshID, 
                              datoFra = as.Date(input$datovalgSamleRapp[1]), 
                              datoTil = as.Date(input$datovalgSamleRapp[2]))
            })
      
      
#------------------ Abonnement ----------------------------------------------
      ## reaktive verdier for å holde rede på endringer som skjer mens
      ## applikasjonen kjører
      rv <- reactiveValues(
         subscriptionTab = rapbase::makeAutoReportTab(session)) #makeUserSubscriptionTab(session))
      
      #print(rapbase::getUserGroups(session))
      #print(session)
      #print(rapbase::shinySessionInfo(session, entity = "groups"))
      
      ## lag tabell over gjeldende status for abonnement
      output$activeSubscriptions <- DT::renderDataTable(
         rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
         rownames = FALSE, options = list(dom = 't')
      )
      
      ## lag side som viser status for abonnement, også når det ikke finnes noen
      output$subscriptionContent <- renderUI({
         fullName <- rapbase::getUserFullName(session)
         if (length(rv$subscriptionTab) == 0) {
            p(paste("Ingen aktive abonnement for", fullName))
         } else {
            tagList(
               p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                       rapbase::getUserEmail(session), ":")),
               DT::dataTableOutput("activeSubscriptions")
            )
         }
      })
      
      
      ## nye abonnement
      observeEvent (input$subscribe, { #MÅ HA
         package <- "nordicscir"
         owner <- rapbase::getUserName(session)
         interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
         intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
         organization <- rapbase::getUserReshId(session)
         runDayOfYear <- rapbase::makeRunDayOfYearSequence(
            interval = interval
         )
         email <- rapbase::getUserEmail(session)
         if (input$subscriptionRep == "Månedsrapport") {
            synopsis <- "NordicSCIR/Rapporteket: månedsrapport"
            rnwFil <- "NSmndRapp.Rnw" #Navn på fila
            #print(rnwFil)
         }
         
         
         fun <- "abonnement"  #"henteSamlerapporter"
         paramNames <- c('rnwFil', 'brukernavn', "reshID", "datoFra", 'datoTil')
         paramValues <- c(rnwFil, brukernavn(), reshID, as.character(startDato), as.character(Sys.Date())) #input$subscriptionFileFormat)
         #paramValues <- c(rnwFil, AlleTab, 'toill', 107627, '2018-01-01', as.character(Sys.Date())) #input$subscriptionFileFormat)
         
         #TESTING:
         #parametre <- paramValues
         #names(parametre) <- paramNames
         #parametre <- as.list(parametre)
         # abonnement(rnwFil=rnwFil, AlleTabeller=AlleTab, brukernavn='toillbokk', reshID=107627,
         #            datoFra=as.Date('2018-01-01'), datoTil=Sys.Date())
         #test <- abonnement(list(rnwFil=rnwFil), list(brukernavn='toillbokk'), list(reshID=107627),
         #          list(datoFra=as.Date('2018-01-01')), list(datoTil=as.character(Sys.Date())) ) 
         
         rapbase::createAutoReport(synopsis = synopsis, package = package,
                                   fun = fun, paramNames = paramNames,
                                   paramValues = paramValues, owner = owner,
                                   email = email, organization = organization,
                                   runDayOfYear = runDayOfYear, interval = interval,
                                   intervalName = intervalName)
         rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })
      
      ## slett eksisterende abonnement
      observeEvent(input$del_button, {
         selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
         rapbase::deleteAutoReport(selectedRepId)
         rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
      })
      
#---Utsendinger---------------
## liste med orgnr og navn
      
      sykehusNavn <- sort(unique(as.character(HovedSkjema$ShNavn)), index.return=T)
      orgs <- c(0, unique(HovedSkjema$ReshId)[sykehusNavn$ix])
      names(orgs) <- c('Alle',sykehusNavn$x)
      orgs <- as.list(orgs)

      ## liste med metadata for rapport
      reports <- list(
            MndRapp = list(
                  synopsis = "Månedsrapport",
                  fun = "abonnement", 
                  paramNames = c('rnwFil', "reshID"),
                  paramValues = c('NSmndRapp', 0)
            ),
            #abonnement <- function(rnwFil, reshID=0
            SamleRapp = list(
                  synopsis = "Rapport med samling av div. resultater",
                  fun = "abonnement",
                  paramNames = c('rnwFil', "reshID"),
                  paramValues = c('NSsamleRapp', 'Alle')
            )
      )
      
      org <- rapbase::autoReportOrgServer("NSuts", orgs)
      
      # oppdatere reaktive parametre, for å få inn valgte verdier (overskrive de i report-lista)
      paramNames <- shiny::reactive("reshID")
      paramValues <- shiny::reactive(org$value())
      
      rapbase::autoReportServer(
            id = "NSuts", registryName = "nordicscir", type = "dispatchment",
            org = org$value, paramNames = paramNames, paramValues = paramValues,
            reports = reports, orgs = orgs, eligible = TRUE
      )


      
      #----------- Eksport ----------------
      registryName <- "nordicscir"
      ## brukerkontroller
      rapbase::exportUCServer("nordicscirExport", registryName)
      ## veileding
      rapbase::exportGuideServer("nordicscirExportGuide", registryName)
      
      
      
      
} #server
# Run the application 
shinyApp(ui = ui, server = server)

