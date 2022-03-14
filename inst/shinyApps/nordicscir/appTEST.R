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

ui <- tagList(
   shinyjs::useShinyjs(),
   navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      id='toppPaneler',
            # lag logo og tittel som en del av navbar
            #title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"), regTitle),
      title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                  regTitle),
            # sett inn tittle ogsÃ¥ i browser-vindu
      windowTitle = regTitle,
            # velg css (forelÃ¸pig den eneste bortsett fra "naken" utgave)
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
                            br('Hvis du ønsker månedsrapporten tilsendt på e-post, 
                               kan du gå til fanen "Abonnement" og bestille dette.'),
               ),
               mainPanel(width = 8,
                         if (paaServer){ 
                            rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                      organization = uiOutput("appOrgName"),
                                                      addUserInfo = TRUE)},
                         h2('Velkommen til Rapporteket - Norsk Ryggmargsskaderegister!', align='center'),
                         br(),
                         h4('Du er nå inne på Rapporteket for NorSCIR. Rapporteket er registerets resultattjeneste. 
                            Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret. 
                            På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert 
                            på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database. 
                            Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                         h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                            Resultatene som vises er 
                              basert på AdmitDt, altså dato for første akutte innleggelse. Alle figurer og 
                            tabeller kan lastes ned.'),
                         br(),
                         h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                         h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variable. 
                              Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                         h4(tags$b('Sykehusvise resultater '), 'viser gjennomsnittsverdier per sykehus. 
                            Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt eller median. 
                            Man kan også velge å filtrere data.'),
                         h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret. Også her kan man gjøre filtreringer.'),
                         h4(tags$b('Abonnement'), 'inneholder oversikt over rapporter du abbonerer på. Her kan du også bestille abonnement, 
                            dvs. rapporter tilsendt på e-post.'),
                         br(),
                         br(),
                         h3('Kommentar: HER ville jeg vist sykehusets registreringer per måned i løpet av siste år'),
                         br(),
                         br(),
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
           
                         
                                       
                         # column(width=6,
                         #        h3('Nevrologisk klassifikasjon.', align='center'),
                         #        br(),
                         #        tableOutput('tabNevrKlass')),
                         # column(width=6,
                         #        h3('Nevrologisk klassifikasjon for pasienter med liggetid over 28 dager i
                         #           ryggmargsskadeavdeling.', align='center'),
                         #        tableOutput('tabNevrKlass28')
                         #        ),
               )
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
                                              'Planlagt utskrevet til' = 'PPlaceDis',
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab', 
                                              'Tid med rehabilitering' = 'DagerRehab',
                                              'Opphold, totalt antall dager' = 'OpphTot', 
                                              #'Fjern? Permisjon (ant. døgn ute av sykehus) ' = 'Permisjon',
                                              'Utskrevet til' = 'UtTil',
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'Skadeårsak ' = 'SkadeArsak',
                                              'Skadeårsak, ikke-traumatisk' = 'Ntsci',
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
                            )
                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(width = 6,
                         tabsetPanel( id='fordeling',
                                      tabPanel(
                                         'Figur',
                                         br(),
                                         em('(Høyreklikk på figuren for å laste den ned)'),
                                         br(),
                                         br(),
                                         plotOutput('fordelinger'),
                                         hr()
                                      ) ,
                                      tabPanel(
                                         'Figur, alle sykehus',
                                         plotOutput('fordelingPrSh')),
                                      tabPanel(
                                         'Tabell',
                                         uiOutput("tittelFord"),
                                         br(),
                                         tableOutput('fordelingTab'),
                                         downloadButton(outputId = 'lastNed_tabFord', label='Last ned'),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         #hr(),
                                         #conditionalPanel(condition = (rolle()=='SC'),
                                         tableOutput('fordelingShTab'),
                                         downloadButton(outputId = 'lastNed_tabFordSh', label='Last ned')
                                         #)
                                      )
                         )) #mainPanel
      ), #tab Fordelinger
   
         
#------------Sykehusvise resultater------------
      tabPanel("Sykehusvise resultater",
               sidebarPanel(width = 3,
                            selectInput(
                                  inputId = "valgtVarGjsnGrVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder', 
                                              'Lengde på rehab.opphold' = 'DagerRehab', 
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab', 
                                              'Opphold, totalt antall dager' = 'OpphTot', 
                                              'Registreringsforsinkelse' = 'RegForsinkelse',
                                              'Livskval.: Tilfredshet med livet' = 'LivsGen',
                                              'Livskval.: Tilfredshet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Tilfredshet med psykisk helse' = 'LivsPsyk'
                                             )
                            ),
                            dateRangeInput(inputId = 'datovalgGjsnGrVar', start = startDato, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            selectInput(inputId = "erMannGjsnGrVar", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alderGjsnGrVar", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'AISGjsnGrVar', label='AIS-grad ved utreise',
                                        multiple = T, #selected=0,
                                         choices = valgAIS
                            ),
                            selectInput(inputId = 'traumeGjsnGrVar', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja', 
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'paratetraGjsnGrVar', label='Nivå ved utreise',
                                        choices = c("Alle" = 99,
                                                    "Paraplegi" = 0, 
                                                    "Tetraplegi" = 1,
                                                    "Ukjent" = 9)
                            ),
                            selectInput(inputId = "sentralmaal", label="Velg gjennomsnitt/median ",
                                                               choices = c("Gjennomsnitt"='gjsn', "Median"='med'))
                            
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 br(),
                                 em('(Høyreklikk på figuren for å laste den ned)'),
                                 br(),
                                 br(),
                                 plotOutput('gjsnGrVar')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelGjsnGrVar"),
                                 br(),
                                 tableOutput('gjsnGrVarTab'),
                                 downloadButton(outputId = 'lastNed_tabGjsnGrVar', label='Last ned') # , class = "butt"))
                           )
                     )
               )
               
), #GjsnGrVar 



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
                                    )
                                    #                                          br(),
                                    # h3("Belegg FJERNES! på rehabiliteringsavdelinga - ønskes flere/andre variable?"), 
                                    # #uiOutput("undertittelBelegg"),
                                    # fluidRow( tableOutput("tabLiggetider"))
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
                                    tableOutput('tabAntTilknyttedeKtrSkjema'),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrAnt', label='Last ned'),
                                    br(),
                                    h3("Andel (%) kontrollskjema med tilknyttede skjema"),
                                    tableOutput("tabAndelTilknyttedeKtrSkjema"),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrPst', label='Last ned')
                           )
                                    
               ))
) #tab Registreringsoversikter
)
)


#----- Define server logic required to draw a histogram-------
server <- function(input, output, session) {
      
   #-----------Div serveroppstart------------------  
   #raplog::appLogger(session = session, msg = "Starter intensiv-app")
      #system.file('NSmndRapp.Rnw', package='nordicscir')
      #system.file('NSsamleRapp.Rnw', package='nordicscir')
   
      #hospitalName <-getHospitalName(rapbase::getUserReshId(session))
      reshID <- reactive({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 107627)}) 
      rolle <- reactive({ifelse(paaServer, rapbase::getUserRole(shinySession=session), 'SC')})
      brukernavn <- reactive({ifelse(paaServer, rapbase::getUserName(shinySession=session), 'tullebukk')})
      #output$reshID <- renderText({ifelse(paaServer, as.numeric(rapbase::getUserReshId(session)), 105460)}) #evt renderUI
      
      observe({if (rolle() != 'SC') {
            #print('OK')
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
         output$appOrgName <- renderText(paste0('rolle: ', rolle(), '<br> ReshID: ', reshID()) )}
      
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
                  
                  # widget
                  output$appUserName <- renderText(rapbase::getUserFullName(session))
                  output$appOrgName <- renderText(rapbase::getUserReshId(session))
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
      
#-------Samlerapporter--------------------      
      # funksjon for å kjøre Rnw-filer (render file funksjon)
      # filename function for re-use - i dette tilfellet vil det funke fint å hardkode det samme..
      # downloadFilename <- function(fileBaseName) {
      #       paste0(fileBaseName, as.character(as.integer(as.POSIXct(Sys.time()))), '.pdf')
      # }
      # 
      # contentFile <- function(file, srcFil, tmpFil, package,
      #                         reshID=0) {
      #       src <- normalizePath(system.file(srcFil, package=package))
      #       dev.off()
      #       
      #       # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
      #       owd <- setwd(tempdir())
      #       on.exit(setwd(owd))
      #       file.copy(src, tmpFil, overwrite = TRUE)
      #       
      #       texfil <- knitr::knit(tmpFil, encoding = 'UTF-8')
      #       tools::texi2pdf(texfil, clean = TRUE)
      #       
      #       #gc() #Opprydning gc-"garbage collection"
      #       file.rename(stringr::str_replace(texfil,'tex','pdf'), file)
      # }
      
      
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
                        reshID = reshID()) #, datoFra = startDato, datoTil = Sys.Date())
         })
      output$samleRappLand.pdf <- downloadHandler(
            filename = function(){'NorScirSamleRapportLand.pdf'}, # downloadFilename('NorScirSamleRapport')
            content = function(file){
                  contentFile(file, srcFil="NSsamleRappLand.Rnw", tmpFile="tmpNSsamleRappLand.Rnw",
                              reshID=reshID(), 
                              datoFra = input$datovalgSamleRapp[1], 
                              datoTil = input$datovalgSamleRapp[2])
            })
      output$samleRappEgen.pdf <- downloadHandler(
         filename = function(){'NorScirSamleRapportEgen.pdf'}, # downloadFilename('NorScirSamleRapport')
         content = function(file){
            contentFile(file, srcFil="NSsamleRapp.Rnw", tmpFile="tmpNSsamleRapp.Rnw",
                        reshID=reshID(), 
                        datoFra = input$datovalgSamleRapp[1], 
                        datoTil = input$datovalgSamleRapp[2])
         })

      # downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
      # downloadButton(outputId = 'samlerappLandet', label='Last ned', class = "butt"),
      # downloadButton(outputId = 'samlerappEgen', label='Last ned', class = "butt"),
      

#--------------Startside------------------------------
      
      output$lenkeNorScir <- renderUI({tagList("www.norscir.no", www.norscir.no)})
      
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
            tabLiggetider(RegData = HovedSkjema, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2])},
            rownames=T,
            digits = 0
      )
      # output$tabLiggetider <- function() { 
      #       tabLigget <- tabLiggetider(RegData = HovedSkjema, datoFra = input$datovalgDash[1], datoTil = input$datovalgDash[2])
      #       kableExtra::kable(tabLigget, format = 'html'
      #                         , full_width=F
      #                         , digits = c(0,0,0,1,0)
      #       )}
      
      
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
         
            #print(rolle())
            #print((rolle() != 'SC'))
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
            #print(input$datoUt)
            #NSFigAndeler(RegData=RegData, valgtVar='UrinLegemidlerHvilke', preprosess = 0)
            
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID(), 
                               AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                               datoUt=as.numeric(input$datoUt))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID = reshID(), 
                                       AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            
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
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               datoUt=as.numeric(input$datoUt))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            UtDataFordSh <- NSFigAndelerSh(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       AIS=input$AIS, traume=input$traume, nivaaUt=as.numeric(input$nivaaUt),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       datoUt=as.numeric(input$datoUt))
            
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
  
#----------------------Sykehusvise resultater -----------------------------------          
      observe({ #Sykehusvise gjennomsnitt, figur og tabell
            RegData <- finnRegData(valgtVar = input$valgtVarGjsnGrVar, Data = AlleTab)
            #print(input$valgtVarGjsnGrVar)
            #print(dim(RegData))
            output$gjsnGrVar <- renderPlot(
                  NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                 datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                 AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, nivaaUt=as.numeric(input$paratetraGjsnGrVar),
                                 minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                                 erMann=as.numeric(input$erMannGjsnGrVar), valgtMaal = input$sentralmaal
                  ),
                  width = 800, height = 600)
            UtDataGjsnGrVar <- NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                              datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                              AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, nivaaUt=as.numeric(input$paratetraGjsnGrVar),
                                              minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                                              erMann=as.numeric(input$erMannGjsnGrVar), valgtMaal = input$sentralmaal)
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
            
            
      }) #observe gjsnGrVar
      
      
 
} #server
# Run the application 
shinyApp(ui = ui, server = server)

