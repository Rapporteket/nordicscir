#Resultattjeneste for NordicScir
library(nordicscir)
library(shiny)
library(knitr)
library(lubridate)
#ibrary(shinyBS) # Additional Bootstrap Controls
library(kableExtra)
#library(zoo)


startDatoStandard <- '2018-01-01' #Sys.Date()-364



ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK SPINALSKADEREGISTER med FIKTIVE data',
      tabPanel("Startside",
               #fluidRow(
               #column(width=5,
               br(),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               
               sidebarPanel(width = 3,
                            br(),
                            #h2('Nedlastbare dokumenter med samling av resultater'),
                            h3("Månedsrapport"), #),
                            downloadButton(outputId = 'mndRapp.pdf', label='Last ned MÅNEDSRAPPORT', class = "butt"),
                            br(),
                            br(),
                            h3("Resultater hele landet, SC-bruker"), #),
                            downloadButton(outputId = 'samlerappLandet', label='Last ned', class = "butt"),
                            br(),
                            h3("Resultater eget sykehus, SC-bruker"), #),
                            downloadButton(outputId = 'samlerappEgen', label='Last ned', class = "butt"),
                            br(),
                            br(),
                            br()
               ),
               mainPanel(width = 8,
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
                         h4(tags$b('Innhold i de ulike fanene:')),
                         h4(tags$b('Fordelinger '), 'viser på fordelinger (figur/tabell) av ulike variable. 
                              Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                         h4(tags$b('Sykehusvise resultater '), 'viser gjennomsnittsverdier per sykehus. 
                            Man kan velge hvilken variabel man vil se på og om man vil se gjennomsnitt eller median. 
                            Man kan også velge å filtrere data.'),
                         h4(tags$b('Registreringsoversikter '), 'viser aktivitet i registeret. Også her kan man gjøre filtreringer.'),
                         br(),
                         br(),
                         h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                  a("NorSCIR", href="https://www.kvalitetsregistre.no/registers/561/resultater"),
                                  target="_blank"),
                         br(),
                         h4('Hjemmeside NorSCIR: ', #
                                  a("www.norscir.no", href="http://www.norscir.no", target="_blank")) #target gjør at lenken åpnes i ny fane
                         
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
                                              'Tarm: Fekal inkontinens (fra 2019)' = 'TarmInkontinens',
                                              'Tarm: Fekal inkontinens (t.o.m. 2018)' = 'TarmInkontinensTom2018',
                                              'Tarm: Kirurgisk inngrep' = 'TarmKirInngrep',
                                              'Tarm: Kirurgiske inngrep, hvilke' = 'TarmKirInngrepHvilke'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = startDatoStandard, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
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
                            
                            #valgAIS <- as.character(0:5),
                            #names(valgAIS) <- c('Alle', LETTERS[1:5]),
                            #valgAIS <- c('"Alle"=0', '"A"=1', '"B"=2', '"C"=3', '"D"=4', '"E"=5'),
                            
                            selectInput(inputId = 'AIS', label='AIS-grad',
                                        multiple = T, #selected=0,
                                        choices = #valgAIS
                                              c("Alle"=0,
                                                "A"=1,
                                                "B"=2,
                                                "C"=3,
                                                "D"=4,
                                                "E"=5)
                            ),
                            selectInput(inputId = 'traume', label='Traume',
                                        choices = c("Alle"=' ', #'ikke'
                                                    "Traume"='ja', 
                                                    "Ikke traume"='nei')
                            ),
                            selectInput(inputId = 'paratetra', label='Nivå ved utreise',
                                        choices = c("Alle" = 99,
                                                    "Paraplegi" = 0, 
                                                    "Tetraplegi" = 1,
                                                    "Ukjent" = 9)
                            )
                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 br(),
                                 em('(Høyreklikk på figuren for å laste den ned)'),
                                 plotOutput('fordelinger')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelFord"),
                                 br(),
                                 tableOutput('fordelingTab'),
                                 downloadButton(outputId = 'lastNed_tabFord', label='Last ned') 
                           )
                     ))
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
                            dateRangeInput(inputId = 'datovalgGjsnGrVar', start = startDatoStandard, end = Sys.Date(),
                                           label = "Tidsperiode", separator="t.o.m.", language="nb"),
                            selectInput(inputId = "erMannGjsnGrVar", label="Kjønn",
                                        choices = c("Begge"=2, "Menn"=1, "Kvinner"=0)
                            ),
                            sliderInput(inputId="alderGjsnGrVar", label = "Alder", min = 0,
                                        max = 110, value = c(0, 110)
                            ),
                            selectInput(inputId = 'AISGjsnGrVar', label='AIS-grad',
                                        multiple = T, #selected=0,
                                        choices = c("Alle"=0,
                                                    "A"=1, 
                                                    "B"=2,
                                                    "C"=3,
                                                    "D"=4,
                                                    "E"=5
                                        )
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
                            dateRangeInput(inputId = 'datovalgReg', start = startDatoStandard, end = Sys.Date(),
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
                                    h3("Antall registreringsskjema med ulike oppfølgingsskjema"),
                                    tableOutput('tabAntTilknyttedeHovedSkjema'),
                                    downloadButton(outputId = 'lastNed_tabOppfHovedAnt', label='Last ned'),
                                    br(),
                                    h3("Andel (%) registreringsskjema med ulike oppfølgingsskjema"),
                                    tableOutput("tabAndelTilknyttedeHovedSkjema"),
                                    downloadButton(outputId = 'lastNed_tabOppfHovedPst', label='Last ned')
                           ), 
                           tabPanel('Antall kontrollskjema med tilknyttede skjema',
                                    h3("Antall kontrollskjema med ulike oppfølgingsskjema"),
                                    tableOutput('tabAntTilknyttedeKtrSkjema'),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrAnt', label='Last ned'),
                                    br(),
                                    h3("Andel (%) registreringsskjema med ulike oppfølgingsskjema"),
                                    tableOutput("tabAndelTilknyttedeKtrSkjema"),
                                    downloadButton(outputId = 'lastNed_tabOppfKtrPst', label='Last ned')
                           )
                                    
               ))
) #tab Registreringsoversikter
) #ui-del




#----- Define server logic required to draw a histogram-------
server <- function(input, output) {

 #NB: Skal bare forholde oss til oppfølgingsskjema som er tilknyttet et gyldig Hovedskjema
      
      context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
      if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
                  
                  registryName <- "nordicscir"
                  dbType <- "mysql"
 #IKKE klar. Må plukke ut datasett koblet til Hovedskjema. Finn hvilke variable fra hovedskjema som trenger å være
#                  med for å gjøre alle filtreringer
                  
                  qLivs <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID
                      FROM LifeQualityFormDataContract')
                  qKontr <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID,
                        HealthUnitShortName, NoControl, CNum
                       FROM ControlFormDataContract')
                  qUrin <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM UrinaryTractFunctionFormDataContract')
                  qTarm <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM BowelFunctionFormDataContract')
                  qFunk <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID 
                      FROM ActivityAndParticipationPerformanceFormDataContract')
                  qTilf <- paste0('SELECT  UPPER(HovedskjemaGUID) AS HovedskjemaGUID, UPPER(SkjemaGUID) AS SkjemaGUID
                      FROM ActivityAndParticipationSatisfactionFormDataContract')
                  
                  #RegData <- NSRegDataSQL(valgtVar = valgtVar) #datoFra = datoFra, datoTil = datoTil)
                  HovedSkjema <- NSRegDataSQL() #datoFra = datoFra, datoTil = datoTil)
                  Livskvalitet <- rapbase::LoadRegData(registryName, qLivs, dbType)
                  Kontroll <- rapbase::LoadRegData(registryName, qKontr, dbType)
                  Urin <- rapbase::LoadRegData(registryName, qUrin, dbType)
                  Tarm <- rapbase::LoadRegData(registryName, qTarm, dbType)
                  AktivFunksjon <- rapbase::LoadRegData(registryName, qFunk, dbType)
                  AktivTilfredshet <- rapbase::LoadRegData(registryName, qTilf, dbType)
                  } #hente data på server

  #----------Hente data og evt. parametre som er statistke i appen----------
     if (!exists('HovedSkjema')){
            #Tulledata:
            data('NordicScirFIKTIVEdata', package = 'nordicscir')
            # data('HovedSkjemaTull', package = 'nordicscir')
            # data('LivskvalitetTull', package = 'nordicscir')
            # data('KontrollTull', package = 'nordicscir')
            # data('UrinTull', package = 'nordicscir')
            # data('TarmTull', package = 'nordicscir')
            # data('AktivFunksjonTull', package = 'nordicscir')
            # data('AktivTilfredshetTull', package = 'nordicscir')
            
            
            #Laste ekte data lokalt
            # dato <- 'FormDataContract2018-01-30' #2017-05-24
            # sti <- 'A:/NordicScir/'
            # HovedSkjema <- read.table(paste0(sti, 'Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Livskvalitet <- read.table(paste0(sti, 'LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Kontroll <- read.table(paste0(sti, 'Control', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # #Urin <- read.table(paste0(sti, 'UrinaryTractFunction', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # Tarm <- read.table(paste0(sti, 'BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # AktivFunksjon <- read.table(paste0(sti, 'ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
            # AktivTilfredshet <- read.table(paste0(sti, 'ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
      }  
            Livskvalitet$HovedskjemaGUID <- toupper(Livskvalitet$HovedskjemaGUID)
            Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
            Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
            Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
            AktivFunksjon$HovedskjemaGUID <- toupper(AktivFunksjon$HovedskjemaGUID)
            AktivTilfredshet$HovedskjemaGUID <- toupper(AktivTilfredshet$HovedskjemaGUID)
            
            
            # sum(Aktivitet$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivFunksjon$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivTilfredshet$HovedskjemaGUID %in% HovedSkjema$SkjemaGUID)
            # sum(AktivTilfredshet$HovedskjemaGUID %in% AktivFunksjon$SkjemaGUID)
      #}
      
      HovedSkjema <- NSPreprosesser(HovedSkjema)
      LivskvalitetH <- KobleMedHoved(HovedSkjema,Livskvalitet)
      KontrollH <- KobleMedHoved(HovedSkjema,Kontroll)
      UrinH <- KobleMedHoved(HovedSkjema,Urin)
      TarmH <- KobleMedHoved(HovedSkjema,Tarm)
      Aktivitet <- KobleMedHoved(AktivFunksjon, AktivTilfredshet) #[,-which(names(AktivFunksjon)=='HovedskjemaGUID')]
      AktivitetH <- KobleMedHoved(HovedSkjema, Aktivitet)
      FunksjonH <- KobleMedHoved(HovedSkjema, AktivFunksjon)
      TilfredsH <- AktivitetH
      #RegData <- KobleMedHoved(HovedSkjema,Livskvalitet, alleHovedskjema = T) #SKAL iKKE BRUKES
      AlleTab <- list(HovedSkjema=HovedSkjema, 
                      LivskvalitetH=LivskvalitetH, 
                      KontrollH=KontrollH, 
                      UrinH=UrinH, 
                      TarmH=TarmH, 
                      FunksjonH=FunksjonH, 
                      TilfredsH = TilfredsH, 
                      AktivitetH = AktivitetH)
      reshID <- 107627

#-------Samlerapporter--------------------      
      # funksjon for å kjøre Rnw-filer (render file funksjon)
      contentFile <- function(file, srcFil, tmpFile) {
            src <- normalizePath(system.file(srcFil, package="nordicscir"))
            
            # gå til tempdir. Har ikke skriverettigheter i arbeidskatalog
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, tmpFile, overwrite = TRUE)
            
            texfil <- knitr::knit(tmpFile, encoding = 'UTF-8')
            tools::texi2pdf(texfil, clean = TRUE)
            
            gc() #Opprydning gc-"garbage collection"
            file.copy(paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf'), file)
      }
      
      
      
      output$mndRapp.pdf <- downloadHandler(
            filename = function(){ paste0('MndRapp', Sys.time(), '.pdf')}, 
            content = function(file){
                  contentFile(file, srcFil="NSmndRapp.Rnw", tmpFile="tmpNSmndRapp.Rnw")
            })
      output$samlerappEgen <- downloadHandler(
                  filename = function(){ paste0('SamleRappEgen', Sys.time(), '.pdf')},
                  content = function(file){
                        contentFile(file, srcFil="NSsamleRapp.Rnw", tmpFile="tmpNSsamleRapp.Rnw")
                  })
      output$samlerappLandet <- downloadHandler(
                        filename = function(){ paste0('SamleRappLand', Sys.time(), '.pdf')},
                        content = function(file){
                              contentFile(file, srcFil="NSsamleRappLand.Rnw", tmpFile="tmpNSsamleRappLand.Rnw")
                        })

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
            #RegData <- finnRegData(Data = AlleTab, valgtVar <- 'UrinKirInngr')
            RegData <- finnRegData(valgtVar = input$valgtVar, Data = AlleTab)
            RegData <- TilLogiskeVar(RegData)
            output$fordelinger <- renderPlot({
                  NSFigAndeler(RegData=RegData, valgtVar=input$valgtVar, preprosess = 0,
                               datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                               reshID = reshID, 
                               AIS=input$AIS, traume=input$traume, paratetra=as.numeric(input$paratetra),
                               minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                               erMann=as.numeric(input$erMann), 
                               enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
            )
            
            #RegData må hentes ut fra valgtVar
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID = reshID, 
                                       AIS=input$AIS, traume=input$traume, paratetra=as.numeric(input$paratetra),
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), 
                                       enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            tabFord <- lagTabavFigAndeler(UtDataFraFig = UtDataFord)
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- renderTable(
                  tabFord, rownames = T)
            
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
      }) #observe Fordeling
      
      observe({ #Sykehusvise gjennomsnitt, figur og tabell
            RegData <- finnRegData(valgtVar = input$valgtVarGjsnGrVar, Data = AlleTab)
            output$gjsnGrVar <- renderPlot(
                  NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                 datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                 AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, paratetra=as.numeric(input$paratetraGjsnGrVar),
                                 minald=as.numeric(input$alderGjsnGrVar[1]), maxald=as.numeric(input$alderGjsnGrVar[2]), 
                                 erMann=as.numeric(input$erMannGjsnGrVar), valgtMaal = input$sentralmaal
                  ),
                  width = 800, height = 600)
            UtDataGjsnGrVar <- NSFigGjsnGrVar(RegData=RegData, preprosess = 0, valgtVar=input$valgtVarGjsnGrVar,
                                              datoFra=input$datovalgGjsnGrVar[1], datoTil=input$datovalgGjsnGrVar[2], 
                                              AIS=input$AISGjsnGrVar, traume=input$traumeGjsnGrVar, paratetra=as.numeric(input$paratetraGjsnGrVar),
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

