#Resultattjeneste for NordicScir
library(shiny)
library(knitr)
#ibrary(shinyBS) # Additional Bootstrap Controls

#Hente data og evt. parametre som er statistke i appen
dato <- 'FormDataContract2018-11-01' #2017-05-24
sti <- 'A:/NordicScir/'
HovedSkjema <- read.table(paste0(sti, 'Main',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Livskvalitet <- read.table(paste0(sti, 'LifeQuality',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Kontroll <- read.table(paste0(sti, 'Control', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Urin <- read.table(paste0(sti, 'UrinaryTractFunction', dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Tarm <- read.table(paste0(sti, 'BowelFunction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Performance <- read.table(paste0(sti, 'ActivityAndParticipationPerformance',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)
Satisfact <- read.table(paste0(sti, 'ActivityAndParticipationSatisfaction',dato,'.csv'), stringsAsFactors=FALSE, sep=';', header=T)

Livskvalitet$HovedskjemaGUID <- toupper(Livskvalitet$HovedskjemaGUID)
Kontroll$HovedskjemaGUID <- toupper(Kontroll$HovedskjemaGUID)
Urin$HovedskjemaGUID <- toupper(Urin$HovedskjemaGUID)
Tarm$HovedskjemaGUID <- toupper(Tarm$HovedskjemaGUID)
Performance$HovedskjemaGUID <- toupper(Performance$HovedskjemaGUID)
Satisfact$HovedskjemaGUID <- toupper(Satisfact$HovedskjemaGUID)


KobleMedHoved <- function(HovedSkjema,Skjema2) {
      varBegge <- intersect(names(Skjema2),names(HovedSkjema)) ##Variabelnavn som finnes i begge datasett
      Skjema2 <- Skjema2[ ,c("HovedskjemaGUID", names(Skjema2)[!(names(Skjema2) %in% varBegge)])]  #"SkjemaGUID",
      NSdata <- merge(HovedSkjema, Skjema2, suffixes = c('','XX'),
                      by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = F, all.y=F)
      return(NSdata)
}

#RegData <- KobleMedHoved(HovedSkjema,Tarm)
RegData <- HovedSkjema


reshID <- 107627




ui <- navbarPage( #fluidPage( #"Hoved"Layout for alt som vises på skjermen
      title = 'NORSK SPINNALSKADEREGISTER',
      tabPanel("Viktigste resultater/Oversiktsside",
               #fluidRow(
               #column(width=5,
               h2("Månedsrapport"), #),
               downloadButton(outputId = 'mndRapp.pdf', label='Månedsrapport-virker ikke på server', class = "butt"),
               tags$head(tags$style(".butt{background-color:#6baed6;} .butt{color: white;}")), # background color and font color
               br(),
               h2("Her kan man evt. vise de variable/resultater som er viktigst å overvåke", align='center' ),
               h2("Gi tilbakemelding på hva som skal være på sida", align='center' ),
               br(),
               br(),
               tags$ul(tags$b('Andre ting å ta stilling til: '),
                       tags$li("Foretrukket tittellayout på side - som på andeler eller gjennomsnitt?"), 
                       tags$li("Ønskes annen organisering av innhold?"), 
                       tags$li("Kun en figur på hver side, eller fint å vise to samtidig som under 'Andeler'? "),
                       tags$li("Hvilke utvalgs/filtreringsmuligheter skal vi ha i de ulike fanene"), 
                       tags$li("Navn på faner"), 
                       tags$li("Innhold i tabeller som vises i tilknytning til figurer.")
               ),
               br(),
               tags$ul(tags$b('Kommer: '),
                       tags$li("Alle figurer vil få tilhørende tabell i fane ved siden av som for 'Andeler' ")
               )
      ), #tab
      
      #-----Registreringsoversikter------------
      tabPanel("Registreringsoversikter",
               sidebarPanel(width=3,
                            h3('Utvalg')
               ),
                            
      
               mainPanel(
                     tabsetPanel(id='ark',
                                 tabPanel('Nøkkeltall',
                                          h2("Nøkkeltall for NorScir"),
                                          br()
                                          #tableOutput('tabNokkeltall')
                                 ),
                                 tabPanel('Ant. opphold',
                                          h2("Antal opphald per avdeling"),
                                          p(em("Velg tidsperiode ved å velge sluttdato i menyen til venstre"))
                                          #tableOutput("tabAntOpphShMnd12")
                                 )
                     ))
      ), #tab Registreringsoversikter
      
      tabPanel("Fordelinger",
               sidebarPanel(width = 2,
                            selectInput(
                                  inputId = "valgtVar", label="Velg variabel",
                                  choices = c('Alder' = 'Alder', 
                                              'Aldersfordeling, 15-årige grupper ' = 'Alder', 
                                              'Ais, innleggelse' = 'AAis' ,
                                              'Ais, kontroll' = 'FAis', 
                                              'Lengde på rehab.opphold' = 'DagerRehab', 
                                              'Tid fra skade til oppstart rehab.' = 'DagerTilRehab', 
                                              'Opphold, totalt antall dager' = 'OpphTot', 
                                              #'Fjern? Permisjon (ant. døgn ute av sykehus) ' = 'Permisjon',
                                              'Utskrevet til' = 'UtTil',
                                              'Skadeårsak ' = 'SkadeArsak',
                                              #'Fjern? Pustehjelp' = 'Pustehjelp[VentAssi]',
                                              'Livskval.: Fornøydhet med livet' = 'LivsGen',
                                              'Livskval.: Fornøydhet med fysisk helse' = 'LivsFys',
                                              'Livskval.: Fornøydhet med psykisk helse' = 'LivsPsyk',
                                              'Ufrivillig urinlekkasje' = 'UrinInkontinens', 
                                              'Urin: Kirurgiske inngrep' = 'UrinKirInngr',
                                              'Urin: Legemiddelbruk' = 'UrinLegemidler',
                                              'Urin: Legemiddelbruk, hvilke' = 'UrinLegemidlerHvilke',
                                              'Urin: Blæretømming, hovedmetode' = 'UrinTomBlareHoved',
                                              'Urin: Blæretømming, tilleggsmetode' = 'UrinTomBlareTillegg',
                                              'Tarm: Avføring, hovedmetode' = 'TarmAvfHoved',
                                              'Tarm: Avføring, tilleggsmetode' = 'TarmAvfTillegg',
                                              'Tarm: Avføringsmiddelbruk' = 'TarmAvfmiddel',
                                              'Tarm: Avføringsmidler, hvilke' = 'TarmAvfmiddelHvilke',
                                              'Tarm: Fekal inkontinens' = 'TarmInkontinens',
                                              'Tarm: Kirurgisk inngrep' = 'TarmKirInngrep',
                                              'Tarm: Kirurgiske inngrep, hvilke' = 'TarmKirInngrepHvilke'
                                  )
                            ),
                            dateRangeInput(inputId = 'datovalg', start = "2017-07-01", end = Sys.Date(),
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
                            #AIS-grad
                            #Traume
                            #Nivå, utreise
                            selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                        choices = c("Egen mot resten av landet"=1, 
                                                    "Hele landet"=0, 
                                                    "Egen enhet"=2)
                            )
                            #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                            #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
               ),
               mainPanel(
                     tabsetPanel(
                           tabPanel(
                                 'Figur',
                                 plotOutput('fordelinger')),
                           tabPanel(
                                 'Tabell',
                                 uiOutput("tittelFord"),
                                 tableOutput('fordelingTab'))
                     )
               )
      ) #tab Fordelinger
)   

# selectInput(
#       inputId = "valgtVarGjsnGrVar", label="Velg variabel",
#       choices = c('Alder' = 'Alder', 
#                   'Ant. dager før rehabilitering' = 'DagerTilRehab',
#                   'Antall dager med rehabilitering' = 'DagerRehab',
#                   'Oppphold, totalt' = 'OpphTot',
#                   'Registreringsforsinkelse' = 'RegForsinkelse',
#                   'Livskval.: Fornøydhet med livet' = 'LivsGen',
#                   'Livskval.: Fornøydhet med fysisk helse' = 'LivsFys',
#                   'Livskval.: Fornøydhet med psykisk helse' = 'LivsPsyk'
#       )
# ),




# Define server logic required to draw a histogram
server <- function(input, output) {

      output$fordelinger <- renderPlot({
            NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                          datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                          reshID = reshID, #AIS=AIS, 
                          minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                          erMann=as.numeric(input$erMann), #traume=traume, paratetra=paratetra,
                          enhetsUtvalg=as.numeric(input$enhetsUtvalg))
      }, height=800, width=800 #height = function() {session$clientData$output_fordelinger_width}
      )
      
      observe({      
            UtDataFord <- NSFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2], 
                                       reshID=reshID, #AIS=AIS, 
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]), 
                                       erMann=as.numeric(input$erMann), #traume=traume, paratetra=paratetra,
                                        enhetsUtvalg=as.numeric(input$enhetsUtvalg))
            #SJEKK !!!!!!!!!!:
            #tab <- lagTabavFig(UtDataFraFig = UtDataFord)
            
            output$tittelFord <- renderUI({
                  tagList(
                        h3(UtDataFord$tittel),
                        h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
                  )}) #, align='center'
            output$fordelingTab <- renderTable(
                  tab, rownames = T)
      })
      
      
      }

# Run the application 
shinyApp(ui = ui, server = server)

