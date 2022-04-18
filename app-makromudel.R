#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Deployment according to this tutorial: 
#
#    https://www.charlesbordet.com/en/guide-shiny-aws/#1-create-a-shiny-app

library(shiny)
library(ggthemes)


# Kataloogide asukohad
wd <<- '.'
script_loc <<- file.path(wd, "1.scripts")  
data_loc <<- file.path(wd, "2.data")
dict_loc <<- file.path(wd, "3.dictionaries")
exa_loc <<- file.path(wd, "4.user_input_examples")
model_loc <<- file.path(wd, "6.model_modules")
model_app_loc <<- file.path(wd, "8.extras", "C19makromudel-app-files")

# Loeme sisse skriptid
source(file.path(model_app_loc,"app_packages.R"), encoding = "UTF-8")
source(file.path(script_loc,"IO_functions.R"), encoding = "UTF-8")
source(file.path(script_loc,"support_functions.R"), encoding = "UTF-8")

# Loeme sisse mudeli moodulid
#source(file.path(model_loc,"0.launch_the_model.R"), encoding = "UTF-8")
#source(file.path(model_loc,"1.input_data_domestic.R"), encoding = "UTF-8")
source(file.path(model_loc,"2.impact_of_tax_change_on_prices.R"), encoding = "UTF-8")
source(file.path(model_loc,"3.impact_of_price_change_on_final_demand.R"), encoding = "UTF-8")
source(file.path(model_loc,"4.demand_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"5.supply_side_impact_on_TP.R"), encoding = "UTF-8")
source(file.path(model_loc,"6.impact_on_VA_LC_PROF.R"), encoding = "UTF-8")
source(file.path(model_loc,"8.impact_on_taxes.R"), encoding = "UTF-8")
source(file.path(model_loc,"9.demand_side_impact_on_employment.R"), encoding = "UTF-8")

# Loeme sisse appi-spetsiifilised scriptid
source(file.path(model_app_loc,"model_app_scripts.R"), encoding = "UTF-8")
source(file.path(model_app_loc,"model_app_tekstid.R"), encoding = "UTF-8")
source(file.path(model_app_loc,"export_app_fns.R"), encoding = "UTF-8")



# Andmed
################################################################################

# Nullstsenaariumi template
input_list_templ <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'null_template.xlsx')))

tegevusalad <- rownames(input_list_templ$d_household_FD)
names(tegevusalad) <- tegevusalad

lopptarbimine_kategooria <- c('Kodumajapidamised' = 'kodumajapidamised',
                              'Investeeringud' = 'investeeringud',
                              'Valitsussektor' = 'valitsussektor',
                              'Eksport' = 'eksport')


old_tax_rates = read_xlsx(file.path(data_loc, "nominal_tax_rates_by_sector.xlsx"))[1,2:5]



# Ekspordimooduili andmed
load(file.path(data_loc, "export_module", "export_baseline.Rda"))
load(file.path(data_loc, "export_module", "ibci_export_shares.Rda"))
load(file.path(data_loc, "export_module", "ibc_export_shares.Rda"))

export_baseline <<- export_baseline
ibci_export_shares <<- ibci_export_shares
ibc_export_shares <<- ibc_export_shares

eksport_sisend_c <- create_user_input_table_app(k = 63, 
                                                destination_agr = 'c', 
                                                scenarios = 'Stsenaarium')

eksport_sisend_ci <- create_user_input_table_app(k = 63, 
                                                 destination_agr = 'ci', 
                                                 scenarios = 'Stsenaarium')

sihtriigid <- unique(eksport_sisend_c$countries$destination_country)
tegevusalad <- unique(eksport_sisend_c$topK$origin_industry_text)
majandusharud <- unique(eksport_sisend_ci$ARG$destination_industry_text)




# Define UI for application
ui <- fluidPage(
  
  titlePanel(windowTitle = "Covid-19 makromudel",
             title = tags$head(tags$link(rel="shortcut icon", href="favicon.ico"))),
  tags$style(type = 'text/css', '.navbar.navbar-default {border-left: none;
                                                         border-right: none;}
                                 h5 {letter-spacing: 1px;
                                     font-size: 0.9rem;}
                                 h6 {letter-spacing: 1px;
                                     font-size: 0.75rem;}
                                 a {color: #2780e3;}
                                 a.nav-link {color: #1a1a1a;}
                                 #inputTabset {border-bottom: 2px solid;
                                               padding-bottom: 2px;}
                                 #figSortBtn {border-color: #4f4f4f;
                                              padding: 0.325rem 0.75rem;
                                              margin: 1rem;}'),
  
  theme = bslib::bs_theme(bootswatch = "lux"),
  add_busy_spinner(spin = "cube-grid", position = 'full-page', color = '#4f4f4f'),
  
  
  navbarPage(
    '',
    tabPanel(
      title = "RITA COVID-19 makromudel",
      
      fluidRow(
        column(
          12,
          markdown(sissejuhatus)
        )
      ),
      
      tags$hr(),
      
      # Sidebar with controls
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = 'inputTabset',
            
            tabPanel(
              title = tags$h6(HTML('Eeldefineeritud<br/>stsenaariumid')),
              value = 'tab_sts',
              
              tags$br(),
              
              # Stsenaariumi valik (või enda stenstaariumi loomine)
              radioButtons(
                inputId = 'simulatsioon',
                label = 'Eeldefineeritud stsenaariumid. Suurendame 100 000 euro võrra:',
                choices = c('Kodumajapidamiste lõpptarbimist' = 'sim_kodu',
                            'Valitsussektori lõpptarbimist' = 'sim_valitsus',
                            'Investeeringuid' = 'sim_invest',
                            'Eksporti' = 'sim_eksp',
                            'Kõiki eelpool mainituid koos' = 'sim_lt'),
                selected = 'sim_kodu'
              )
              
              
            ),
            
            
            
            tabPanel(
              title = tags$h6(HTML('Defineeri<br/>stsenaarium')),
              value = 'tab_param',
              
              tags$br(),
              
              tags$h6('Muutuse tüüp'),
              
              radioButtons(
                inputId = 'muutus_tyyp',
                label = 'Vali muutuse tüüp:',
                choices = c('Muutus lõppkasutamises' = 'muutus_loppkasutamine',
                            'Muutus hõives' = 'muutus_hoive',
                            'Muutus maksumäärades' = 'muutus_maksud'),
              ),
              
              tags$hr(),
              
              conditionalPanel(
                condition = "input.muutus_tyyp == 'muutus_loppkasutamine' | input.muutus_tyyp == 'muutus_hoive'",
                
                
                pickerInput(
                  inputId = 'sim_select_tegevusala',
                  label = 'Vali tegevusala:',
                  choices = tegevusalad, 
                  multiple = F
                )
                
              ),
              
              conditionalPanel(
                condition = "input.muutus_tyyp == 'muutus_loppkasutamine'",
                
                
                radioButtons(
                  inputId = 'sim_select_kategooria',
                  label = 'Vali lõppkasutamise komponent:',
                  choices = lopptarbimine_kategooria
                ),
                
                numericInput(
                  inputId = 'sim_select_kategooria_value',
                  label = 'Sisesta lisanduv lõppkasutamine (eur):',
                  value = 0
                )
              ),
              
              conditionalPanel(
                condition = "input.muutus_tyyp == 'muutus_hoive'",
                
                numericInput(
                  inputId = 'sim_select_hoive',
                  label = 'Sisesta hõive muutus:',
                  value = 0,
                  min = 0
                )
              ),
              
              conditionalPanel(
                condition = "input.muutus_tyyp == 'muutus_maksud'",
                
                numericInput(
                  inputId = 'sim_select_km',
                  label = 'Käibemaks - tavamäär',
                  value = old_tax_rates["Käibemaks - tavamäär"][[1]],
                  min = 0,
                  max = 1,
                  step = 0.001,
                  width = 250),
                numericInput(
                  inputId = 'sim_select_kmv',
                  label = 'Käibemaks - vähendatud määr',
                  value = old_tax_rates["Käibemaks - vähendatud määr"][[1]],
                  min = 0,
                  max = 1,
                  step = 0.001,
                  width = 250
                ),
                numericInput(
                  inputId = 'sim_select_sm',
                  label = 'Sotsiaalmaks',
                  value = old_tax_rates["Sotsiaalmaks"][[1]],
                  min = 0,
                  max = 1,
                  step = 0.001,
                  width = 250
                ),
                numericInput(
                  inputId = 'sim_select_tkm',
                  label = 'Töötuskindlustusmaksed',
                  value = old_tax_rates["Töötuskindlustusmaksed"][[1]],
                  min = 0,
                  max = 1,
                  step = 0.001,
                  width = 250
                )
                
              ),
              
              
            ),
            
            # Ekspordimudel
            tabPanel(
              title = tags$h6(HTML('Ekspordikanalite<br/>mõju')),
              value = 'tab_eksport',
              
              tags$h6('Ekspordisisendi loomine'),
              
              radioButtons(
                inputId = 'eksport_sisend',
                label = 'Muutub:',
                choices = c('Eesti eksport' = 'eksport_tegevusala',
                            'Sihtriigi import' = 'eksport_sihtriik'),
                selected = 'eksport_tegevusala'
              ),
              
              radioButtons(
                inputId = "goods_services",
                label = "Väliskaubanduse tüüp:",
                choices = c("Kaubad ja teenused" = "goods_and_services", 
                            "Kaubad" = "goods",
                            "Teenused" = "services"),
                selected = "goods_and_services"
              ),
              
              
              
              # Tegevusala järgi (topK)
              conditionalPanel(
                condition = "input.eksport_sisend == 'eksport_tegevusala'",
                
                pickerInput(
                  inputId = "eksport_tegevusala_tgala",
                  label = "Vali tegevusala:",
                  choices = tegevusalad,
                  selected = tegevusalad[1]
                ),
                
                pickerInput(
                  inputId = "eksport_tegevusala_riik",
                  label = "Vali ekspordi sihtriik:",
                  choices = eksport_sisend_c$topK[origin_industry_text==tegevusalad[1], destination_country]
                )
              ),
              
              
              
              # Sihtriigi järgi
              conditionalPanel(
                condition = "input.eksport_sisend == 'eksport_sihtriik'",
                
                radioButtons(
                  inputId = "eksport_riik_agr",
                  label = "Kas muutub:",
                  choices = c("Sihtriigi koguimport" = "c",
                              "Sihtriigi kindla tegevusala import" =  "ci"),
                  selected = "c"
                ),
                
                pickerInput(
                  inputId = "eksport_riik_riik",
                  label = "Vali sihtriik:",
                  choices = sihtriigid
                ),
                
                conditionalPanel(
                  condition = "input.eksport_riik_agr == 'ci'",
                  
                  pickerInput(
                    inputId = "eksport_riik_tgala",
                    label = "Vali majandusharu:",
                    choices = majandusharud,
                    selected = majandusharud[1]
                  )
                )
                
              ),
              
              radioButtons(
                inputId = "input_type",
                label = "Sisendi tüüp:",
                choices = c("Suhteline muutus baastaseme (2019) suhtes (osakaaluna, kui 10% siis 0.1)" = "relative_change", 
                            "Absoluutne muutus baastaseme (2019) suhtes (eurodes)" = "absolute_change",
                            "Absoluuttase (eurodes)" = "absolute_level"),
                selected = "relative_change"
              ),
              
              
              numericInput(
                inputId = 'eksport_muutus',
                label = 'Sisendi väärtus:',
                value = 0,
                min = 0,
                max = 1,
                step = 0.001,
                width = 250
              )
              
            ),
            
            
            tabPanel(
              title = tags$h6(HTML('Lae üles oma<br/>stsenaarium')),
              value = 'tab_lae',
              
              tags$h6('Lae stsenaarium'),
              
              fileInput(
                inputId = 'uploadData',
                label = NULL,
                accept = ".xlsx",
                buttonLabel = "Vali...",
                placeholder = "Faili ei ole valitud"
              )
            )
            
          ),
          
          tags$hr(),
          
          tags$h6('Mudeli parameetrid'),
          
          
          radioButtons(
            inputId = 'multiplikaatorid',
            label = 'Kas kasutame I või II tüüpi multiplikaatoreid?',
            choices = c('I' = 'I',
                        'II' = 'II'),
            selected = 'I'
          ),
          conditionalPanel(
            condition = "input.multiplikaatorid == 'II'",
            numericInput(
              inputId = 'saastumaar',
              label = 'Millist säästumäära kasutame? (kui nt 10%, siis sisesta 0.1)',
              value = 0.14
            )
          ),
          
          conditionalPanel(
            condition = "input.inputTabset == 'tab_lae' | input.muutus_tyyp == 'muutus_maksud'",
            radioButtons(
              inputId = 'elastsused',
              label = 'Milliseid elastsusi kasutame?',
              choices = c('Nullid' = 'null',
                          'Miinusühed' = 'miinusyks',
                          'Ekspert' = 'ekspert'),
              selected = 'null'
            )
          ),
          
          
          conditionalPanel(
            condition = "input.inputTabset == 'tab_param' | input.inputTabset == 'tab_eksport' | input.inputTabset == 'tab_sts'",
            
            tags$hr(),
            
            downloadButton(outputId = 'downloadDataParam', 
                           label = 'Lae stsenaarium alla')),
          
          
          
          tags$hr(),
          
          actionButton("arvuta", tags$h5("Arvuta tulemused", style = "color: #BF2124;")),
          
          conditionalPanel(
            condition = "input.arvuta != 0",
            tags$hr(),
            downloadButton(outputId = 'downloadResults', 
                           label = 'Lae Tulemused alla')
          )
          
        ),
        
        # Output panel
        mainPanel(
          tabsetPanel(
            id = 'figTabset',
            
            tabPanel("Koondtulemused",
                     value = 'tabKoondtulemused',
                     tags$br(),
                     plotOutput('koondtulemused', height = '100%'),
                     tags$br(),
                     tags$hr(),
                     tags$br(),
                     tableOutput('koondtulemusedTabel')),
            
            tabPanel("Detailne maksutulu",
                     value = 'tabMaksutuluDetailne',
                     tags$br(),
                     plotOutput('maksutuluDetailne', height = '100%')),
            
            tabPanel("Kogutoodang",
                     value = 'tabKogutoodang',
                     tags$br(),
                     plotOutput('kogutoodang', height = '100%')),
            
            tabPanel("Lisandväärtus",
                     value = 'tabLisandvaartus',
                     tags$br(),
                     plotOutput('lisandvaartus', height = '100%')),
            
            tabPanel("Tööjõukulu",
                     value = 'tabToojoukulu',
                     tags$br(),
                     plotOutput('toojoukulu', height = '100%')),
            
            tabPanel("Kasum",
                     value = 'tabKasum',
                     tags$br(),
                     plotOutput('kasum', height = '100%')),
            
            tabPanel("Hõive",
                     value = 'tabHoive',
                     tags$br(),
                     plotOutput('hoive', height = '100%')),
            
            tabPanel("Maksutulu",
                     value = 'tabMaksutulu',
                     tags$br(),
                     plotOutput('maksutulu', height = '100%'))
          ),
          conditionalPanel(
            condition = 
            "(input.figTabset == 'tabKogutoodang' | 
            input.figTabset == 'tabLisandvaartus' |
            input.figTabset == 'tabToojoukulu' |
            input.figTabset == 'tabKasum' |
            input.figTabset == 'tabHoive' |
            input.figTabset == 'tabMaksutulu') &
            input.arvuta != 0",
            actionButton("figSortBtn", "Sorteeri")
          )
        )
      )
    ),
    
    tabPanel(
      title = 'Mudeli kasutamine',
      column(
        width = 8,  offset = 2,
        markdown(kasutamine))
      )
      
    
  )
  
)








################################################################################
# Define server logic
################################################################################
server <- function(input, output, session) {
  
  
  # Ekspordi tegevusala järgi riigi valik lähtuvalt tegevusalast
  observeEvent(input$eksport_tegevusala_tgala, {
    
    choices = eksport_sisend_c$topK[origin_industry_text==input$eksport_tegevusala_tgala, destination_country]
    names(choices) = paste0(
      eksport_sisend_c$topK[origin_industry_text==input$eksport_tegevusala_tgala, destination_country],
      ' (ekspordi osakaal ',
      round(eksport_sisend_c$topK[origin_industry_text==input$eksport_tegevusala_tgala, export_share], 3),
      ')')
    
    updatePickerInput(
      session, 
      'eksport_tegevusala_riik',
      choices = choices)
  })
  
  output$eksport_osakaal <- renderText({
    eksport_sisend_c$topK[origin_industry_text==input$eksport_tegevusala_tgala & 
                            destination_country == input$eksport_tegevusala_riik, 
                          export_share] %>% round(3)
  })
  
  
  input_list <- reactive({
    
    if(input$inputTabset == 'tab_lae'){
      
      req(input$uploadData)
      input_list <- input_data_f(openxlsx::loadWorkbook(input$uploadData$datapath))
      
    }
    
    if(input$inputTabset == 'tab_param'){
      
      validate(
        need(input$sim_select_tegevusala, message = 'Tegevusala sisend on puudu!'),
        need(input$sim_select_hoive, message = 'Hõive sisend on puudu! Kui hõives muutusi ei ole, siis jäta sisend 0-ks.'),
        need(input$sim_select_kategooria_value, message = 'Lisanduv lõppkasutamine on puudu! Kui lõppkasutamises muutusi ei ole, siis jäta sisend 0-ks.'),
        need(input$sim_select_km, message = 'Käibemaksu sisend on puudu! Kui käibemaksu määras muutusi ei ole, siis jäta sisendiks vaikeväärtus (kehtiv maksumäär).'),
        need(input$sim_select_kmv, message = 'Käibemaksu vähendatud määra sisend on puudu! Kui määras muutusi ei ole, siis jäta sisendiks vaikeväärtus (kehtiv maksumäär).'),
        need(input$sim_select_sm, message = 'Sotsiaalmaksu sisend on puudu! Kui määras muutusi ei ole, siis jäta sisendiks vaikeväärtus (kehtiv maksumäär).'),
        need(input$sim_select_tkm, message = 'Töötuskindlustusmakse sisend on puudu! Kui määras muutusi ei ole, siis jäta sisendiks vaikeväärtus (kehtiv maksumäär).')
      )
      
      if(input$muutus_tyyp == 'muutus_loppkasutamine'){
        kategooria_value = input$sim_select_kategooria_value
        km = 0.2
        kmv = 0.09
        sm = 0.33
        tkm = 0.024
        hoive = 0
      }
      
      if(input$muutus_tyyp == 'muutus_hoive'){
        kategooria_value = input$sim_select_kategooria_value
        km = 0.2
        kmv = 0.09
        sm = 0.33
        tkm = 0.024
        hoive = input$sim_select_hoive
      }
      
      if(input$muutus_tyyp == 'muutus_maksud'){
        kategooria_value = 0
        km = input$sim_select_km
        kmv = input$sim_select_kmv
        sm = input$sim_select_sm
        tkm = input$sim_select_tkm
        hoive = 0
      }
      
      
      
      input_list <- gen_sim_stsenaarium(inp_list = input_list_templ,
                                        tegevusala = input$sim_select_tegevusala,
                                        kategooria = input$sim_select_kategooria,
                                        kategooria_value = kategooria_value,
                                        km = km,
                                        kmv = kmv,
                                        sm = sm,
                                        tkm = tkm,
                                        hoive = hoive)
    }
    
    if(input$inputTabset == 'tab_eksport'){
      
      if(input$eksport_sisend == 'eksport_tegevusala'){
        
        input_list <- gen_eksport_stsenaarium_tegevusala(sisend = eksport_sisend_c,
                                                         input_template = input_list_templ,
                                                         goods_services = input$goods_services,
                                                         input_type = input$input_type,
                                                         eksport_tegevusala_tgala = input$eksport_tegevusala_tgala,
                                                         eksport_tegevusala_riik = input$eksport_tegevusala_riik,
                                                         eksport_muutus = input$eksport_muutus)
      }
      
      if(input$eksport_sisend == 'eksport_sihtriik'){
        
        input_list <- gen_eksport_stsenaarium_riik(sisend_ci = eksport_sisend_ci,
                                                   sisend_c = eksport_sisend_c,
                                                   input_template = input_list_templ,
                                                   eksport_riik_agr = input$eksport_riik_agr,
                                                   goods_services = input$goods_services,
                                                   input_type = input$input_type,
                                                   eksport_riik_tgala = input$eksport_riik_tgala,
                                                   eksport_riik_riik = input$eksport_riik_riik,
                                                   eksport_muutus = input$eksport_muutus)
        
      }
      
    }
    
    
    if(input$inputTabset == 'tab_sts'){
      
      if(input$simulatsioon == 'sim_lt'){
        input_list <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'Andmete sisestamise vormi näidis-LT liigid.xlsx')))
        
      }
      
      if(input$simulatsioon == 'sim_kodu'){
        input_list <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'Andmete sisestamise vormi näidis-kodumajapidamised.xlsx')))
        
      }
      
      if(input$simulatsioon == 'sim_valitsus'){
        input_list <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'Andmete sisestamise vormi näidis-valitsussektor.xlsx')))
        
      }
      
      if(input$simulatsioon == 'sim_invest'){
        input_list <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'Andmete sisestamise vormi näidis-investeeringud.xlsx')))
        
      }
      
      if(input$simulatsioon == 'sim_eksp'){
        input_list <- input_data_f(openxlsx::loadWorkbook(file.path(exa_loc, 'Andmete sisestamise vormi näidis-eksport_st.xlsx')))
        
      }
      
    }
    
    return(input_list)
  })
  
  
  # Tulemuste arvutamine (peale actionButton'i vajutamist)
  tulemused <- eventReactive(input$arvuta, {
    if(input$multiplikaatorid == 'II'){
      validate(
        need(input$saastumaar, message = 'Säästumäär on puudu. Kui säästumääras muudatusi ei ole, siis jäta vaikeväärtus.')
      )
    }
    
    
    run_model(input_data = input_list(),
              #intervall = input$intervall,
              intervall = 'aasta',
              elastsused = input$elastsused,
              multiplikaatorid = input$multiplikaatorid,
              saastumaar = input$saastumaar*100,
              label_raha = "eurot",
              label_inimesed = "inimest",
              murra_y=200, murra_f=200,
              f_axes_y = T,
              hoive = "ETU")
  })
  
  
  # Mitu rida (stsenaariumit) joonisel on. Sellest tuletame joonise kõrguse
  joonis_rida_arv <- reactive(length(unique(tulemused()$Joonised$Koondtulemused$data$Stsenaarium)))
  
  
  # Jooniste väärtuste sorteerimine (tegevusalade lõikes joonised)
  sortval <- reactiveVal(TRUE)
  observeEvent(input$figSortBtn, {
    sortval(!sortval())
    if(sortval()){
      figSortBtn_label = 'Sorteeri'
    } else {
      figSortBtn_label = 'Algjärjestus'
    }
    updateActionButton(session, 
                       "figSortBtn", 
                       label = figSortBtn_label)
  })
  
  
  
  # Joonised - koondtulemused
  output$koondtulemused <- renderPlot({
    tulemused()$Joonised$Koondtulemused
  }, 
  res = 96,
  height = function(){
    if(joonis_rida_arv() == 1){
      return(400)
    } else {
      return(joonis_rida_arv() * 15 + 120 + 30)
    }
  }
  )
  
  # Koondtulemuste tabel
  output$koondtulemusedTabel <- renderTable({
    tab <- tulemused()$Andmed$`Koond pikk`$totals %>% 
      as.data.table() %>% 
      .[, Näitaja := factor(Näitaja, levels = c("Kogutoodang", "Lisandväärtus", "Tööjõukulu", "Kasum", "Maksutulu", "Hõive"))] %>% 
      dcast(Stsenaarium~`Näitaja`, value.var = 'Value')
    tab[,2:7] <- lapply(tab[,2:7], FUN=function(x) prettyNum(round(x, 0), big.mark=" "))
    if(joonis_rida_arv() == 1){
      tab[, Stsenaarium := NULL]
    }
    return(tab)
  }
  )
  
  # Joonised - detailne maksutulu
  output$maksutuluDetailne <- renderPlot({
    tulemused()$Joonised$`Maksutulu-detailne`
  }, 
  res = 96,
  height = function() joonis_rida_arv() * 13 * 7 + 120
  )
  

  # Joonised - tegevusalade lõikes
  output$kogutoodang <- renderPlot({
    
    fig <- tulemused()$Joonised$Kogutoodang
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
    
  }, 
  res = 96,
  height = 900
  )
  
  output$lisandvaartus <- renderPlot({
    
    fig <- tulemused()$Joonised$Lisandväärtus
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
    
  }, 
  res = 96,
  height = 900
  )
  
  output$toojoukulu <- renderPlot({
    
    fig <- tulemused()$Joonised$Tööjõukulu
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
    
  }, 
  res = 96,
  height = 900
  )
  
  output$kasum <- renderPlot({
    
    fig <- tulemused()$Joonised$Kasum
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
    
  }, 
  res = 96,
  height = 900
  )
  
  output$hoive <- renderPlot({
    
    fig <- tulemused()$Joonised$Hõive
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
    
  }, 
  res = 96,
  height = 900
  )
  
  output$maksutulu <- renderPlot({
    
    fig <- tulemused()$Joonised$Maksutulu
    
    if(sortval()){
      return(fig)
    } else {
      fig$data$tegevusala_lyh <- reorder(fig$data$tegevusala_lyh, fig$data$Value)
      return(fig)
    }
  }, 
  res = 96,
  height = 900
  )
  
  
  
  
  # Tulemuste andmete allalaadimine
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("tulemused.xlsx", sep = "")
    },
    content = function(con) {
      openxlsx::write.xlsx(tulemused()$Andmed$`Koond pikk`, file = con)
    }
  )
  
  # Stsenaariumi allalaadimine
  output$downloadDataParam <- downloadHandler(
    filename = function() {
      paste("stsenaarium.xlsx", sep = "")
    },
    content = function(con) {
      openxlsx::write.xlsx(format_input_list(input_list()), file = con)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
