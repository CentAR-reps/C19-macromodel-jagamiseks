#Directories
wd <<- '.'
module_loc <<- file.path(wd, "8.extras", "export-module-app-files")

# Load necessary scripts
source(file.path(module_loc, "export_app_packages.R"), encoding = "UTF-8")
source(file.path(module_loc, "export_app_fns.R"), encoding = "UTF-8")

# Load data
load(file.path(module_loc, "export_module", "export_baseline.Rda"))
load(file.path(module_loc, "export_module", "ibci_export_shares.Rda"))
load(file.path(module_loc, "export_module", "ibc_export_shares.Rda"))


export_baseline <<- export_baseline
ibci_export_shares <<- ibci_export_shares
ibc_export_shares <<- ibc_export_shares

#period_start <- as.Date("2021-06-01")
sihtriigid <- unique(ibc_export_shares$destination_country)
majandusharud_df <- ibc_export_shares %>% 
  dplyr::select(origin_industry, origin_industry_text) %>% 
  unique() %>% 
  mutate(haru = paste(origin_industry, origin_industry_text, sep = " ")) %>%
  #tidyr::unite(col = haru, origin_industry, origin_industry_text, sep = " ", remove = FALSE) %>%
  dplyr::select(origin_industry, haru) %>% 
  as.data.frame()
majandusharud <- c(majandusharud_df$origin_industry)
names(majandusharud) = majandusharud_df$haru

ui <- fluidPage(
  useShinyjs(),
  # Sidebar with a slider input for number of bins 
  fluidRow(
    
    
    # Show a plot of the generated distribution
    column(width = 11,
           
           tabsetPanel(type = "tabs",
                       
                       tabPanel("Sisend / Input",
                                selectizeInput("goods_services",
                                               "Väliskaubanduse tüüp / Export type:",
                                               choices = c("kaubad ja teenused / goods and services" = "goods_and_services", 
                                                           "kaubad / goods" = "goods",
                                                           "teenused / services" = "services"),
                                               selected = "kaubad ja teenused / goods and services"),
                                selectizeInput("input_type",
                                               "Sisendi tüüp / Input type:",
                                               choices = c("suhteline muutus / relative change" = "relative_change", 
                                                           "absoluutne muutus / absolute change" = "absolute_change",
                                                           "absoluuttase / absolute level" = "absolute_level"),
                                               selected = "absoluutne muutus / absolute change"),
                                selectizeInput("input_agr",
                                               "Sihtturu tase / Destination aggregation level",
                                               c("riik-majandusharu / country-industry" =  "ci",
                                                 "riik / country" = "c"),
                                               selected = "riik-majandusharu / country-industry"),
                                radioButtons("scenarios_type", "Tsenaariumi tüüp / Scenarios type", 
                                             choices = c("Tsenaariumid / Scernarios", "Aegrida / Timeseries"),
                                             selected = "Tsenaariumid / Scernarios",
                                             inline = FALSE),
                                textInput("scenarios", "Tsenaariumid / Scenarios", placeholder = "Eraldaja koma / separate by comma: (a,b,c)"),
                                airDatepickerInput("period_start_date",
                                                   label = "Perioodi algus / Period start",
                                                   value = "2022-01-01",
                                                   maxDate = "2030-01-01",
                                                   minDate = "2021-01-01",
                                                   view = "months", 
                                                   minView = "months",
                                                   dateFormat = "yyyy-mm"
                                ),
                                sliderInput("perioodi_pikkus", 
                                            label = "Perioodi pikkus kuudes / Period length in months",
                                            min = 1,
                                            max = 18,
                                            step = 1,
                                            value = 1),
                                radioButtons("intervall", "Intervall / intarval", c("kuu / month", "aasta / year"), "aasta / year", inline = TRUE),
                                hr(),
                                actionButton("reset_all", "Lähtesta kõik / Reset all"),
                                actionButton("reset_selected", "Lähtesta praegune tabel / Reset current table"),
                                hr(),
                                p(em("Väärtuste sisestamise juhend / Instruction:")),
                                tags$ol(
                                  tags$li(tags$p("Väärtuste sisestamiseks tee topeltklikk muudetaval real. Andmeid saab sisestada 1 rea kaupa."),
                                          tags$p(em("To insert values, double-click on row you want to change. One row can be changed at once."))), 
                                  tags$li(tags$p("Väärtuste salvestamiseks vajuta Ctrl + Enter."),
                                          tags$p(em("To save inserted values, press Ctrl + Enter."))), 
                                ),
                                tabsetPanel(id = "input_tab",
                                            type = "tabs",
                                            tabPanel("topK", 
                                                     DTOutput('topKtbl')),
                                            tabPanel("Sihtturg / Destination market",
                                                     selectizeInput("sihtriik_input", 
                                                                    label = "Vali sihtriik / Select destination country",
                                                                    choices = sihtriigid),
                                                     DTOutput('sihtriiktbl')))),
                       
                       tabPanel("Väljund / Output", 
                                h2("Kogu ekspordi muutus / Total export change"),
                                DTOutput("export_demand_change_total"),
                                
                                h2("Majandusharude eksport / Export change by industries"),
                               # h3("Ekspordi absoluutmuutuse tabel / Table of absolute change"),
                                DTOutput("export_demand_change"),
                                #h3("Ekspordi muutuse joonis"),
                                plotlyOutput("valjund_joonis", height = "800px")),
                       
                       tabPanel("Jaotused / Distributions",
                                h2("Koondpilt sihtriigi tasemel"),
                                plotlyOutput("c_jaotus", height = "600px"),
                                #plotOutput("c_jaotus", height = "600px"),
                                
                                h2("Top 25 sihtturgu majandusharu tasemel / Top 25 destinations"),
                                selectizeInput("jaotused_haru", "Vali majandusharu / Select industry", choices = majandusharud),
                                plotlyOutput("jaotus_joonis_c", height = "500px"),
                                p(em("Märkus: 'ROW' tähistab OECD andmetesse mittekuuluvaid riike, mitte kõiki neid riike, mis ei ole top25 seas")),
                                p(em("Note: 'ROW' denotes countries that are not in OECD dataset")),
                                hr(),
                                plotlyOutput("jaotus_joonis_ci", height = "500px")
                                
                                #plotlyOutput("ci_jaotus", height = "600px")
                       )
           )
           
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if (input$scenarios_type == "Tsenaariumid / Scernarios"){
      shinyjs::show("scenarios")
      shinyjs::hide("period_start_date")
      shinyjs::hide("perioodi_pikkus")
      updateRadioButtons(session, "intervall", "Intervall / intarval", c("kuu / month", "aasta / year"), "aasta / year", inline = TRUE)
      shinyjs::show("intervall")
    } else{
      shinyjs::hide("scenarios")
      updateTextInput(session, "scenarios", "Tsenaariumid / Scenarios", value = "", placeholder = "Eralda tsenaariumid ',' / Separate scenarios by ','")
      shinyjs::show("period_start_date")
      shinyjs::show("perioodi_pikkus")
      updateRadioButtons(session, "intervall", "Intervall / intarval", c("kuu / month", "aasta / year"), "kuu / month", inline = TRUE)
      shinyjs::hide("intervall")
    }
  })
  
  scenarios <- reactiveVal()
  
  observe({
    scens <- unique(trimws(unlist(strsplit(input$scenarios, ","))))
    if (length(scens) == 0 || scens == "") scens = NULL
    scenarios(scens)
  })
  
  period_start <- reactiveVal()
  period_end <- reactiveVal()
  
  observe({
    period_start(input$period_start_date)
  })
  
  observe({
    #req(input$scenarios_type != "Tsenaariumid / Scernarios")
    period_end(period_start() %m+% months(input$perioodi_pikkus - 1))
  })
  
  algsisend_ci <- reactiveVal()
  algsisend_c <- reactiveVal()
  observe({
    algsisend_ci(create_user_input_table_app(k = 5, 
                                             destination_agr = "ci", 
                                             scenarios = scenarios(), 
                                             period_start = period_start(), 
                                             period_end = period_end()))
    })
  observe({
    algsisend_c(create_user_input_table_app(k = 5, 
                                            destination_agr = "c", 
                                            scenarios = scenarios(),
                                            period_start = period_start(), 
                                            period_end = period_end()))
    })

  topKsisend <- reactiveVal()

  observe({
    req(!is.null(algsisend_ci()))
    topKsisend(algsisend_ci()$topK)
  })

  observe({
    tmp = input$reset_all
    if(input$input_agr == "ci"){
      topKsisend(algsisend_ci()$topK)
    }else{
      topKsisend(algsisend_c()$topK)
    }
  })

  sihtriiksisend <- reactiveVal()
  observe({
    sihtriiksisend(algsisend_ci()[-1])
  })

  observe({
    tmp = input$reset_all
    if(input$input_agr == "ci"){
      sihtriiksisend(algsisend_ci()[-1])
    }else{
      sihtriiksisend(algsisend_c()$countries)
    }
  })

  observe({
    if (input$input_agr == "ci"){
      show("sihtriik_input")
    } else {
      hide("sihtriik_input")
    }
  })
  
  observeEvent(input$reset_selected, {
    if (input$input_tab == "topK"){
      if(input$input_agr == "ci"){
        topKsisend(algsisend_ci()$topK)
      }else{
        topKsisend(algsisend_c()$topK)
      }
    }
    if (input$input_tab == "Sihtturg"){
      if(input$input_agr == "ci"){
        sihtriiksisend_lahtesta = sihtriiksisend()
        sihtriiksisend_lahtesta[[input$sihtriik_input]] = algsisend_ci()[[input$sihtriik_input]]
        sihtriiksisend(sihtriiksisend_lahtesta)
      }else{
        sihtriiksisend(algsisend_c()$countries)
      }
    }
    if (!(input$input_tab %in% c("topK", "Sihtturg"))){
      stop("tab valesti määratud")
    }
  })

  disable_cols_topK <- reactiveVal(0:5)

  observe({
    if(input$input_agr == "ci"){
      disable_cols_topK(0:5)
    }else{
      disable_cols_topK(0:3)
    }
  })

  disable_cols_sihtriik <- reactiveVal(0:1)

  observe({
    if(input$input_agr == "ci"){
      disable_cols_sihtriik(0:1)
    }else{
      disable_cols_sihtriik(0)
    }
  })

  topKtbl_cn <- reactiveVal()
  observe({
    tmp = c("majandusharu kood / industry code" = "origin_industry",
            "majandusharu / industry" = "origin_industry_text",
            "sihtriik / destination country" = "destination_country",
            "ekspordi osakaal / export share" = "export_share")
    if(input$input_agr == "ci"){
      tmp = c(tmp, c("sihtharu kood / destination industry code" = "destination_industry",
                     "sihtharu / destination industry" = "destination_industry_text"))
    }

    topKtbl_cn(tmp)
  })

  # output$topKtbl = renderDT(
  #     datatable(topKsisend(), editable = list(target = 'row', disable = list(columns = disable_cols_topK())),
  #               selection = 'none',
  #               rownames = FALSE, options = list(lengthChange = FALSE, dom = 'tp'),
  #               colnames = topKtbl_cn()) %>% formatRound(columns=c("ekspordi osakaal"), digits=5), server = TRUE
  #
  # )

  observe({sihtriik <<- sihtriiksisend()})

  output$topKtbl = renderDT(
    topKsisend(), editable = list(target = 'row', disable = list(columns = disable_cols_topK())),
    selection = 'none',
    rownames = FALSE, options = list(lengthChange = FALSE, dom = 'tp'),
    colnames = topKtbl_cn() , server = TRUE

  )

  observeEvent(input$topKtbl_cell_edit, {
    uus_topKsisend <<- editData(topKsisend(), input$topKtbl_cell_edit, 'topKtbl', rownames = FALSE)
    topKsisend(uus_topKsisend)
  })


  sihtriiksisend_kuva <- reactive({
    if(input$input_agr == "ci"){
      sihtriiksisend()[[input$sihtriik_input]]
    } else {
      sihtriiksisend()
    }
  })

  sihtriiktbl_cn <- reactiveVal()
  observe({
    if(input$input_agr == "ci"){
      tmp = c("sihtharu kood / destination industry code" = "destination_industry",
              "sihtharu / destination industry" = "destination_industry_text")
    } else{
      tmp = c("sihtriik / destination country" = "destination_country")
    }

    sihtriiktbl_cn(tmp)
  })

  output$sihtriiktbl = renderDT(
    sihtriiksisend_kuva(), editable = list(target = 'row', disable = list(columns = disable_cols_sihtriik())),
    selection = 'none', server = TRUE,
    rownames = FALSE, options = list(lengthChange = FALSE, dom = 'tp'),
    colnames = sihtriiktbl_cn()
  )

  observeEvent(input$sihtriiktbl_cell_edit, {

    if(input$input_agr == "ci"){
      uus_sihtriiksisend <- sihtriiksisend()
      uus_sihtriiksisend_riik <<- editData(sihtriiksisend()[[input$sihtriik_input]], input$sihtriiktbl_cell_edit, 'sihtriiktbl', rownames = FALSE)
      uus_sihtriiksisend[[input$sihtriik_input]] = uus_sihtriiksisend_riik
      sihtriiksisend(uus_sihtriiksisend)
    } else{
      uus_sihtriiksisend <<- editData(sihtriiksisend(), input$sihtriiktbl_cell_edit, 'sihtriiktbl', rownames = FALSE)
      sihtriiksisend(uus_sihtriiksisend)
    }
  })

  export_change_long <- reactiveVal()
  observe({
    export_change_long(calculate_export_demand_change_long_app(input_type = input$input_type, 
                                                               input_agr = input$input_agr, 
                                                               topKsisend(), 
                                                               sihtriiksisend(), 
                                                               intervall = input$intervall,
                                                               export_baseline = export_baseline,  
                                                               goods_services = input$goods_services))
  })

  export_change <- reactiveVal()
  observe({
    export_change(calculate_export_demand_change_app(export_change_long()))
  })
  
  output$export_demand_change_total <- renderDT(
    export_change_long() %>%
      group_by(month) %>%
      summarise(absoluutmuutus = sum(simulated_amount) - sum(export_amount),
                suhteline_muutus = absoluutmuutus / sum(export_amount),
                .groups = "drop") %>%
      rename(`tsenaarium / scenario` = month,
             `suhteline muutus / relative change` = suhteline_muutus,
             `absoluutmuutus / absolute change` = absoluutmuutus),
    rownames = FALSE, options = list(lengthChange = FALSE, dom = 'tp')
  )

  output$export_demand_change <- renderDT(server = FALSE,{
    export_change() %>%
      rename(`majandusharu kood / industry code` = industry_code,
             `majandusharu / industry` = industry_text)},
    rownames = FALSE,  extensions = 'Buttons', options = list(lengthChange = FALSE, dom = 'tpB', buttons = c('copy', 'excel'))
  )

  output$valjund_joonis <- renderPlotly({
    export_change_tmp = export_change_long() %>%
      rename(`majandusharu kood` = industry_code,
             majandusharu = industry_text,
             `absoluutne muutus` = absolute_change,
             `suhteline muutus` = relative_change)

    p <- ggplot(export_change_tmp, aes(x = `majandusharu kood`, text = majandusharu, y = `absoluutne muutus`, fill = `suhteline muutus`)) +
      geom_bar(stat = "identity") +
      facet_wrap(~month) +
      coord_flip() +
      labs(x = "", y = "absoluutne muutus / absolute change (EUR)", fill = "suhteline muutus \nrelative change") +
      scale_fill_gradient2(low = "darkred", high = "darkgreen", mid = "yellow")

    ggplotly(p)
    #ggplotly(p, tooltip = c("x", "text", "y", "fill"))

  })

  # output$muutus_heatmap <- renderPlotly({
  #     export_change_tmp = export_change()
  #     mat <- export_change_tmp %>%
  #         as.data.frame()
  #
  #     rownames(mat) <- mat[,1]
  #     mat <- mat %>% dplyr::select(-industry_code, -industry_text)
  #     mat <- as.matrix(mat)
  #
  #
  #     labels <- export_change_tmp %>%
  #         as.data.frame()
  #
  #     rownames(labels) <- labels[,1]
  #     labels <- labels %>% dplyr::select(-industry_code)
  #     labels <- as.matrix(labels)
  #
  #     p <- heatmaply(mat,
  #                    dendrogram = "none",
  #                    xlab = "", ylab = "",
  #                    main = "Ekspordi muutused",
  #                    grid_color = NA,
  #                    grid_gap = 0,
  #                    titleX = FALSE,
  #                    hide_colorbar = FALSE,
  #                    label_names = c("Sisemaine majandusharu", "Kuu", "Ekspordi muutus"),
  #                    fontsize_row = 6, fontsize_col = 6,
  #                    labCol = colnames(mat),
  #                    labRow = rownames(mat),
  #                    heatmap_layers = theme(axis.line=element_blank()),
  #                    custom_hovertext = labels,
  #                    plot_method = "ggplot"
  #     )
  #     p
  # })

  ############################################
  ### Jaotused ###

  output$c_jaotus <- renderPlotly({

    p <- ggplot(ibc_export_shares, aes(x = destination_country, y = origin_industry, fill = export_share)) +
      geom_tile()


    ggplotly(p)
  })

  # output$c_jaotus <- renderPlotly({
  #   mat <- ibc_export_shares %>%
  #     mutate(export_share = round(export_share,5)) %>%
  #     spread(key = destination_country, value = export_share) %>%
  #     as.data.frame()
  #
  #   rownames(mat) <- mat[,1]
  #   mat <- mat %>% dplyr::select(-origin_industry, -origin_industry_text)
  #   mat <- as.matrix(mat)
  #
  #
  #   labels <- ibc_export_shares %>%
  #     dplyr::select(-export_share) %>%
  #     spread(key = destination_country, value = origin_industry_text) %>%
  #     as.data.frame()
  #
  #   rownames(labels) <- labels[,1]
  #   labels <- labels %>% dplyr::select(-origin_industry)
  #   labels <- as.matrix(labels)
  #
  #   p <- heatmaply(mat,
  #                  dendrogram = "none",
  #                  xlab = "", ylab = "",
  #                  main = "Ekspordi osakaal sihtriigi lõikes",
  #                  grid_color = NA,
  #                  grid_gap = 0,
  #                  titleX = FALSE,
  #                  hide_colorbar = FALSE,
  #                  label_names = c("Sisemaine majandusharu", "Sihtriik", "Ekspordi osakaal"),
  #                  fontsize_row = 6, fontsize_col = 6,
  #                  labCol = colnames(mat),
  #                  labRow = rownames(mat),
  #                  heatmap_layers = theme(axis.line=element_blank()),
  #                  custom_hovertext = labels,
  #                  plot_method = "ggplot"
  #   )
  #   p
  # })



  output$jaotus_joonis_c <- renderPlotly({
    ibc_export_shares_tmp <- ibc_export_shares %>%
      filter(origin_industry == input$jaotused_haru) %>%
      arrange(desc(export_share)) %>%
      slice(1:25) %>%
      mutate(destination_country = reorder(destination_country, export_share))

    p <- ggplot(ibc_export_shares_tmp, aes(x = destination_country, y = export_share)) +
      geom_bar(stat = "identity", fill = "steelblue3") +
      coord_flip() +
      labs(x = "", y = "ekspordi osakaal / export share")

    ggplotly(p)

  })

  output$jaotus_joonis_ci <- renderPlotly({
    ibci_export_shares_tmp <- ibci_export_shares %>%
      filter(origin_industry == input$jaotused_haru) %>%
      arrange(desc(export_share)) %>%
      slice(1:25) %>%
      tidyr::unite(col = sihtriik_haru, destination_country, destination_industry, sep = " ", remove = FALSE) %>%
      mutate(sihtriik_haru = reorder(sihtriik_haru, export_share))

    p <- ggplot(ibci_export_shares_tmp, aes(x = sihtriik_haru, y = export_share, fill = destination_country, text = destination_industry_text)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "", y = "ekspordi osakaal / export share") +
      theme(legend.position = "none")

    ggplotly(p)

  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
