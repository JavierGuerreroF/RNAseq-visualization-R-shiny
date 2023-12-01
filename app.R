
## RNAseq ANALISYS DATA DEMO RANDOM DATA
# JAVIER GUERRERO FLORES
# 24/11/23



## app.R ##
library(shinydashboard)
library(shinyWidgets)
library(DT)
#library(shinydashboardPlus)

###
library(shinycssloaders)
library(ggplot2)
library(shinyBS)
library(dplyr)
library(stringr)
library(RColorBrewer)

# Javascript code to identity of tabItem
# Now the name of the active tab is given by input[["activeTab"]]:
js <- '
$(document).on("shiny:connected", function(){
  Shiny.setInputValue("activeTab", $("li.active>a").attr("data-value"));
  $("a[data-toggle=tab]").on("show.bs.tab", function(e){
    Shiny.setInputValue("activeTab", $(this).attr("data-value"));
  });
});
'

set_col <- brewer.pal(3,"Accent")

KT_genes <- read.csv(paste0("data/diff_expressed_genes_tsv/KT_all_merged.csv"))
KT_genes <- unique(KT_genes$id)

Ophiostoma_genes <- read.csv(paste0("data/diff_expressed_genes_tsv/Ophiostoma_all_merged.csv"))
Ophiostoma_genes <- unique(Ophiostoma_genes$id)
# 

KT_GO_categories <- read.csv(paste0("data/Results/GO_enrichment_KT.tsv"),sep = "\t", header = T)
KT_GO_categories_id<- unique(KT_GO_categories$GO.ID)
KT_GO_categories_term<- unique(KT_GO_categories$Term)

Ophiostoma_GO_categories<- read.csv(paste0("data/Results/GO_enrichment_Ophiostoma.tsv"),sep = "\t", header = T)
Ophiostoma_GO_categories_id <- unique(Ophiostoma_GO_categories$GO.ID)
Ophiostoma_GO_categories_term <- unique(Ophiostoma_GO_categories$Term)
#"Treatment <br/> FaOHvsCTL","Time","Medium","OVSC"),
ui <- 
  
  dashboardPage(
    
  
  
  dashboardHeader(title = "GO ENRICHMENT",
                  titleWidth = 350),
  
  
  dashboardSidebar(width = 350,
                   
                   #bar-chart-o
                   sidebarMenu(
                     menuItem("Enrichment / Volcano plot", tabName = "enrichment",icon=icon("chart-bar")),
                     menuItem("Search by ...",tabName = "search",icon=icon("magnifying-glass"),
                              # startExpanded = T,
                              menuSubItem("Gene",
                                          tabName = "search_gene"),
                              menuSubItem("Category",
                                          tabName = "search_category")),
                     menuItem("Information & bibliography", tabName = "information",icon = icon("book"))
                   ),
                   
                   radioGroupButtons(
                     inputId = "TSD",
                     label = "View volcano plot:",
                     choices = c("Show","Hide"),
                     justified = TRUE,
                     selected = "Show",
                     size="lg",
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square", 
                                    style = "color: black"),
                       no = tags$i(class = "fa fa-square-o", 
                                   style = "color: black")
                     )
                   ),
                   
                   conditionalPanel(    
                     condition = "input.TSD == 'Hide'",
                     
                     hr(),
                                           
                                           radioGroupButtons(
                                             inputId = "parameter",
                                             label = "Parameter:",
                                             choices = c("FDR","pval"),
                                             selected = "pval",
                                             justified = TRUE,
                                             size="normal",
                                             checkIcon = list(
                                               yes = tags$i(class = "fa fa-check-square", 
                                                            style = "color: 	#dc143c"),
                                               no = tags$i(class = "fa fa-square-o", 
                                                           style = "color: 	#dc143c")
                                             )
                                           ),
                                           
                                           
                                           numericInput(
                                             inputId="pvalue",
                                             label="Threshold (default = 0.05):",
                                             value=0.05,
                                             min = 0,
                                             max = 1,
                                             step = 0.001,
                                             
                                             # width = NULL
                                           ),
                                           
                                           
                                           pickerInput(
                                             inputId = "ontology",
                                             label = "Select ontology:", 
                                             choices = c("Molecular Function (MF)" = "MF",
                                                         "Biological Process (BP)" = "BP",
                                                         "Cellular Component (CC)" = "CC"),
                                             selected = c("Molecular Function" = "MF",
                                                          "Biological Process" = "BP",
                                                          "Cellular Component" = "CC"),
                                             options = list(
                                               `actions-box` = TRUE), 
                                             multiple = TRUE
                                           )
                   
                       ),
                   
              
                   
                   hr(),
                   radioGroupButtons(
                     inputId = "organism",
                     label = "Reference genome (Organism):",
                     choices = c("Ophiostoma","KT"),
                     justified = TRUE,
                     size="lg",
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square", 
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o", 
                                   style = "color: steelblue")
                     )
                   ),
                   hr(),
                   radioGroupButtons(
                     inputId = "analysis",
                     label = "Analysis:",
                     choices = list(
                     "Treatment: <b>FaOH vs CTL</b>"="Treatment",
                     "Time: <b>early vs late</b>"="Time",
                     "Medium: <b>GPP vs BWP</b>"="Medium",
                     "Organism VS consortium"="OVSC"),
                     justified = TRUE,
                     direction = "vertical",
                     size = "normal",
                     checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square", 
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o", 
                                   style = "color: steelblue")
                      )
                    ),
                   conditionalPanel(
                     condition = "input.analysis == 'Treatment' | input.analysis == 'Time' | input.analysis == 'Medium'",
                     radioGroupButtons(
                       inputId = "consortium",
                       label = "Consortium:",
                       choices = list("Yes"=TRUE,"No"=FALSE),
                       justified = TRUE,
                       size = "normal",
                       checkIcon = list(
                         yes = tags$i(class = "fa fa-check-square", 
                                      style = "color: steelblue"),
                         no = tags$i(class = "fa fa-square-o", 
                                     style = "color: steelblue")
                       )
                     )
                   ),
    
    hr(),
    h4("Conditions selection",align="center"),
   # conditionalPanel(output$analisys, ..., ns = NS(NULL)),
   conditionalPanel(
     condition = "input.analysis == 'Treatment' | input.analysis == 'Time' | input.analysis == 'OVSC'",
     radioGroupButtons(
       inputId = "contrast_medium",
       label = "Medium:",
       choices = c("GPP","BWP"),
       size = "normal",
       direction =  "horizontal",
       justified = TRUE,
       checkIcon = list(
         yes = tags$i(class = "fa fa-check-square",
                      style = "color: steelblue"),
         no = tags$i(class = "fa fa-square-o",
                     style = "color: steelblue")
       ))
    ),
   conditionalPanel(
     condition = "input.analysis == 'Medium' | input.analysis == 'Time' | input.analysis == 'OVSC'",
     radioGroupButtons(
       inputId = "contrast_treatment",
       label = "Treatment:",
       choices = c("FaOH","CTL"),
       size = "normal",
       direction =  "horizontal",
       justified = TRUE,
       selected=NULL,
       checkIcon = list(
         yes = tags$i(class = "fa fa-check-square",
                      style = "color: steelblue"),
         no = tags$i(class = "fa fa-square-o",
                     style = "color: steelblue")
       )
     )
   ),
   conditionalPanel(
     condition = "input.analysis == 'Medium' | input.analysis == 'Treatment'  | input.analysis == 'OVSC'",
     radioGroupButtons(
       inputId = "contrast_time",
       label = "Time:",
       choices = c("early","late"),
       size = "normal",
       direction =  "horizontal",
       selected=NULL,
       justified = TRUE,
       checkIcon = list(
         yes = tags$i(class = "fa fa-check-square",
                      style = "color: steelblue"),
         no = tags$i(class = "fa fa-square-o",
                     style = "color: steelblue")
       )
     )),
   hr(),
   fluidRow(width=12,
            align = "center",
          
            #box(  
            #actionButton('load_csv', 'Load Go enriment',
             #            style='padding:8px; font-size:150%'),
            #title = "TABLE TEST",
            #tableOutput("table.output") 
            textOutput("selected_var")
            # )
   )

                
  
  
  ),
  # !important;
  #show{ z-index: 10000000000 !important;
  dashboardBody(
    tags$head(
    tags$style(HTML(".modal-dialog {width:  fit-content ;}
                              
                              #help_modal .modal-dialog{width: 60% !important;}
                              
                              #info_txt{color: black;font-size: 16px;}
                              
                              #show{position: absolute;}
                              
                              .dataTables_scrollBody {
                                 transform:rotateX(180deg);
                              }
                              
                              .dataTables_scrollBody table {
                                    transform:rotateX(180deg);
                              }"
                              )),
    tags$head(
      tags$script(HTML(js))
    )
    ),
    
    
    tabItems(
      tabItem(tabName = "enrichment",

              box(width = NULL,
                  status="primary",
                  actionBttn(
                    inputId = "show",
                    label = NULL,
                    style = "pill",
                    color = "default",
                    icon = icon("question-circle")
                  ),
                  
                  h2(textOutput("condition"),align="center"),
                # splitLayout(
                #   imageOutput("home_img"),
                # ),
                h1(textOutput("contrast"),align="center"),
                # radioButtons(
                #   inputId = "parameter",
                #   label = "",
                #   choices = c("FDR", 
                #               "pval"),
                #   inline=TRUE
                # ),
              ),
      
              # splitLayout(   
              #   verbatimTextOutput('plot_over_selected'),
              #   verbatimTextOutput('plot_under_selected')
              # ),
              
              #### HIDE VOLCANO###
              conditionalPanel(
                condition =  "input.TSD == 'Hide'",
                conditionalPanel(
                  condition =  "true" ,#input.enrichment_switch == true",
                  splitLayout(
                    plotOutput('plot_over'),
                    plotOutput('plot_under')
                  )),
                splitLayout(
                  conditionalPanel(
                    condition = "true", #"input.table_switch == true",
                    splitLayout(
                      dataTableOutput('dt'),
                      dataTableOutput('dt_1'),
                    ))
                )
              ),
              
    
              
              ###### SHOW VOLCANO ####
              conditionalPanel(
                
                condition =  "input.TSD == 'Show'",
                
                
                
                fluidRow(
                  column(
                    8,
                    box(width = 12,
                        title="Volcano plot:",
                        plotOutput('plot_volcano')
                    )),
                  column(
                    4,box(width = 12,
                          title = "Values selection:", status = "primary", solidHeader = TRUE,
                          collapsible = TRUE,
                          sliderInput("logfc", "Log Fold-Change:",
                                      min = 0, max = 10,step = 0.1,
                                      value = 2),
                          
                          # sliderTextInput(inputId = "pval", 
                          #                 label = "P-value:", 
                          #                 choices = c(0.001,0.005,0.01,0.05)
                          #                 ),
                          # sliderInput("pval", "P-value:",
                          #             min = 0, max = 1,step = 0.01,
                          #             value = 0.5)),
                          sliderTextInput("pval", "P-value:",
                                          choices = c(0.001,0.005,0.01,0.05),
                                          selected = 0.05,
                                          grid = TRUE
                                )
                          ),
                    fluidRow(
                      column(6,
                             valueBoxOutput('valuebox_over',width = 12)
                      ),
                      column(6,
                             valueBoxOutput('valuebox_under',width = 12)
                      )
                    )
                    # fluidRow( 
                    #   column(3),
                    #   column(1, 
                    #          switchInput(
                    #     size = "mini",
                    #     inputId = "showcondition",
                    #     onLabel = "Hide",
                    #     offLabel = "Show",
                    #     inline = TRUE
                    #   ),
                    #          ),
                    #   column(3)
                    #  
                    # )
                    
                  ),
                  
                  
                  fluidRow(
                    
                    
                    column(6,box(status="danger",#background = "red", 
                                 width = 12,
                                 title = "Over:",
                                 dataTableOutput('genes_over')
                    )),
                    column(6,box(status="primary",#background = "blue", 
                                 width = 12,
                                 title = "Under:",
                                 dataTableOutput('genes_under')
                    ))
                    
                  )
                  
                ),
                
                
                
                ################### NO VOLCANO
                # fluidRow(
                #   
                #   # column(
                #   #   8,
                #   #   box(width = 12,
                #   #       title="Volcano plot:",
                #   #       plotOutput('plot_volcano')
                #   #   )),
                #   # column(
                #   #   4,box(width = 12,
                #   #         title = "Values selection:", status = "primary", solidHeader = TRUE,
                #   #         collapsible = TRUE,
                #   #         sliderInput("logfc", "Log Fold-Change:",
                #   #                     min = 0, max = 10,step = 0.01,
                #   #                     value = 0.5),
                #   #         sliderInput("pval", "P-value:",
                #   #                     min = 0, max = 1,step = 0.01,
                #   #                     value = 0.5)),
                #   #   fluidRow(
                #   #     column(6,
                #   #            valueBoxOutput('valuebox_over',width = 12)
                #   #     ),
                #   #     column(6,
                #   #            valueBoxOutput('valuebox_under',width = 12)
                #   #     )
                #   #     
                #   #     
                #   #   )
                #   # ),
                #   
                #   splitLayout(
                #     dataTableOutput('targeted_over'),
                #     dataTableOutput('targeted_under'),
                #   )
                #   
                #   # fluidRow(
                #   #   column(6,box(status="danger",#background = "red", 
                #   #                width = 12,
                #   #                title = "Over:",
                #   #                dataTableOutput('targeted_over')
                #   #   )),
                #   #   column(6,box(status="primary",#background = "blue", 
                #   #                width = 12,
                #   #                title = "Under:",
                #   #                dataTableOutput('targeted_under')
                #   #   ))
                #   #   
                #   # )
                #   
                # )
                
                # box(plotOutput('plot_volcano')),
                # box(
                #   sliderInput("logfc", "Log Fold-Change:",
                #                 min = 0, max = 10,
                #                 value = 0.5),
                #   sliderInput("pval", "P-value:",
                #               min = 0, max = 1,
                #               value = 0.05),
                # 
                # )
                
                # splitLayout(        
                #   plotOutput('plot_volcano')
                #   #,
                #   #dataTableOutput('display')
                #   
                # )
                
              ),
              ####### END VOLCANO
              
              
              
      ),
      
      tabItem(tabName = "information",
              
              fluidRow(
                
                column(8,
                       box(
                         width=NULL,
                       h2("INFORMATION"),
                       hr(),
                       htmlOutput("info_txt")),
                       
                       box(
                         width=NULL,
                         h2("Experiment design"),
                         hr(),
                         #img(src='GO_picture.png',height="80%", width="80%", align="center"),
                         tags$img(src="experiment.png", 
                                  title="Experiment design", 
                                  width="100%",
                                  height="100%"),
                         p("Author: Javier Guerrero")
                         # tags$a(
                         #   href="http://geneontology.org/docs/ontology-documentation//", 
                         #   tags$img(src="experiment.png", 
                         #            title="Gene Ontology", 
                         #            width="90%",
                         #            height="90%"),
                         #   target="_blank"
                         # )
                       ),
                       box(
                         width=NULL,
                         h2("Experiment samples"),
                         hr(),
                         #img(src='GO_picture.png',height="80%", width="80%", align="center"),
                         tags$img(src="OphiKT_RNAseq_samples.svg", 
                                  title="Experiment samples", 
                                  width="100%",
                                  height="100%"),
                         p("Author: Javier Guerrero")
                         # tags$a(
                         #   href="http://geneontology.org/docs/ontology-documentation//", 
                         #   tags$img(src="experiment.png", 
                         #            title="Gene Ontology", 
                         #            width="90%",
                         #            height="90%"),
                         #   target="_blank"
                         # )
                       ),
                       ),
                column(4,
                       box(
                         width=NULL,
                       #img(src='GO_picture.png',height="80%", width="80%", align="center"),
                       tags$a(
                         href="http://geneontology.org/docs/ontology-documentation//", 
                         tags$img(src="GO_picture.png", 
                                  title="Gene Ontology", 
                                  width="90%",
                                  height="90%"),
                         target="_blank"
                       )
                       ),
                     
                       box(
                         width=NULL,
                         h4("Bibliography"),
                         htmlOutput("bibliography")
                         
                       ),
                       box(
                         width=NULL,
                         h4("Useful links"),
                         htmlOutput("links")
                         
                       )
                      
                )
          
              )
         
      ),
      tabItem(tabName = "search_gene",
            
            fluidRow(
              column(6,
                     box(
                       width = NULL,
                       title = "Gene selection:",
                       status = "primary", 
                       solidHeader = TRUE,
                       fluidRow(
                         column(6,
                                conditionalPanel(
                                  condition = "input.organism_search == 'KT'",
                                  pickerInput(
                                    inputId = "KT_selected_gene",
                                    label = "Select gene to display:",
                                    choices = KT_genes,
                                    options = list(
                                      `actions-box` =TRUE,
                                      `live-search` = TRUE
                                    ),
                                    multiple = FALSE
                                  )
                                  
                                ),
                                conditionalPanel(
                                  condition = "input.organism_search == 'Ophiostoma'",
                                  pickerInput(
                                    inputId = "Ophiostoma_selected_gene",
                                    label = "Select gene to display:",
                                    choices = Ophiostoma_genes,
                                    options = list(
                                      `actions-box` =TRUE,
                                      `live-search` = TRUE
                                    ),
                                    multiple = FALSE
                                  ) 
                                )
                         ),
                         column(6,
                                radioGroupButtons(
                                  inputId = "organism_search",
                                  label = "Reference genome (Organism):",
                                  choices = c("Ophiostoma","KT"),
                                  justified = TRUE,
                                  size="lg",
                                  checkIcon = list(
                                    yes = tags$i(class = "fa fa-check-square", 
                                                 style = "color: steelblue"),
                                    no = tags$i(class = "fa fa-square-o", 
                                                style = "color: steelblue")
                                  )
                                )
                         )
                         
                         
                         
                       )
                       
                     ) 
                     
                     ),
              column(6,
                     box(width = NULL,
                         title = "Values selection:",
                         status = "primary", 
                         solidHeader = TRUE,
                         # collapsible = TRUE,
                         # collapsed = T,
                         fluidRow(
                           column(6,
                                  sliderInput("logfc_search", "Log Fold-Change:",
                                              min = 0, max = 10,step = 0.1,
                                              value = 2)),
                           column(6,
                                  sliderTextInput("pval_search", "P-value:",
                                                  choices = c(0.001,0.005,0.01,0.05,1),
                                                  selected = 0.05,
                                                  grid = TRUE
                                  ))
                         )
                         
                  
                     )
                     
                     )
            ),
            
            
            box(width = NULL,
                title = "Gene information:",
                status = "primary", 
                solidHeader = TRUE,
                collapsible = TRUE,
                collapsed = T,
                dataTableOutput('selected_table_geneinfo')),
             
             
             box(status="primary",
                 width = NULL,
                 title = "Selected gene barplot:",
                 plotOutput('selected_plot')),
             
             box(status="primary",
                 width = NULL,
                 title = "Selected gene:",
                 dataTableOutput('selected_table'))
             
      ),
      
########################## SEARCH GO CATEGORY
      tabItem(tabName = "search_category",
              
              fluidRow(
                column(6,
                       box(
                         width = NULL,
                         title = "Category selection:",
                         status = "primary", 
                         solidHeader = TRUE,
                         fluidRow(
                           column(6,
                                  conditionalPanel(
                                    condition = "input.organism_search_category == 'KT'",
                                    pickerInput(
                                      inputId = "KT_selected_category",
                                      label = "Select gene to display:",
                                      choices = KT_GO_categories_id,
                                      choicesOpt = list(
                                        subtext = KT_GO_categories_term),
                                      options = list(
                                        `actions-box` =TRUE,
                                        `live-search` = TRUE
                                      ),
                                      multiple = FALSE
                                    )
                                    
                                  ),
                                  conditionalPanel(
                                    condition = "input.organism_search_category == 'Ophiostoma'",
                                    pickerInput(
                                      inputId = "Ophiostoma_selected_category",
                                      label = "Select gene to display:",
                                      choices = Ophiostoma_GO_categories_id ,
                                      choicesOpt = list(
                                        subtext = Ophiostoma_GO_categories_term),
                                      options = list(
                                        `actions-box` =TRUE,
                                        `live-search` = TRUE
                                      ),
                                      multiple = FALSE
                                    ) 
                                  )
                           ),
                           column(6,
                                  radioGroupButtons(
                                    inputId = "organism_search_category",
                                    label = "Reference genome (Organism):",
                                    choices = c("Ophiostoma","KT"),
                                    justified = TRUE,
                                    size="lg",
                                    checkIcon = list(
                                      yes = tags$i(class = "fa fa-check-square", 
                                                   style = "color: steelblue"),
                                      no = tags$i(class = "fa fa-square-o", 
                                                  style = "color: steelblue")
                                    )
                                  )
                           )
                           
                           
                           
                         )
                         
                       ) 
                       
                ),
                column(6,
                       box(width = NULL,
                           title = "Values selection:",
                           status = "primary", 
                           solidHeader = TRUE,
                           # collapsible = TRUE,
                           # collapsed = T,
                           radioGroupButtons(
                             inputId = "parameter_category",
                             label = "Parameter to display in barplot:",
                             choices = c(
                               "Gene Ratio" = "gene_ratio",
                               "Nº of Genes expressed" = "Significant",
                               "-log(FDR)" = "FDR",
                               "-log(P-Value)" = "pval"),
                             justified = TRUE,
                             selected = "gene_ratio",
                             size="normal",
                             checkIcon = list(
                               yes = tags$i(class = "fa fa-check-square", 
                                            style = "color: black"),
                               no = tags$i(class = "fa fa-square-o", 
                                           style = "color: black")
                             )
                           )
                           
                           
                           # fluidRow(
                           #   column(6,
                           #          sliderInput("logfc_search_category", "Log Fold-Change:",
                           #                      min = 0, max = 10,step = 0.1,
                           #                      value = 2)),
                           #   column(6,
                           #          sliderTextInput("pval_search_category", "P-value:",
                           #                          choices = c(0.001,0.005,0.01,0.05,1),
                           #                          selected = 0.05,
                           #                          grid = TRUE
                           #          ))
                           # )
                           
                           
                       )
                       
                )
              ),
              
              
              # box(width = NULL,
              #     title = "Category information:",
              #     status = "primary", 
              #     solidHeader = TRUE,
              #     collapsible = TRUE,
              #     collapsed = T,
              #     dataTableOutput('selected_table_categoryinfo')),
              
              
              box(status="primary",
                  width = NULL,
                  title = "Selected category barplot:",
                  plotOutput('selected_plot_category')),
              
              box(status="primary",
                  width = NULL,
                  title = "Genes in selected category:",
                  dataTableOutput('selected_table_category'))
              
      )
      
      
    ),
#

    
  ),

)



server <- function(input, output) {
 
  output$info_txt  <- renderUI({
    fileName <- 'data/GO_help_text.txt'
    HTML(readChar(fileName, file.info(fileName)$size))
  })
  
  output$bibliography  <- renderUI({
    fileName <- 'data/bibliography.txt'
    HTML(readChar(fileName, file.info(fileName)$size))
  })
  
  output$links  <- renderUI({
    fileName <- 'data/links.txt'
    HTML(readChar(fileName, file.info(fileName)$size))
  })

  vals <- reactiveValues()
  tables <- reactiveValues()
  
  observe({
    print("PESTAÑA SELECCIONADA")
    print(input[["activeTab"]])
    if(is.null(input[["activeTab"]])){
      vals$active_tab <- ""
    }else{
      vals$active_tab <- input[["activeTab"]]
    }
  })


  observe({
    vals$organism_search <- input$organism_search
    vals$Ophiostoma_selected_gene <- input$Ophiostoma_selected_gene
    vals$KT_selected_gene <- input$KT_selected_gene
    
    vals$logfc_search <- input$logfc_search
    vals$pval_search <- input$pval_search
    
    if(vals$organism_search=="Ophiostoma"){
    ophiostoma_all_genes <- read.csv("data/diff_expressed_genes_tsv/Ophiostoma_all_merged.csv")
    ophi_selected <- grep(vals$Ophiostoma_selected_gene,ophiostoma_all_genes$id)
    selected_table <- ophiostoma_all_genes[ophi_selected,]
    selected_table[,1] <- selected_table$file_data
    selected_table$file_data <- NULL
    colnames(selected_table)[1] <- "conditions"
    print(str(selected_table))
    }else{
    kt_all_genes <- read.csv("data/diff_expressed_genes_tsv/KT_all_merged.csv")
    kt_selected <- grep(vals$KT_selected_gene,kt_all_genes$id)
    selected_table <- kt_all_genes[kt_selected,]
    selected_table[,1] <- selected_table$file_data
    selected_table$file_data <- NULL
    colnames(selected_table)[1] <- "conditions"
    }
    
    # print("## SELECTED TABLE STR")
    # print(str(selected_table))
    
    selected_table_geneinfo <- subset(selected_table, select = -c(conditions,log2FoldChange,padj))
    selected_table_geneinfo <- unique(selected_table_geneinfo)
    tables$selected_table_geneinfo <- selected_table_geneinfo
    
    # print(selected_table_geneinfo)
    
    selected_table[c("contrast","conditions")] <- str_split_fixed(selected_table$conditions,"_",2)
   # selected_table$conditions <-  gsub('_','\n',selected_table$conditions)
    selected_table$conditions <-  gsub('\\.','\n',selected_table$conditions)
    selected_table$conditions <-  gsub('organism','org',selected_table$conditions)
    selected_table$conditions <-  gsub('consortium','cons',selected_table$conditions)
    
    
    selected_table$contrast <- ifelse(selected_table$contrast=="Medium","GPP vs BWP",
                                      ifelse(selected_table$contrast=="Treatment","FaOH vs CTL",
                                             ifelse(selected_table$contrast=="Time","Early vs Late","Org vs Cons")))
    selected_table$overexpressed <- ifelse(selected_table$log2FoldChange>=0,
                                           str_split_fixed(selected_table$contrast," vs ",2)[,1],
                                           str_split_fixed(selected_table$contrast," vs ",2)[,2])
    

    selected_table <- selected_table %>% filter(padj < vals$pval_search)
    #selected_table$color <- which(selected_table$lo)
    tables$selected_table <- selected_table
    
  
    
    ## PLOT
 
    # create graph
    error_plot <- data.frame(message = "NO INFO AVAILABLE",
                     message_pval = paste0("p-value = ",vals$pval_search))
    
    if(nrow(selected_table)==0){
      plots$selected_plot <-  ggplot(error_plot, aes(x = 1, y = 1, label = message)) + 
        geom_text(size = 10, color = "black", vjust = 0.5, hjust = 0.5) +
        theme_void()
    }else{
    plots$selected_plot <- ggplot(data = selected_table, aes(x = conditions, y = log2FoldChange)) +
      geom_bar(stat = 'identity', aes(fill = log2FoldChange)) +
      geom_text(aes(
                 label = round(log2FoldChange,2),
                 x=conditions,
                 y=log2FoldChange+ifelse(log2FoldChange>=0,-0.5, 0.5),
                 fontface=2
                ),
                vjust=1,
                colour = "black")+
      geom_text(aes(
        label = overexpressed,
        x=conditions,
        y=log2FoldChange+ifelse(log2FoldChange>=0,-0.5, 0.5),
        fontface=2
      ),
      vjust=-1.2,
      colour = "black")+
      geom_hline(yintercept=vals$logfc_search, linetype="dashed", color = "gray70") +
      geom_hline(yintercept=-vals$logfc_search, linetype="dashed", color = "gray70") +
      scale_fill_gradient2(low='blue', mid='snow', high='red') +
      theme_bw()+
      theme(
        strip.background = element_rect(fill = "white"),
        strip.text=element_text(size=14,face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
      facet_grid(~contrast, scales = "free_x",space="free_x")


    }
  })
  
  output$selected_plot <- renderPlot({plots$selected_plot})
  
  
    output$selected_table <- renderDataTable({
      req(data())
      datatable(
        tables$selected_table,
        # data_overexpressed(),
        escape=FALSE,
        rownames=FALSE,
        selection = "single",
        extensions = 'Buttons',
        options = list(dom = 'Bfrtip', 
                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                       pageLength = 100)
      )
      #%>%
      #   formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
      #   formatStyle(
      #     'log2FoldChange',
      #     backgroundColor = styleInterval(c(-20,20), clrs)
      #   )
      # 
      
      
    })
    
    output$selected_table_geneinfo <- renderDataTable({
      req(data())
      datatable(
        tables$selected_table_geneinfo,
        escape=FALSE,
        rownames=FALSE,
        options = list(dom = 't')
      )
      
    })
  
    
  ######################## SEARCH CATEGORY START
    
    observe({if(vals$active_tab == "search_category"){
      
      vals$organism_search_category <- input$organism_search_category
      vals$Ophiostoma_selected_category <- input$Ophiostoma_selected_category
      vals$KT_selected_category <-  input$KT_selected_category
      
     
      vals$parameter_category <- input$parameter_category
      print( vals$parameter_category)
      KT_GO_categories <- read.csv(paste0("data/Results/GO_enrichment_KT.tsv"),sep = "\t", header = T)
      KT_GO_categories_id<- unique(KT_GO_categories$GO.ID)
      
      Ophiostoma_GO_categories<- read.csv(paste0("data/Results/GO_enrichment_Ophiostoma.tsv"),sep = "\t", header = T)
      Ophiostoma_GO_categories_id <- unique(Ophiostoma_GO_categories$GO.ID)
      
      if(vals$organism_search_category=="Ophiostoma"){
        ophi_selected <- grep(vals$Ophiostoma_selected_category,Ophiostoma_GO_categories$GO.ID)
        selected_table <- Ophiostoma_GO_categories[ophi_selected,]
        # selected_table[,1] <- selected_table$file_data
        # selected_table$file_data <- NULL
        # colnames(selected_table)[1] <- "conditions"
      }else{
        kt_selected <- grep(vals$KT_selected_category,KT_GO_categories$GO.ID)
        selected_table <- KT_GO_categories[kt_selected,]
      }
      
      selected_table <- unique(selected_table)
      selected_table$DE <- gsub("underexpressed","down",selected_table$DE)
      selected_table$DE <- gsub("overexpressed","up",selected_table$DE)
      
      selected_table$conditions <- paste0(
        selected_table$condition_1,"\n",
        selected_table$condition_2,"\n",
        selected_table$condition_3,"\n",
        selected_table$DE
      )
      selected_table <- subset(selected_table, selected_table$Significant != 0)       
      
      selected_table$gene_ratio <- selected_table$Significant / selected_table$Annotated
     
      # tryCatch({
      #   selected_table[selected_table$DE == "down",] <- selected_table[,"gene_ratio"]*-1 
      # }, error = function(err) {
      #     print("ERROR")        }
      # )
      selected_table <- selected_table %>% filter(pval <= 0.05 )
      selected_table$FDR <- -log10( selected_table$FDR)
      selected_table$pval <- -log10( selected_table$pval)
      
      parameter_category <- vals$parameter_category
      parameter_category_column <- selected_table[parameter_category]
    
      if(parameter_category == "gene_ratio"){
        selected_table[parameter_category] <- ifelse(selected_table$DE == "down", -1 * selected_table$gene_ratio, selected_table$gene_ratio)
      }
      if(parameter_category == "Significant"){
        selected_table[parameter_category] <- ifelse(selected_table$DE == "down", -1 * selected_table$Significant, selected_table$Significant)
      }
      if(parameter_category == "FDR"){
        selected_table[parameter_category] <- ifelse(selected_table$DE == "down", -1 * selected_table$FDR, selected_table$FDR)
      }
      if(parameter_category == "pval"){
        selected_table[parameter_category] <- ifelse(selected_table$DE == "down", -1 * selected_table$pval, selected_table$pval)
      }
     
      # print("## SELECTED TABLE STR")
      # selected_table <- selected_table[order(selected_table$condition_3,decreasing = T),]
      # print(str(selected_table))
      # print(unique(selected_table$condition_3))
      # print(unique(selected_table$conditions))
      # 
      # # print(parameter_category)
      # # print(str( selected_table$parameter_category))
      # # print(selected_table[parameter_category])
      # # print(parameter_category_column)
      # print("##########################")
      
      # colnames(selected_table) <- gsub("FDR","-log(FDR)",colnames(selected_table))
      # colnames(selected_table) <- gsub("pval","-log(pval)",colnames(selected_table))
      # 
      # 
      selected_table_category <- subset(selected_table, select = -c(condition_1, condition_2,condition_3,DE))
      colnames(selected_table_category) <- gsub("FDR","-log(FDR)",colnames(selected_table_category))
      colnames(selected_table_category) <- gsub("pval","-log(pval)",colnames(selected_table_category))
      tables$selected_table_category <- selected_table_category
#       selected_table_geneinfo <- subset(selected_table, select = -c(conditions,log2FoldChange,padj))
#       selected_table_geneinfo <- unique(selected_table_geneinfo)
#       tables$selected_table_geneinfo <- selected_table_geneinfo
# 
#       # print(selected_table_geneinfo)
# 
#       selected_table[c("contrast","conditions")] <- str_split_fixed(selected_table$conditions,"_",2)
#       # selected_table$conditions <-  gsub('_','\n',selected_table$conditions)
#       selected_table$conditions <-  gsub('\\.','\n',selected_table$conditions)
#       selected_table$conditions <-  gsub('organism','org',selected_table$conditions)
#       selected_table$conditions <-  gsub('consortium','cons',selected_table$conditions)
# 
# 
      selected_table$file <- ifelse(selected_table$file=="Medium","GPP vs BWP",
                                        ifelse(selected_table$file=="Treatment","FaOH vs CTL",
                                               ifelse(selected_table$file=="Time","Early vs Late","Org vs Cons")))
#       selected_table$overexpressed <- ifelse(selected_table$log2FoldChange>=0,
#                                              str_split_fixed(selected_table$contrast," vs ",2)[,1],
#                                              str_split_fixed(selected_table$contrast," vs ",2)[,2])
# 
# 
#       selected_table <- selected_table %>% filter(padj < vals$pval_search)
#       #selected_table$color <- which(selected_table$lo)
#       tables$selected_table <- selected_table
      
      selected_table$overexpressed <- ifelse(selected_table$DE=="up",
                                             str_split_fixed(selected_table$file," vs ",2)[,1],
                                             str_split_fixed(selected_table$file," vs ",2)[,2])

      parameter_text <-  ifelse(vals$parameter_category=="gene_ratio","Gene ratio",
                                  ifelse(vals$parameter_category=="Significant","Nº of genes",
                                     ifelse(vals$parameter_category=="FDR","-log10(FDR)","-log10(pval)")))

      ## PLOT

      # create graph
      error_plot <- data.frame(message = "NO INFO AVAILABLE",
                               message_pval = paste0("p-value = ",vals$pval_search))

      if(nrow(selected_table)==0){
        plots$selected_plot <-  ggplot(error_plot, aes(x = 1, y = 1, label = message)) +
          geom_text(size = 10, color = "black", vjust = 0.5, hjust = 0.5) +
          theme_void()
      }else{
        plots$selected_plot_category <- ggplot(data = selected_table, aes(x = conditions, y = get(vals$parameter_category))) +
          geom_bar(stat = 'identity', aes(fill = get(vals$parameter_category) ) )+
          ylab(parameter_text) +
          guides(fill=guide_legend(title=parameter_text))+
          # geom_text(aes(          ylab=paste0(vals$parameter_category)+
          #   label = round(get(vals$parameter_category),2),
          #   x=conditions,
          #   y=get(vals$parameter_category)+ifelse(get(vals$parameter_category)>=0,-0.5, 0.5),
          #   fontface=2
          # ),
          # vjust=1,
          # colour = "black")+
          
          geom_text(aes(
            label = overexpressed,
            x=conditions,
            y=get(vals$parameter_category)+ifelse(get(vals$parameter_category)>=0,-(0.1*get(vals$parameter_category)), 0.1*get(vals$parameter_category)),
            fontface=2
          ),
          vjust=-1.2,
          colour = "black")+
        
          # # geom_hline(yintercept=vals$logfc_search, linetype="dashed", color = "gray70") +
          # geom_hline(yintercept=-vals$logfc_search, linetype="dashed", color = "gray70") +
          scale_fill_gradient2(low='blue', mid='snow', high='red') +
          theme_bw()+
          theme(
            strip.background = element_rect(fill = "white"),
            strip.text=element_text(size=14,face="bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))+
          facet_grid(~file, scales = "free_x",space="free_x")


      }
    }})
    
    output$selected_plot_category <- renderPlot({plots$selected_plot_category})
    output$selected_table_category <- renderDataTable({
      req(data())
      datatable(
        tables$selected_table_category,
        escape=FALSE,
        rownames=FALSE,
        options = list(dom = 't',
                       pageLength = 100)
      )
      
    })
  ######################## SEARCH CATEGORY END

  
  observe({
    vals$organism <- input$organism
    #if(input$analysis == "OVSC"){vals$culture<-""}
    if(input$consortium){vals$culture<-"consortium"}else{vals$culture<-""}
    vals$analysis <- input$analysis

    vals$parameter <- input$parameter
    

    vals$contrast_time <- input$contrast_time
    vals$contrast_treatment <- input$contrast_treatment
    vals$contrast_medium <- input$contrast_medium


    if(vals$analysis=="Medium"){ vals$contrast_medium <- "";vals$anal_contrast<-"GPP vs BWP"}
    if(vals$analysis=="Treatment"){ vals$contrast_treatment<- "";vals$anal_contrast<-"FaOH vs Control"}
    if(vals$analysis=="Time"){ vals$contrast_time <- "";vals$anal_contrast<-"early vs late"}
    if(vals$analysis=="OVSC"){ vals$contrast_OVSC<- "";vals$anal_contrast<-"Organism vs Consortium";vals$culture<-""}

    
    
    
    if(vals$analysis=="OVSC"){
      go_enrichment_df <- read.csv(paste0('data/Results/',vals$organism ,'_OVSC_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
     # print(paste0('data/Results/',vals$organism ,'_OVSC_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
    }else{
      go_enrichment_df <- read.csv(paste0('data/Results/',vals$organism ,'_',vals$culture,'_',vals$analysis,'_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
      #print(paste0('data/Results/',vals$organism ,'_',vals$culture,'_',vals$analysis,'_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
    }
    
   
    
    if(vals$analysis=="Medium"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_time}
    if(vals$analysis=="Treatment"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_time}
    if(vals$analysis=="Time"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_treatment}
    
    if(vals$analysis=="OVSC"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_medium;condition_3react=vals$contrast_time}
    
    # 
    ontology <- input$ontology
    # print("==================== ONTOLOGY =============")
    # print(ontology)
    
    go_enrichment_df <-  go_enrichment_df %>% filter(type %in% ontology)
    
    if(vals$analysis=="OVSC"){
      go_enrichment_df <- go_enrichment_df %>% filter(pval < input$pvalue &
                                                        condition_1 == condition_1react &
                                                        condition_2 == condition_2react &
                                                        condition_3 == condition_3react)
      
      vals$sample <- paste0(paste0(condition_1react,".",condition_2react,".",condition_3react))
      
    }else{
      # print(condition_1react)
      # print(condition_2react)
      
      go_enrichment_df <- go_enrichment_df %>% filter(pval < input$pvalue &
                                                        condition_1 == condition_1react &
                                                        condition_2 == condition_2react)
      # print(nrow(go_enrichment_df))
      vals$sample <- paste0(paste0(condition_1react,".",condition_2react))
    }
    
    #print("BEFORE SPLIT-...")
    #print(str(go_enrichment_df))
    order_index_pval <- order(go_enrichment_df$pval)
    go_enrichment_df <- go_enrichment_df[order_index_pval,]
   
   if(nrow(go_enrichment_df)==0){

   }else{
       go_enrichment_df[,1]<-sapply(go_enrichment_df[,1],function(x){
         paste0(
           '<a href="http://amigo.geneontology.org/amigo/term/',x,'" target="_blank">',x,'</a>'
         )
       })
   }
    
       
    table_overexpressed <- go_enrichment_df %>% filter(DE=="overexpressed")
    table_underexpressed <- go_enrichment_df %>% filter(DE=="underexpressed")
    
   # print("INITIAL TABLE OVER")
  #  print(str(table_overexpressed))
    # 
     parameter <- vals$parameter
    # 
    table_overexpressed <- table_overexpressed %>% filter(get(parameter)<=input$pvalue)
    table_underexpressed <- table_underexpressed %>% filter(get(parameter)<=input$pvalue)
    
      

    #    
    table_overexpressed$GeneRatio <- table_overexpressed$Significant/table_overexpressed$Annotated
    table_underexpressed$GeneRatio <- table_underexpressed$Significant/table_underexpressed$Annotated       
    
    tables$table_overexpressed <- table_overexpressed
    tables$table_underexpressed <- table_underexpressed
    
    table_overexpressed <- table_overexpressed %>% arrange(desc(GeneRatio))
    table_underexpressed <- table_underexpressed %>% arrange(desc(GeneRatio))
    
    #OVER
    terminos <- table_overexpressed$Term
    terminos <- stringr::str_trunc(terminos, 70)
    table_overexpressed$Term_short <- terminos
    
    # para que quepa todo bien en el grafico  
    threshold <- 25
    if(nrow(table_overexpressed)>threshold){
      table_overexpressed <- table_overexpressed[1:(threshold+1),]
      table_overexpressed[threshold+1,]$Term_short <- "--MORE FUNCTIONS IN THE TABLE--"
      #table_overexpressed[threshold+1,]$pval <- 0.1
    }
    #UNDER
    
    terminos <- table_underexpressed$Term
    terminos <- stringr::str_trunc(terminos, 70)
    table_underexpressed$Term_short <- terminos
    
    # para que quepa todo bien en el grafico  
    if(nrow(table_underexpressed)>threshold){
      table_underexpressed <- table_underexpressed[1:(threshold+1),]
      table_underexpressed[threshold+1,]$Term_short <- "--MORE FUNCTIONS IN THE TABLE--"
      #table_underexpressed[threshold+1,]$pval <- 0.1
    }
    
   
    # print(nrow(table_overexpressed))
    # print(nrow(table_underexpressed))
    # print(table_overexpressed)
    # print(table_underexpressed)

  
    #print(str(table_overexpressed))
   # print(max(table_underexpressed[parameter]))
  #  print(max(table_overexpressed[parameter]))
    
   # print(str(table_underexpressed))
  #  print(str(table_overexpressed))
    
    
    gradient_max <- max(c(max(table_underexpressed[parameter],na.rm = TRUE),max(table_overexpressed[parameter],na.rm = TRUE)))
    
    plots$go_plot_over  <- ggplot(table_overexpressed, aes(x = GeneRatio, y = forcats::fct_reorder(Term_short, GeneRatio))) +
      geom_point(aes(size = Significant, color = get(  parameter),shape=type)) +
      theme_bw(base_size = 14) +
      scale_colour_gradient(name=  parameter,limits=c(0, gradient_max), low="red") +
      ylab(NULL) 
      #guides(color=guide_legend(title=parameter))
    
      #ggtitle("GO pathway enrichment")
    
    plots$go_plot_under  <- ggplot(table_underexpressed, aes(x = GeneRatio, y = forcats::fct_reorder(Term_short, GeneRatio))) +
      geom_point(aes(size = Significant, color = get(  parameter),shape=type)) +
      theme_bw(base_size = 14) +
      scale_colour_gradient(name=  parameter,limits=c(0, gradient_max), low="red") +
      ylab(NULL) 
      #ggtitle("GO pathway enrichment")
    # plots$go_plot_over <- ggplot(data=table_overexpressed, aes(x=reorder(Term_short, -pval), y=-log10(pval), fill=type)) + 
    #   scale_fill_manual(values = set_col, breaks = c("MF","BP","CC")) +
    #   #ylim(0,0.01) + 
    #   geom_bar(stat="identity", width=0.5) + 
    #   coord_flip() + 
    #   theme_bw() + 
    #   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #         axis.title.y=element_blank(),axis.text=element_text(size=10))
    # 
    # plots$go_plot_under <- ggplot(data=table_underexpressed, aes(x=reorder(Term_short, -pval), y=-log10(pval), fill=type)) + 
    #   scale_fill_manual(values = set_col, breaks = c("MF","BP","CC")) +
    #   #ylim(0,0.01) + 
    #   geom_bar(stat="identity", width=0.5) + 
    #   coord_flip() + 
    #   theme_bw() + 
    #   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
    #         axis.title.y=element_blank(),axis.text=element_text(size=10))
    
   

    })
  
  
  
  ## CON EL OBSERVE EVENT
#   observeEvent(input$organism,{
#     vals$organism <- input$organism
#     })
#   
#   observeEvent(input$consortium,{
#     if(input$consortium){vals$culture<-"consortium"}else{vals$culture<-""}
#   })
#     
#   observeEvent(input$analysis,{
#     vals$analysis <- input$analysis
#   })
#   
#   observeEvent(input$contrast_time,{
#     vals$contrast_time <- input$contrast_time
#   }) 
#   observeEvent(input$contrast_treatment,{
#     vals$contrast_treatment <- input$contrast_treatment
#   }) 
#   observeEvent(input$contrast_medium,{
#     vals$contrast_medium <- input$contrast_medium
#   }) 
#     
# 
# observe({
#   
#   if(vals$analysis=="Medium"){ vals$contrast_medium <- "";vals$anal_contrast<-"GPP vs BWP"}
#   if(vals$analysis=="Treatment"){ vals$contrast_treatment<- "";vals$anal_contrast<-"FaOH vs Control"}
#   if(vals$analysis=="Time"){ vals$contrast_time <- "";vals$anal_contrast<-"early vs late"}
#   if(vals$analysis=="OVSC"){ vals$contrast_OVSC<- "";vals$anal_contrast<-"Organism vs Consortium";vals$culture<-""}
#   
#   
# })


  
  
  output$condition  <- renderText({
    #if(input$consortium){culture<-"consortium"}else{culture<-""}
    paste0(vals$organism ,' ',vals$culture,' ',#vals$analysis,' ',
           vals$contrast_time,' ',
           vals$contrast_treatment,' ',
           vals$contrast_medium)
    
  })
  
  output$contrast <- renderText({
    paste0(vals$anal_contrast)
  })
  
  
  # output$condition <- renderUI({
  #   str1 <- paste("You have selected", input$var)
  #   str2 <- paste("You have chosen a range that goes from",
  #                 input$range[1], "to", input$range[2])
  #   HTML(paste(str1, str2, sep = '<br/>'))
  #   
  # })
  ### TABLES
  
  
  plots <- reactiveValues()
  
  
  ### READ datos del enrichment
  #observeEvent(input$analysis,{
  # observe({
  #   if(vals$analysis=="OVSC"){
  #     go_enrichment_df <- read.csv(paste0('data/Results/',vals$organism ,'_OVSC_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
  #   }else{
  #     go_enrichment_df <- read.csv(paste0('data/Results/',vals$organism ,'_',vals$culture,'_',vals$analysis,'_LFC_2_pval/GO_enrichment/GOenrichment.txt'),sep="\t")
  #   }
  #   
  #   if(vals$analysis=="Medium"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_time}
  #   if(vals$analysis=="Treatment"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_time}
  #   if(vals$analysis=="Time"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_treatment}
  #   
  #   if(vals$analysis=="OVSC"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_medium;condition_3react=vals$contrast_time}
  #   
  #   # 
  #   if(vals$analysis=="OVSC"){
  #     go_enrichment_df <- go_enrichment_df %>% filter(pval < 0.01 &
  #                                                       condition_1 == condition_1react &
  #                                                       condition_2 == condition_2react &
  #                                                       condition_3 == condition_3react)
  #     
  #     vals$sample <- paste0(paste0(condition_1react,".",condition_2react,".",condition_3react))
  #     
  #   }else{
  #     go_enrichment_df <- go_enrichment_df %>% filter(pval < 0.01 &
  #                                                       condition_1 == condition_1react &
  #                                                       condition_2 == condition_2react)
  #     
  #     vals$sample <- paste0(paste0(condition_1react,".",condition_2react))
  #   }
  #   
  #   
  #   go_enrichment_df <- go_enrichment_df[order(go_enrichment_df["pval"]),]
  #   
  #   tables$table_overexpressed <- go_enrichment_df %>% filter(DE=="overexpressed")
  #   tables$table_underexpressed <- go_enrichment_df %>% filter(DE=="underexpressed")
  #   
  # 
  # 
  #   terminos <- go_enrichment_df$Term
  #   terminos <- stringr::str_trunc(terminos, 70)
  #   go_enrichment_df$Term_short <- terminos
  # 
  #         # para que quepa todo bien en el grafico  
  #     threshold <- 40
  #   if(nrow(go_enrichment_df)>threshold){
  #     go_enrichment_df <- go_enrichment_df[1:(threshold+1),]
  #     go_enrichment_df[threshold+1,]$Term_short <- "--MORE FUNCTIONS IN THE TABLE--"
  #     go_enrichment_df[threshold+1,]$pval <- 0.1
  #   }
  #   
  #   table_overexpressed <- go_enrichment_df %>% filter(DE=="overexpressed")
  #   table_underexpressed <- go_enrichment_df %>% filter(DE=="underexpressed")
  #   
  #   plots$go_plot_over <- ggplot(data=table_overexpressed, aes(x=reorder(Term_short, -pval), y=-log10(pval), fill=type)) + 
  #     scale_fill_manual(values = set_col, breaks = c("MF","BP","CC")) +
  #     #ylim(0,0.01) + 
  #     geom_bar(stat="identity", width=0.5) + 
  #     coord_flip() + 
  #     theme_bw() + 
  #     theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  #           axis.title.y=element_blank(),axis.text=element_text(size=10))
  #   
  #   plots$go_plot_under <- ggplot(data=table_underexpressed, aes(x=reorder(Term_short, -pval), y=-log10(pval), fill=type)) + 
  #     scale_fill_manual(values = set_col, breaks = c("MF","BP","CC")) +
  #     #ylim(0,0.01) + 
  #     geom_bar(stat="identity", width=0.5) + 
  #     coord_flip() + 
  #     theme_bw() + 
  #     theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  #           axis.title.y=element_blank(),axis.text=element_text(size=10))
  # 
  # })
  

  
  ## plot outputs
  output$plot_over <- renderPlot({plots$go_plot_over})
  output$plot_under <- renderPlot({plots$go_plot_under})
  
  
  
  # output$analysis <- reactive({input$analysis})
  output$analysis <- reactive({ vals$analysis })
  
  output$home_img <- renderImage({
    
    list(src = "www/BWP.PNG",
         width = "100",
         height = "100",
         style="display: block; margin-left: auto; margin-right: auto;"
         )
    
  }, deleteFile = F)
  
  # 
  # data_overexpressed <- eventReactive(input$load_csv, {
  #        tables$table_overexpressed
  # })
  # data_underexpressed <- eventReactive(input$load_csv, {
  #   tables$table_underexpressed
  # })
  # 
  output$selected_var <- renderText({ 
    #tables$table_overexpressed_genes
    #if(input$consortium){culture<-"consortium"}else{culture<-""}
    #paste0(as.character(tables$gene_list_under))
    paste0("© 2022 Javier Guerrero")
    #paste0(vals$organism ,'_',vals$culture,'_',vals$analysis,'_LFC_2_pval')
  })
  
  observe({
    parameter <- vals$parameter
    if( vals$parameter=="FDR"){
      vals$param_col <- 7
    }else{
      vals$param_col <- 6
    }
    
    
  })
 
  output$dt <- renderDataTable({
    req(data())
    datatable(
      select(tables$table_overexpressed,type,GO.ID,Term,GeneRatio,vals$parameter,Annotated,Significant),
      # data_overexpressed(),
      escape=FALSE,
      rownames=FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip', 
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 100)
    )%>%
      formatSignif(columns = c(vals$parameter,"GeneRatio"), digits = 3) %>%
      formatStyle(
        'type',
        backgroundColor = styleEqual(c("MF", "BP","CC"), c('#76b5c5', '#99d466','#eab676'))
      )
    
    
   
    })
  output$dt_1 <- renderDataTable({
    req(data())
    datatable(
      select(tables$table_underexpressed,type,GO.ID,Term,GeneRatio,vals$parameter,Annotated,Significant),
      #data_underexpressed(),
      escape=FALSE,
      rownames = FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip', 
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 100,
                     autoWidth = TRUE,scrollX=FALSE,
                     columnDefs = list(list(width = '20px', targets = 2 ))
                     )
    ) %>%
      formatSignif(columns = c( vals$parameter,"GeneRatio"), digits = 3) %>%
      formatStyle(
        'type',
        backgroundColor = styleEqual(c("MF", "BP","CC"), c('#76b5c5', '#99d466','#eab676'))
      )
   
    })
  
  
  output$plot_over_selected = renderPrint({
    s = input$dt_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(as.character(tables$table_overexpressed_GO.ID),sep = ', ')
    }
  })
  
  output$plot_under_selected = renderPrint({
    s = input$dt_1_rows_selected
    if (length(s)) {
      cat('These rows were selected:\n\n')
      cat(as.character(tables$table_underexpressed_GO.ID), sep = ', ')
    }
  })

  # CREATE TABLE WITH GENES
  observe({
    #"/adg_",organism,"_",culture,"_",analysis,"_",sample,".tsv"
    
    #"/adg_",organism,"_",culture,"_OVSC_",sample,".tsv")
    pval <- input$pval
    logfc <- input$logfc
    
    ophi_best_hits <- read.csv(paste0("data/Parse_JGI/best_hits_data.csv"), header = F)
    colnames(ophi_best_hits) <- c("id","blasp_besthit")
    
    ophi_domains_info <- read.csv(paste0("data/ophi_domains_annotations/domain_info_ophi_html.tsv"), header = T,sep="\t")
    
    genes <- read.csv(paste0("data/diff_expressed_genes_tsv/adg_",vals$organism,"_",vals$culture,"_",vals$analysis,"_",vals$sample,".tsv"),sep = "\t", header = T)
   
    
    genes <- unique(genes)
    genes <- genes %>% filter(padj <pval)
    
    genes <- merge(x = genes, y = ophi_best_hits, by = "id", all.x = TRUE)
    genes <- merge(x = genes, y = ophi_domains_info, by = "id", all.x = TRUE)
    
    print("STR GENES: ")
    print(str(genes))
    print(str(ophi_best_hits))
    if(vals$organism=="Ophiostoma"){
      genes 
    }
    #genes <- arrange(genes,desc(log2FoldChange))
    
    genes_over <- genes %>% filter(log2FoldChange > logfc)
    genes_over <- arrange(genes_over,desc(log2FoldChange))
    
    genes_under <- genes %>% filter(log2FoldChange < -logfc)
    genes_under <- arrange(genes_under,log2FoldChange)
    
    genes_under_ori <- genes_under
    genes_over_ori  <- genes_over

    if(vals$organism=="Ophiostoma"){
      genes_over[,1]<-sapply(genes_over[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
      genes_under[,1]<-sapply(genes_under[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
    }

    if(vals$organism == "KT"){
      genes_over[,1]<-sapply(genes_over[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
      genes_under[,1]<-sapply(genes_under[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
    }
    
    if(nrow(genes_over)==0){genes_over <- genes_over_ori}
    if(nrow(genes_under)==0){genes_under <- genes_under_ori}

    if(vals$organism == "KT"){
      tables$genes_over <- select(genes_over,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)
      tables$genes_under <- select(genes_under,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)

    }
    if(vals$organism == "Ophiostoma"){
      tables$genes_over <- select(genes_over,id,	log2FoldChange,	blasp_besthit,domain_info,	GO_terms,	KEGG	,kogdefline,	InterPro)
      tables$genes_under <- select(genes_under,id,	log2FoldChange,blasp_besthit,	domain_info,	GO_terms,	KEGG	,kogdefline,	InterPro)
    }


    # genes_volcano <- select(genes,id,log2FoldChange,padj)
    
    # genes <- genes %>% filter(log2FoldChange < -logfc | log2FoldChange > logfc)
    genes_volcano <- select(genes,id,log2FoldChange,padj)
    
    
    #genes_volcano <- select(genes,id,log2FoldChange,padj)
    genes_volcano$padj <- -log10(genes_volcano$padj)
    
    if(input$TSD == 'Show'){
      
  
      if(nrow(genes_volcano)==0){
        
        genes_volcano[nrow(genes_volcano) + 1,] <- list("No Genes",0, 0)
        
      }  

    pval <- input$pval
    logfc <- input$logfc
    

    
    # add a column of NAs
    genes_volcano$diffexpressed <- "NO"
    genes_volcano$color <- "black"
    
    # if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP" 
    genes_volcano$diffexpressed[genes_volcano$log2FoldChange > logfc & genes_volcano$padj > -log10(pval)] <- "UP"
    genes_volcano$color[genes_volcano$log2FoldChange > logfc & genes_volcano$padj > -log10(pval)] <- "red"
    # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"<
    genes_volcano$diffexpressed[genes_volcano$log2FoldChange < -logfc & genes_volcano$padj > -log10(pval)] <- "DOWN"
    genes_volcano$color[genes_volcano$log2FoldChange < -logfc & genes_volcano$padj > -log10(pval)] <- "blue"
    
    colors_volcano <- levels(as.factor(genes_volcano$color))

    plots$plot_volcano <- ggplot(data=genes_volcano, aes(x=log2FoldChange, y=padj , col=diffexpressed, label=id)) +
        geom_point() +
        theme_bw() + 
        ylab("-log10(p-value)")+
        xlab("log2(FoldChange)")+
        
        # He quitado lo de mostrar las etiquetas porque ralentiza mucho el tiempo de carga del plot
        #geom_text_repel() +
        scale_color_manual(breaks= genes_volcano$diffexpressed, values= genes_volcano$color) +
        geom_vline(xintercept=c(-logfc, logfc), linetype = "dashed",col="black") +
        geom_hline(yintercept=-log10(pval), linetype = "dotted",col="black")
      
    }
    
    tables$genes_volcano <- genes_volcano
    
    genes["padj"] <- NULL
    tables$genes <- genes
    
    #print(str(tables$genes_volcano))
  
  })
  
  # SELECT GO ID
  observe({
    
    organismo <- input$organism
    
    
    tables$table_overexpressed_GO.ID <- tables$table_overexpressed[input$dt_rows_selected,1]
    tables$table_underexpressed_GO.ID <- tables$table_underexpressed[input$dt_1_rows_selected,1]
    
    tables$table_overexpressed_genes <- tables$table_overexpressed[input$dt_rows_selected,"genes"]
    tables$table_underexpressed_genes <- tables$table_underexpressed[input$dt_1_rows_selected,"genes"]
    
    gene_list_over <- str_split(tables$table_overexpressed_genes , "; ")
    gene_list_under <- str_split(tables$table_underexpressed_genes , "; ")
    
    if(length(gene_list_over)>0){
      gene_list_over <- gene_list_over[[1]]
    }
    
    if(length(gene_list_under)>0){
      gene_list_under <- gene_list_under[[1]]
    }
    #
    gene_list_over <- tables$genes[tables$genes[,1]%in%gene_list_over,]
    #gene_list_over <- gene_list_over %>% filter(log2FoldChange > 2)
    gene_list_over["padj"] <- NULL
    gene_list_over <- unique(gene_list_over)
    
    #
    gene_list_under <- tables$genes[tables$genes[,1]%in%gene_list_under,]
    #gene_list_under <- gene_list_under %>% filter(log2FoldChange < -2)
    gene_list_under["padj"] <- NULL
    gene_list_under <- unique(gene_list_under)

    if(organismo=="Ophiostoma"){
      gene_list_over[,1]<-sapply(gene_list_over[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
      
      gene_list_over <- gene_list_over[,-c(3,4,5)]
      
      gene_list_under[,1]<-sapply(gene_list_under[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
      
      gene_list_under <- gene_list_under[,-c(3,4,5)]
      
      
    }else{
      gene_list_under[,1]<-sapply(gene_list_under[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
      
      gene_list_under <- gene_list_under[,-c(5,6,7)]
      
      gene_list_over[,1]<-sapply(gene_list_over[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
      
      gene_list_over <- gene_list_over[,-c(5,6,7)]
    }
    
    
    tables$gene_list_over <- gene_list_over
    tables$gene_list_under <- gene_list_under
    
    # if(organism="Ophiostoma"){
    #   go_info <- read.csv(paste0(path_data,"Data JGI/Consortium/Ophiostoma reference/goinfo_ExternalModels.tab"), 
    #                       sep = "\t", header = T) %>% dplyr::rename("proteinid" = X.proteinId)
    # }else{
    #   
    # }
  })  

  # observe({
  # 
  #   print("SELECT FUNCTION ERROR")
  #   print(colnames(tables$genes_over))
  #   print(colnames(tables$genes_under))
  #   
  # 
  #   if(input$organism == "KT"){
  #     tables$genes_over <- select(tables$genes_over,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)
  #     tables$genes_under <- select(tables$genes_under,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)
  # 
  #   }
  #   
  #   if(input$organism == "Ophiostoma"){
  #     tables$genes_over <- select(tables$genes_over,id,	log2FoldChange,		GO_terms,	KEGG	,kogdefline,	InterPro,transcriptid)
  #     tables$genes_under <- select(tables$genes_under,id,	log2FoldChange,		GO_terms,	KEGG	,kogdefline,	InterPro,transcriptid)
  #   }
  # 
  # 
  # 
  # })

  # observe({
  #   if(input$organism == "KT"){
  #     tables$colums_over <- c("tables$genes_over","id","log2FoldChange",	"product","prot_ID","GO_function",	"GO_process"	,"GO_component"	,"KEGG_definitions")
  #     tables$colums_under <- c("tables$genes_under","id","log2FoldChange",	"product","prot_ID","GO_function",	"GO_process"	,"GO_component"	,"KEGG_definitions")
  # 
  # 
  #   }else{
  #     tables$colums_over <- c("tables$genes_over","id",	"log2FoldChange",		"GO_terms",	"KEGG"	,"kogdefline",	"InterPro","transcriptid")
  #     tables$colums_under <- c("tables$genes_under","id",	"log2FoldChange",		"GO_terms",	"KEGG"	,"kogdefline",	"InterPro","transcriptid")
  #   }
  # 
  # })



  output$plot_volcano <- renderPlot({plots$plot_volcano})
  
  output$valuebox_over <- renderValueBox({
    valueBox(
      paste0(nrow(tables$genes_over)), "Over", icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$valuebox_under <- renderValueBox({
    valueBox(
      nrow(tables$genes_under), "Under", icon = icon("arrow-down"),
      color = "blue"
    )
  })
  ###########################
  observe({
    under <- colnames(tables$genes_under)
    over <- colnames(tables$genes_over)
    under[2]<-"fc"
    over[2]<-"fc"
    
    tables$colnames_genes_under <- under
    tables$colnames_genes_over <- over
    
  })
  
  output$genes_over <- renderDataTable({
    req(data())
    datatable(
      tables$genes_over,
      colnames = tables$colnames_genes_over,
      
      #data_underexpressed(),
      escape=FALSE,
      rownames = FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     scrollX = T,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 15,
                     autoWidth = TRUE,scrollX=FALSE,
                     columnDefs = list(list(width = '20px', targets = 2 ))
      )
    ) %>%
      formatSignif(columns = c( "log2FoldChange"), digits = 3)
    
  })
  
  
  
  output$genes_under <- renderDataTable({
    req(data())
    datatable(
      tables$genes_under,
      colnames = tables$colnames_genes_under,
      #data_underexpressed(),
      escape=FALSE,
      rownames = FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     scrollX = T,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 15,
                     autoWidth = TRUE,scrollX=FALSE,
                     columnDefs = list(list(width = '20px', targets = 2 ))
      )
    ) %>%
      formatSignif(columns = c( "log2FoldChange"), digits = 3)
    
  })
  
  # output$targeted_over <- renderDataTable({tables$genes_over})
  # 
  # output$targeted_under <- renderDataTable({tables$genes_under})
  
  
  ## UNDER
  observeEvent(   input$dt_1_rows_selected,
                ignoreNULL = TRUE,
                {

                  showModal(modalDialog(title=HTML(paste0("<b>",tables$table_underexpressed[input$dt_1_rows_selected,1],"</b> - ",tables$table_underexpressed[input$dt_1_rows_selected,2])),
                                        {
                                          renderDataTable({
                                            req(data())
                                            datatable(
                                              tables$gene_list_under,
                                              #tables$table_underexpressed,
                                              #data_underexpressed(),
                                              escape=FALSE,
                                              rownames = FALSE,
                                              extensions = 'Buttons',
                                              options = list(dom = 'Bfrtip', 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                             pageLength = 100)
                                            ) %>%
                                              formatRound(columns=c('log2FoldChange'), digits=3)}
                                          )},
                                        header=tagList(
                                          modalButton('Close')
                                        ),
                                        footer=tagList(
                                          #downloadButton(outputId = "dwnld_data", "Download Data"),
                                          modalButton('Close')),

                                        size = "l",
                                        easyClose = TRUE,
                                        fade = FALSE
                  ))

                })
  #input$show
  
  
  ## OVER
  observeEvent( input$dt_rows_selected ,
                ignoreNULL = TRUE,
                {

    showModal(modalDialog(title=HTML(paste0("<b>",tables$table_overexpressed[input$dt_rows_selected,1],"</b> - ",tables$table_overexpressed[input$dt_rows_selected,2])),
                          {
      renderDataTable({
        req(data())
        datatable(
          tables$gene_list_over,
          #tables$table_underexpressed,
          #data_underexpressed(),
          escape=FALSE,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(dom = 'Bfrtip', 
                         buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                         pageLength = 100)
        ) %>%
          formatRound(columns=c('log2FoldChange'), digits=3)}
      )},
      header=tagList(
        modalButton('Close')
      ),
    footer=tagList(
      #downloadButton(outputId = "dwnld_data", "Download Data"),
      modalButton('Close')),

    size = "l",
    easyClose = TRUE,
    fade = FALSE
    ))

  })

  observeEvent(input$show, {
    showModal(
      tags$div(id="help_modal", modalDialog(
        size = "m",
        title = "Help window",
        HTML(readChar('data/q_page.txt', file.info('data/q_page.txt')$size)),
        easyClose = TRUE,
        footer = NULL
      )))

  })
  
  # output$dwnld_data <- downloadHandler(
  #   filename = "data.csv",
  #   content = function(con){
  #     on.exit(removeModal())
  #     data.table::fwrite(data, con)
  #   }
  # )
  
  
}

shinyApp(ui, server)

