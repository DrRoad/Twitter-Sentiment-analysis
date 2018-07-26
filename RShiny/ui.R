packages = function(package){
  new_package = package[!(package %in% installed.packages()[,'Package'])]
  if(length(new_package))
    install.packages(new_package,dependencies = T)
  sapply(package , require , character.only = TRUE)
}
pkg = c("shiny","shinythemes")
packages(pkg)
shinyUI(
  fluidPage(theme = shinytheme("spacelab"),
      titlePanel("TWITTER SENTIMENT ANALYSIS", windowTitle = "SENTIMENT ANALYSIS"),
    
    sidebarLayout(
      
      
      
      sidebarPanel(
        conditionalPanel(condition = "input.tabselected == 0",
                         br(),br(),  # Two line breaks for visual separation
                         h4("Team Members : "),
                         br(),
                         h4("Name : SHUBHAM KUMAR SONDHIYA"),
                         h4("USN : 1NT15CS174 "),
                         br(),
                         h4("Name : VINAYAK PAI"),
                         h4("USN : "),
                         br(),
                         h4("Name : SWATHI "),
                         h4("USN : "),
                         br(),
                         
                         br(),
                         h5("Built with",
                            img(src = "shiny.png", height = "30px"),
                            "Powered by",
                            img(src = "RStudio.png", height = "30px"),
                            ".")
        ),
        
        conditionalPanel(condition = "input.tabselected == 1" ,
                         selectInput(inputId = "taining-dataset-plots-google",
                                     label = "SELECT-TRAINING-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                                     
                         ),
                         selectInput(inputId = "test-dataset-plots-google",
                                     label = "SELECT-PREDICT-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         radioButtons(inputId = "show-dataset-google",
                                            label = "CHOOSE A DATASET:",
                                            choices = c("TRAINING-DATASET"=1, 
                                                        "TEST-DATASET"=2)
                         ),
                         numericInput(inputId = "observations-google",
                                      label = "NUMBER OF OBSERVATIONS TO VIEW:",
                                      value = 10),
                         radioButtons(inputId = "show-summary-google",
                                            label = "CHOOSE AN OPTION TO DISPLAY:",
                                            choices = c("SUMMARY"=1, 
                                                        "CONFUSION-MATRIX"=2))
                         ),
                         
        conditionalPanel(condition = "input.tabselected == 2",
                         selectInput(inputId = "taining-dataset-plots-alexa",
                                     label = "SELECT-TRAINING-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         selectInput(inputId = "test-dataset-plots-alexa",
                                     label = "SELECT-PREDICTION-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         radioButtons(inputId = "show-dataset-alexa",
                                            label = "CHOOSE A DATASET:",
                                            choices = c("TRAINING-DATASET"=1, 
                                                        "TEST-DATASET"=2)),
                         numericInput(inputId = "observations-alexa",
                                      label = "NUMBER OF OBSERVATIONS TO VIEW:",
                                      value = 10),
                         radioButtons(inputId = "show-summary-alexa",
                                            label = "CHOOSE AN OPTION TO DISPLAY:",
                                            choices = c("SUMMARY"=1, 
                                                        "CONFUSION-MATRIX"=2))
                         ),
        conditionalPanel(condition = "input.tabselected == 3" ,
                         selectInput(inputId = "taining-dataset-plots-siri",
                                     label = "SELECT-TRAINING-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         selectInput(inputId = "test-dataset-plots-siri",
                                     label = "SELECT-PREDICTION-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         radioButtons(inputId = "show-dataset-siri",
                                            label = "CHOOSE A DATASET:",
                                            choices = c("TRAINING-DATASET"=1, 
                                                        "TEST-DATASET"=2)),
                         numericInput(inputId = "observations-siri",
                                      label = "NUMBER OF OBSERVATIONS TO VIEW:",
                                      value = 10),
                         radioButtons(inputId = "show-summary-siri",
                                            label = "CHOOSE AN OPTION TO DISPLAY:",
                                            choices = c("SUMMARY"=1, 
                                                        "CONFUSION-MATRIX"=2))
                         ),
        conditionalPanel(condition = "input.tabselected == 4" ,  
                         selectInput(inputId = "taining-dataset-plots-cortana",
                                     label = "SELECT-TRAINING-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         selectInput(inputId = "test-dataset-plots-cortana",
                                     label = "SELECT-PREDICTION-PLOT:",
                                     choices = c("BAR-PLOT"=1,"PIE-CHART"=2)
                         ),
                         radioButtons(inputId = "show-dataset-cortana",
                                            label = "CHOOSE A DATASET:",
                                            choices = c("TRAINING-DATASET"=1, 
                                                        "TEST-DATASET"=2)),
                         numericInput(inputId = "observations-cortana",
                                      label = "NUMBER OF OBSERVATIONS TO VIEW:",
                                      value = 10),
                         radioButtons(inputId = "show-summary-cortana",
                                            label = "CHOOSE AN OPTION TO DISPLAY:",
                                            choices = c("SUMMARY"=1, 
                                                        "CONFUSION-MATRIX"=2))
                         ),
        conditionalPanel(condition = "input.tabselected == 5" , 
                         selectInput(inputId = "show-plots-together",
                                            label = "CHOOSE A BAR-PLOT TO DISPLAY:",
                                            choices = c("GROUPED-BARPLOT"=1, 
                                                        "STACKED-BARPLOT"=2))
                         
                         )
        ),
        
      # Output
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel(title = "ABOUT",br(),value = 0,
                             htmlOutput("DESCRIPTION"),
                             br()
                             ),
                    tabPanel(title = "GOOGLE",br(),value = 1, 
                             plotOutput(outputId = "training-plot-google"),
                             br(),br(),
                             plotOutput(outputId = "prediction-plot-google"),
                             br(),br(),
                             tags$h3("Tables for Training and Prediction Datasets"),
                             DT::dataTableOutput(outputId = "showdata-google"),
                             br(),br(),
                             tags$h3("Outputs for Training-Summary and Prediction Confusion-Matrix"),
                             verbatimTextOutput(outputId = "showsummary-google"),
                             br(),br()),
                    tabPanel(title = "ALEXA", br(), value = 2,
                             plotOutput(outputId = "training-plot-alexa"),
                             br(),br(),
                             plotOutput(outputId = "prediction-plot-alexa"),
                             br(),br(),
                             tags$h3("Tables for Training and Prediction Datasets"),
                             DT::dataTableOutput(outputId = "showdata-alexa"),
                             br(),br(),
                             tags$h3("Outputs for Training-Summary and Prediction Confusion-Matrix"),
                             verbatimTextOutput(outputId = "showsummary-alexa"),
                             br(),br()
                             ),
                    tabPanel(title = "SIRI",br(), value = 3,
                             plotOutput(outputId = "training-plot-siri"),
                             br(),br(),
                             plotOutput(outputId = "prediction-plot-siri"),
                             br(),br(),
                             tags$h3("Tables for Training and Prediction Datasets"),
                             DT::dataTableOutput(outputId = "showdata-siri"),
                             br(),br(),
                             tags$h3("Outputs for Training-Summary and Prediction Confusion-Matrix"),
                             verbatimTextOutput(outputId = "showsummary-siri"),
                             br(),br()
                             ),
                    
                    tabPanel(title = "CORTANA",br(), value = 4,
                             plotOutput(outputId = "training-plot-cortana"),
                             br(),br(),
                             plotOutput(outputId = "prediction-plot-cortana"),
                             br(),br(),
                             tags$h3("Tables for Training and Prediction Datasets"),
                             DT::dataTableOutput(outputId = "showdata-cortana"),
                             br(),br(),
                             tags$h3("Outputs for Training-Summary and Prediction Confusion-Matrix"),
                             verbatimTextOutput(outputId = "showsummary-cortana"),
                             br(),br()
                             ),
                    
                    tabPanel(title = "OVERALL-ANALYSIS",br(), value = 5,
                             plotOutput(outputId = "grouped-plot" , height = 600),
                             plotOutput(outputId = "stacked-plot" , height = 600),
                             br(),br()),
                    id = "tabselected"
                    )
        )
      )
    )
  )
      


