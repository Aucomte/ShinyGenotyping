tabItem(
  tabName ="inputs",
  pageWithSidebar(
    headerPanel("Input data"),
    sidebarPanel(
      conditionalPanel(condition="input.tabselected2=='1'", 
                       h3("INPUT"),
                       radioButtons("inputtype", "What data source to use?",
                                    list("Repetition file"="rep","Genemapper Output"="genemapper-output")),
                       conditionalPanel(condition = "input.inputtype=='rep'",
                                   fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       conditionalPanel(condition = "input.inputtype=='genemapper-output'",
                                   fileInput("genemapperfile", "Genemapper File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                   fileInput("Metadata", "Metadata File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                   fileInput("repFile", "Repetition File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                       ),
                       radioButtons('sep', 'Separator',
                                    c(Semicolon=';',
                                      Tab='\t'),
                                    selected = '\t'),
                       actionButton("submitGenemapperFiles", label = "Submit files", icon = NULL, width = NULL)
      ),
      conditionalPanel(condition="input.tabselected2=='2'",   
                       h3("Create genotype object"),
                       checkboxGroupInput(inputId = "checkboxcol", "Loci: "),
                       selectInput(inputId = "strata", "Population : ", choice = ""),
                       radioButtons(inputId = "genindtype", "type of marker :", choiceNames = c("codominant","presence/absence"), choiceValues = c("codom","PA"), selected = "codom"),
                       sliderInput(inputId = "ploid", "ploidy number :", min = 1, max = 6, 1, step = 1),
                       actionButton(inputId="Submit","Submit")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabselected2",
                  tabPanel("input", value=1,
                           conditionalPanel(condition="input.inputtype=='genemapper-output'", 
                                  h3("Genemapper table :"),
                                  DT::dataTableOutput(outputId = "GMdataset"),
                                  DT::dataTableOutput(outputId = "Metadataset"),
                                  h3("repetition table :"),
                                  DT::dataTableOutput(outputId = "REPdataset"),
                                  h3("Final Table :"),
                                  DT::dataTableOutput(outputId = "DataSetFinal")
                           ),
                           conditionalPanel(condition="input.inputtype=='rep'", 
                                  DT::dataTableOutput(outputId = "DataSet")
                           )
                  ),
                  tabPanel("Genotype object", value=2,
                           fluidRow(
                             box(width = 12,
                                 textOutput("numberOfHaplo"),
                                 textOutput("numberOfind")
                             )
                             # ,
                             # fluidRow(
                             #   box(width = 12,
                             #     htmlOutput("genindStat")
                             #   )
                             # )
                           ),
                           fluidRow(
                             box(width = 12,
                                 DT::dataTableOutput(outputId = "tabhaplo")
                             ),
                             box(width = 12,
                                 DT::dataTableOutput(outputId = "tabhaploloc")
                             )
                           )
                  )
      )
    )
  )
)
