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
                                   fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>%
                                     helper(icon = "question",
                                            type = "markdown",
                                            content = "file1")
                       ),
                       conditionalPanel(condition = "input.inputtype=='genemapper-output'",
                                   fileInput("genemapperfile", "Genemapper File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
                                     helper(icon = "question",
                                            type = "markdown",
                                            content = "genemapperfile"),
                                   fileInput("Metadata", "Metadata File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
                                     helper(icon = "question",
                                            type = "markdown",
                                            content = "Metadata"),
                                   fileInput("repFile", "Repetition File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
                                     helper(icon = "question",
                                            type = "markdown",
                                            content = "repFile")
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
                           fluidRow(
                             box(width = 12, "After submitting, please fill in the genotype object slot to indicate your loci, and the variable you choose as population."),
                             conditionalPanel(condition="input.inputtype=='genemapper-output'",
                                    conditionalPanel(condition="output.GMdataset",
                                      h3("Genemapper table :")
                                    ),
                                    DT::dataTableOutput(outputId = "GMdataset"),
                                    conditionalPanel(condition="output.Metadataset",
                                      h3("Metadata table :")
                                    ),
                                    DT::dataTableOutput(outputId = "Metadataset"),
                                    conditionalPanel(condition="output.REPdataset",
                                      h3("repetition table :")
                                    ),
                                    DT::dataTableOutput(outputId = "REPdataset"),
                                    conditionalPanel(condition="input.Submit",
                                      h3("Final Table :"),
                                      DT::dataTableOutput(outputId = "DataSetFinal")
                                    )
                             )
                           ),
                           conditionalPanel(condition="input.inputtype=='rep'", 
                                  DT::dataTableOutput(outputId = "DataSet")
                           )
                  ),
                  tabPanel("Genotype object", value=2,
                           conditionalPanel(condition= "input.Submit",
                             fluidRow(
                               box(width = 12,
                                   textOutput("numberOfHaplo"),
                                   textOutput("numberOfind")
                               )
                             ),
                             fluidRow(
                               box(width = 12,
                                   h3("Table 1: Haplotypes and Strains.")
                               ),
                               box(width = 12,
                                   DT::dataTableOutput(outputId = "tabhaplo") %>% withLoader(loader = "dnaspin")
                               ),
                               box(width = 12,
                                   h3("Table 2: Allelic profiles of the haplotypes.")
                               ),
                               box(width = 12, 
                                   DT::dataTableOutput(outputId = "tabhaploloc") %>% withLoader(loader = "dnaspin")
                               )
                             )
                           )
                  )
      )
    )
  )
)
