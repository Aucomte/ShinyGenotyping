tabItem(
  tabName ="inputs",
  pageWithSidebar(
    headerPanel("Input data"),
    sidebarPanel(
      conditionalPanel(condition="input.tabselected2=='1'", 
                       h3("INPUT"),
                       downloadButton("downloadData", label = "Download a test file")%>%
                         helper(icon = "question",
                                type = "markdown",
                                content = "downloadData"),
                       bsPopover("downloadData", "Example Data Set", content = "data test (repetition file + genemapper files) in a zip archive", placement = "bottom", trigger = "hover", options = NULL),
                       HTML("<br><br>"),
                       radioButtons("inputtype", "What data source to use?",
                                    list("Repetition file"="rep","Genemapper Output"="genemapper-output")),
                       conditionalPanel(condition = "input.inputtype=='rep'",
                                   fileInput("file1", "Input file (csv or txt)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")) %>%
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
                                   fileInput("repFile", "Locus File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))%>%
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
                       conditionalPanel(condition = "input.inputtype=='genemapper-output'",
                          p("locus = size of locus, locus_rep = number of repetitions of the locus")
                       ),
                       selectInput(inputId = "strata", "Population : ", choice = ""),
                       p("Some analysis will not be possible if some populations are not sufficiently represented"),
                       radioButtons(inputId = "genindfilterbypop", "Filter populations with few individuals :", choiceNames = c("yes","no"), choiceValues = c("yes","no"), selected = "no"),
                       conditionalPanel(condition="input.genindfilterbypop=='yes'",
                              sliderInput("filtergenindpop","minimal population size :", min = 1, max = 15, 5, step = 1)
                       ),
                       radioButtons(inputId = "genindtype", "type of marker :", choiceNames = c("codominant","presence/absence"), choiceValues = c("codom","PA"), selected = "codom"),
                       #sliderInput(inputId = "ploid", "ploidy number :", min = 1, max = 6, 1, step = 1),
                       actionButton(inputId="Submit","Submit")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabselected2",
                  tabPanel("input", value=1,
                           fluidRow(
                             box(width = 12, class="box2", 
                                 "First of all, please submit your input data (in csv or txt tabulated format), either repetition file or genemapper output (see file example). Do not forget to hit submit files!
                                 After submitting, fill in the genotype object tab to indicate your loci, the variable you choose as population and then create the genind object 
                                 that will be used for the rest of the analyse."
                                 ),
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
                           box(width = 12, class="box2", 
                               "genind = adegenet class for individual genotypes",
                               br(),
                               a("(rdocumentation)", href="https://www.rdocumentation.org/packages/adegenet/versions/1.0-0/topics/genind", target="_blank"),
                               br(),
                               "Before everithing select the loci and the population (ex: Country for the datatest) you want to work with, the his the button Submit. 
                               If you change the parameters, do not forget to hit sumbit again."
                           ),
                           conditionalPanel(condition= "input.Submit",
                             fluidRow(
                               box(width = 12,
                                   textOutput("numberOfHaplo"),
                                   textOutput("numberOfind"),
                                   conditionalPanel(condition="input.genindfilterbypop=='yes'",
                                       textOutput("populationremoved")
                                   )
                               )
                             ),
                             fluidRow(
                               box(width = 12, class="box2", 
                                   "Each haplotype have been numeroted and have a specific allelic profile. Each strain have a corresponding haplotype."
                               ),
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
