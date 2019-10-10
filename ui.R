##TODO trouver un endroit ou le mettre
jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"


header <- dashboardHeader(title = "ShinyGenotyping", titleWidth = 380)
useShinyjs()
extendShinyjs(text = jscode)
inlineCSS(css)
sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("Input", tabName = "inputs", icon = icon("book-open")), 
    menuItem("Statistics", tabName = "stats", icon = icon("calculator")), 
    menuItem("DAPC", tabName = "DAPC", icon = icon("calculator"))
  )
)
body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  #tags$head(tags$script(HTML(js))),
  tabItems(
    tabItem(
      tabName ="menu"
    ),
    tabItem(
      tabName ="inputs",
      pageWithSidebar(
        headerPanel("Input data"),
        sidebarPanel(
          conditionalPanel(condition="input.tabselected2=='1'", 
               h3("INPUT"),
               fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               radioButtons('sep', 'Separator',
                            c(Semicolon=';',
                              Tab='\t'),
                            selected = '\t')
          ),
          conditionalPanel(condition="input.tabselected2=='2'",   
                           h3("Create genotype object"),
                           checkboxGroupInput(inputId = "checkboxcol", "Loci: "),
                           selectInput(inputId = "strata", "Strata : ", choice = ""),
                           radioButtons(inputId = "genindtype", "type of marker :", choiceNames = c("codominant","presence/absence"), choiceValues = c("codom","PA"), selected = "codom"),
                           sliderInput(inputId = "ploid", "ploidy number :", min = 1, max = 6, 1, step = 1),
                           actionButton(inputId="Submit","Submit")
          )
        ),
        mainPanel(
          tabsetPanel(id = "tabselected2",
            tabPanel("input", value=1,
                     DT::dataTableOutput(outputId = "DataSet")
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
    ),
    tabItem(
      tabName ="stats",
      pageWithSidebar(
        headerPanel("Statistics"),
        sidebarPanel(
            conditionalPanel(condition="input.tabselected == '3'", 
              h3("PCA"),
              checkboxGroupInput(inputId = "checkboxcolPCA", "variables: "),
              selectInput(inputId = "colsupDiv", "suplementary explicative column : ", choice = ""),
              selectInput(inputId = "axeschoices", "axes", choice=c("axe1 vs axe2", "axe1 vs axe3", "axe2 vs axe3")),
              actionButton(inputId="Submitpca","Submit")
            ),
          conditionalPanel(condition="input.tabselected == '4'", 
            actionButton("submitarchive", "Calculate the diversity by locus (can be long)")
          ),
          conditionalPanel(condition="input.tabselected == '5'", 
            radioButtons(inputId = "drop", "non prise en compte des loci monomorphes :", choiceNames =  c("yes","no"), choiceValues = c("T","F"), selected = "F"),
            radioButtons(inputId = "dropna", "non prise en compte des données manquantes :", choiceNames = c("yes","no"), choiceValues = c("T","F"), selected = "F"),
            sliderInput(inputId = "thresgeno", "threshold :", min = 0, max = 1, 0.95, step = 0.05),
            sliderInput(inputId = "samplegeno", "number of times loci will be resampled without replacement :", min = 0, max = 20000, 10000, step = 100),
            actionButton(inputId="Submitcurve","Submit")
          ),
          conditionalPanel(condition="input.tabselected == '6'", 
            sliderInput(inputId = "samplepoppr", "number of permutations desired to obtain p-values (sample) :", min = 0, max = 10000, 1000, step = 50),
            sliderInput(inputId = "minsamp", "the minimum number of individuals to resample for rarefaction analysis (minsample) :", min = 0, max = 15, 8, step = 1),
            radioButtons(inputId = "missingpopp", "how should missing data be treated? (missing):", choiceNames = c("mean","zero"), choiceValues = c("mean", "zero"), selected = "mean"),
            actionButton(inputId="Submitstat","Submit")
          )
        ),
        mainPanel(
          tabsetPanel(id = "tabselected",
            tabPanel("PCA", value=3, id = "t3",
               conditionalPanel("input.Submitpca",
                  fluidRow(
                    box(width = 6,
                        plotOutput(outputId = "pcaInd", height = "600px")
                        %>% withLoader(loader = "dnaspin")
                    ),
                    box(width = 6,
                        plotOutput(outputId = "pcaVar", height = "600px")
                        %>% withLoader(loader = "dnaspin")
                    ),
                    fluidRow(
                      box(width = 6,
                          plotOutput(outputId = "pcahab", height = "600px")
                          %>% withLoader(loader = "dnaspin")
                      ),
                      box(width = 6,
                          plotOutput(outputId = "pcahabi", height = "600px")
                          %>% withLoader(loader = "dnaspin")
                      )
                    )
                  )
               )
            ),
            tabPanel("PopGeneReport", value=4, id = "t4",
                     fluidRow(
                       fluidRow(
                         conditionalPanel("output.heatmapDiv",
                            box(width = 12,
                                "Diversity by locus, estimated by PopGeneReport :   ", 
                                br(),
                                downloadButton('downloadDiv', 'Download Output archive',style="color: #fff; background-color: #ff0000; border-color: #000000; text-align: center;")
                            )
                         )
                       ),
                        fluidRow(
                         conditionalPanel("input.submitarchive",
                            box(width = 12,
                                "Individual counts, allelic frequencies, observed heterozygosities and genetic diversities by locus :",
                                br(),
                                DT::dataTableOutput(outputId = "genostatbasePerLoc") 
                                #%>% withSpinner(color="#0dc5c1")
                                %>% withLoader(loader = "dnaspin")
                            ),
                            box(width = 12,
                                "Hierarchical F-Statistics :", 
                                br(),
                                DT::dataTableOutput(outputId = "genind2hierfstat") 
                                #%>% withSpinner(color="#0dc5c1")
                                %>% withLoader(loader = "dnaspin")
                            ),
                            box(width = 12,
                                "missing data by locus and by population:",
                                br(),
                                plotOutput(outputId = "heatmapDiv", height = "600px") 
                                #%>% withSpinner(color="#0dc5c1") 
                                %>% withLoader(loader = "dnaspin")
                            )
                         )
                       )
                     )
            ),
            tabPanel("rarefaction curve", value=5, id = "t5",
                  conditionalPanel("input.Submitcurve",
                     plotOutput(outputId = "genocurve", height = "600px")  
                      %>% withLoader(loader = "dnaspin")
                  )
            ),
            tabPanel("Poppr", value=6,
                 conditionalPanel("input.Submitstat",
                      # box(width = 12,
                      #     plotOutput(outputId = "popprplot", height = "600px")
                      #       %>% withLoader(loader = "dnaspin")
                      # ),
                      box(width = 12,
                          DT::dataTableOutput(outputId = "popprtab")
                            %>% withLoader(loader = "dnaspin")
                      )
                 )
            )
          )
        )
      )
    ),
    tabItem(
      tabName ="DAPC",

      pageWithSidebar(
        headerPanel("DAPC Analysis"),
        ## SIDE PANEL CONTENT
        sidebarPanel(
          ## define the type of input
          conditionalPanel(condition="input.tabDAPC=='21'",  
            radioButtons("datatype", "What data source to use?",
                         list("Input"="expl","Input file"="file"))
            ),
          
          ## choice of dataset if source is an example
          conditionalPanel(condition = "input.datatype=='expl'",
                           selectInput("dataset", "The dataset", choices="Input")
          ),
          
          ## choice of dataset if source is a file
          conditionalPanel(condition = "input.datatype=='file'",
                           fileInput('datafile', 'Choose input file',
                           accept=c('gtx/gen/dat/GTX/GEN/DAT/RData/Rdata/Rda/rda', 'GENETIX/genepop/Fstat/R data')),
                           tags$hr()
          ),
          
          ## CROSS-VALIDATION
          # n.pca.max slider
          conditionalPanel(condition="input.tabDAPC=='25'",  
            h3("Cross-validation"),
            uiOutput("doxval"),
            uiOutput("npcaMax")
          ),
          
          
          # Select Output variable:
          conditionalPanel(condition="input.tabDAPC != '25'", 
            checkboxInput("useoptimnpca", "Use suggested number of PCA components?", FALSE)
          ),
          
          conditionalPanel(condition="input.tabDAPC=='21'",  
                           radioButtons("clusters", "Number of clusters to use?",
                                        list("Choose a number of clusters"="num","Clusters are genind object pop"="pop"))
          ),
          ## nclust
          conditionalPanel(condition="input.clusters == 'num'", 
                           sliderInput("nclust",
                                       "Number of clusters:",
                                       min = 1,
                                       max = 100,
                                       value = 3)
          ),
          
          
          ##sliderInput("npca", "Number of PCA axes retained:", min=1, max=1000, value=10),
          conditionalPanel(condition="input.tabDAPC != '25'", 
            uiOutput("npca")
          ),
          
          ## select number of DA axes
          ##sliderInput("nda", "Number of discriminant functions retained:", min=1, max=100, value=1),
            uiOutput("nda"),
          
          ## nrep slider
          conditionalPanel(condition="input.tabDAPC == '25'", 
            sliderInput("nrep",
                        "Number of replicates:",
                        min = 1,
                        max = 100,
                        value = 3)
          ),
          
          ## trainingset slider
          conditionalPanel(condition="input.tabDAPC == '25'", 
            sliderInput("trainingset",
                        "Training set size:",
                        min = 0.1,
                        max = 0.95,
                        value = 0.9,
                        step = 0.01)
          ),
          
          
          ## result type
          conditionalPanel(condition="input.tabDAPC == '25'", 
            radioButtons("result", "Assess by:",
                         list("Group" = "groupMean",
                              "Overall" = "overall"))
          ),
          
          
          
          conditionalPanel(condition="input.tabDAPC == '21'", 
            h3("Graphical parameters")
          ),
          
          
          
          ## inputs specific of scatterplot tab
          conditionalPanel(condition="input.tabDAPC == '21'", 
            
            ## select first axis to plot
            ##numericInput("xax", "Indicate the x axis", value=1, min=1),
            uiOutput("xax"),
            
            ## select second axis to plot
            ##numericInput("yax", "Indicate the y axis", value=1, min=1),
            uiOutput("yax")
          ),
          
          
          
          conditionalPanel(condition="input.tabDAPC == '21'", 
            h3("Aesthetics")
          ),
          
          conditionalPanel(condition="input.tabDAPC == '23'",
            h3("Aesthetics")
          ),
          
          
          
          ## select color palette
          conditionalPanel(condition="input.tabDAPC == '21'", 
            selectInput("col.pal", "Indicate a color palette to be used",
                        choices=c("funky","spectral","seasun","azur","wasp"))
          ),
          
          ## select color palette
          conditionalPanel(condition="input.tabDAPC == '23'",
            selectInput("col.pal", "Indicate a color palette to be used",
                        choices=c("funky","spectral","seasun","azur","wasp"))
          ),
          
          
          
          ## select transparency
          conditionalPanel(condition="input.tabDAPC == '21'", 
            sliderInput("alpha", "Choose transparency", min=0, max=1, step=0.05, value=0.5)
          ),
          
          conditionalPanel(condition="input.tabDAPC == '21'",
            ## symbol size
            sliderInput("pointsize", "Size of the points", value=1, min=0, max=10, step=0.2),
            
            ## label size
            sliderInput("labelsize", "Size of the labels", value=1, min=0, max=10, step=0.2),
            
            ## add screeplot of PCA?
            selectInput("screepca", "Position of the PCA screeplot:",
                        choices=c("None" = "none",
                                  "Bottom right" = "bottomright",
                                  "Bottom left" = "bottomleft",
                                  "Top right" = "topright",
                                  "Top left" = "topleft")),
            
            ## add screeplot of DA?
            selectInput("screeda", "Position of the DA screeplot:",
                        choices=c("None" = "none",
                                  "Bottom right" = "bottomright",
                                  "Bottom left" = "bottomleft",
                                  "Top right" = "topright",
                                  "Top left" = "topleft")),
            
            ## plot ellipses?
            checkboxInput("ellipses", "Show inertia ellipses?", value=TRUE),
            
            ## plot stars?
            checkboxInput("stars", "Link points to their centre?", value=TRUE),
            
            ## plot minimum spanning tree?
            checkboxInput("mstree", "Show minimum spanning tree?", value=FALSE)
            
          ),
          
          ## input specific of compoplot tab
          conditionalPanel(condition="input.tabDAPC=='23'",
            ## add legend?
            checkboxInput("compo.legend", "Add a legend?", TRUE),
            ## add labels?
            checkboxInput("compo.lab", "Display labels?", FALSE)
          ),
          
          ## inputs specific of loadingplot tab
          conditionalPanel(condition="input.tabDAPC == '24'",
            
            h3("Graphical parameters"),
            
            ## select axis to plot
            uiOutput("LPax"),
            
            checkboxInput("threshold", "Display threshold?", TRUE),
            
            selectInput("thresholdMethod", "Method for selecting threshold:",
                        choices=c("Third quartile" = "quartile",
                                  "Complete linkage clustering" = "complete",
                                  "Single linkage clustering" = "single",
                                  "Average linkage clustering" = "average",
                                  "Centroid clustering" = "centroid",
                                  "McQuitty's similarity analysis" = "mcquitty",
                                  "Median clustering" = "median",
                                  "Ward's minimum variance method" = "ward")),
            
            checkboxInput("FS", "Select and describe features above threshold", FALSE)
          )
        ), # end sidebarPanel
        
        ## MAIN PANEL
        mainPanel(
          tabsetPanel(id = "tabDAPC",
            
            tabPanel("Scatterplot", value=21,
                     plotOutput("scatterplot")
                     ),
            tabPanel("Summary", value=22, 
                     verbatimTextOutput("summary")
                     ),
            tabPanel("Compoplot", value=23, 
                     plotOutput("compoplot")
                     ),
            tabPanel("Loading Plot", value=24, 
                     plotOutput("loadingplot"),
                     conditionalPanel(condition = "input.FS==1",
                                      h3("Number of selected vs. unselected alleles"),
                                      verbatimTextOutput("FS1"),
                                      h3("List of selected alleles"),
                                      verbatimTextOutput("FS2"),
                                      h3("Names of selected alleles"),
                                      verbatimTextOutput("FS3"),
                                      h3("Contributions of selected alleles to discriminant axis"),
                                      verbatimTextOutput("FS4"))),
            
            tabPanel("Cross-Validation", value=25,  
                     plotOutput("xvalPlot"),
                     h3("Mean success by number of PCs"),
                     verbatimTextOutput("xvalResults3"),
                     h3("Number of PCs with highest mean"),
                     verbatimTextOutput("xvalResults4"),
                     h3("RMSE by number of PCs"),
                     verbatimTextOutput("xvalResults5"),
                     h3("Number of PCs with lowest RMSE"),
                     verbatimTextOutput("xvalResults6"),
                     h3("Cross-validation results"),
                     verbatimTextOutput("xvalResults1"),
                     h3("Median and CI for random chance"),
                     verbatimTextOutput("xvalResults2"))

          ) # end tabsetPanel
        ) # end mainPanel
            ) # end pageWithSidebar
        ) # end shinyUI
    )
)
shinyUI(
  dashboardPage(title = "MLVA", skin = "yellow", header, sidebar, body)
)
