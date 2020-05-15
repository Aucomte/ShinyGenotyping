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
                       h3("WARNING : works only if a population is defined for gening object. Can be long to run."),
                       actionButton("submitarchive", "Calculate the diversity by locus")
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
                                              conditionalPanel(condition = "input.colsupDiv != 'None'",
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
                           )
                  ),
                  tabPanel("Basic statistics", value=4, id = "t4",
                           fluidRow(
                             fluidRow(
                               conditionalPanel("output.heatmapDiv",
                                                box(width = 12,
                                                    h4("Diversity by locus, estimated by PopGeneReport :   "), 
                                                    br(),
                                                    downloadButton('downloadDiv', 'Download Output archive',style="color: #fff; background-color: #ff0000; border-color: #000000; text-align: center;")
                                                )
                               )
                             ),
                             fluidRow(
                               conditionalPanel("input.submitarchive",
                                                box(width = 12,
                                                    h4("Pairwise FST :"),
                                                    DT::dataTableOutput(outputId = "pairwiseFST")
                                                    %>% withLoader(loader = "dnaspin")
                                                 ),
                                                box(width = 12,
                                                    h4("Basic statistics per locus (hierfstat) :")%>%
                                                      helper(icon = "question",
                                                             type = "markdown",
                                                             content = "genostatbasePerLoc"),
                                                    DT::dataTableOutput(outputId = "genostatbasePerLoc") 
                                                    %>% withLoader(loader = "dnaspin")
                                                ),
                                                box(width = 12,
                                                    h4("The number of alleles used for rarefaction :"),
                                                    verbatimTextOutput("AllelicRichnessMIN"),
                                                    h4("Rarefied allele counts :"),
                                                    DT::dataTableOutput(outputId = "AllelicRichness") 
                                                    %>% withLoader(loader = "dnaspin")
                                                ),
                                                box(width = 12,
                                                    h4("missing data by locus and by population:"),
                                                    br(),
                                                    plotOutput(outputId = "heatmapDiv", height = "600px") 
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
                  ),
                  tabPanel("MST", value=7,
                           conditionalPanel("input.strata!= 'None'",
                              h4("Dendrogram with bootstrap support using any Nei distance (Populations):"),
                              plotOutput(outputId = "MSTpop", height = "800px") %>% withLoader(loader = "dnaspin")
                           ),
                           h4("Dendrogram with bootstrap support using any Nei distance (Strains):"),
                           plotOutput(outputId = "MSTind", height = "800px") %>% withLoader(loader = "dnaspin")
                  )
      )
    )
  )
)
