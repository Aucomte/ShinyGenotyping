tabItem(
  tabName ="stats",
  pageWithSidebar(
    headerPanel("Statistics"),
    sidebarPanel(
      conditionalPanel(condition="input.tabselected == '3'", 
                       h3("PCA"),
                       checkboxGroupInput(inputId = "checkboxcolPCA", "variables: "),
                       selectInput(inputId = "colsupDiv", "Supplementary variable to color the individual : ", choice = ""),
                       selectInput(inputId = "axeschoices", "axes", choice=c("axe1 vs axe2", "axe1 vs axe3", "axe2 vs axe3")),
                       checkboxInput(inputId ="ShowInd", "Show individuals on graph", value = FALSE),
                       checkboxInput(inputId ="ShowSup", "Show suplementary variable on graph", value = TRUE),
                       actionButton(inputId="Submitpca","Submit")
      ),
      conditionalPanel(condition="input.tabselected == '4'", 
                       h3("WARNING : works only if a population is defined for genind object. Can be long to run."),
                       actionButton("submitarchive", "Calculate the diversity by locus")
      ),
      conditionalPanel(condition="input.tabselected == '5'", 
                       radioButtons(inputId = "drop", "monomorphic loci will be removed before analysis :", choiceNames =  c("yes","no"), choiceValues = c("T","F"), selected = "F"),
                       radioButtons(inputId = "dropna", "NAs will be ignored when determining if a locus is monomorphic :", choiceNames = c("yes","no"), choiceValues = c("T","F"), selected = "F"),
                       sliderInput(inputId = "thresgeno", "threshold :", min = 0, max = 1, 0.95, step = 0.05),
                       sliderInput(inputId = "samplegeno", "number of times loci will be resampled without replacement :", min = 0, max = 20000, 10000, step = 100),
                       actionButton(inputId="Submitcurve","Submit")
      ),
      conditionalPanel(condition="input.tabselected == '6'", 
                       sliderInput(inputId = "samplepoppr", "number of permutations desired to obtain p-values (sample) :", min = 0, max = 10000, 1000, step = 50),
                       sliderInput(inputId = "minsamp", "the minimum number of individuals to resample for rarefaction analysis (minsample) :", min = 0, max = 15, 8, step = 1)%>%
                         helper(icon = "question",
                                type = "markdown",
                                content = "minsamp")
                       ,
                       radioButtons(inputId = "missingpopp", "how should missing data be treated? (missing):", choiceNames = c("mean","zero"), choiceValues = c("mean", "zero"), selected = "mean"),
                       actionButton(inputId="Submitstat","Submit")
      ),
      conditionalPanel(condition="input.tabselected == '7'", 
               selectInput("datasetMSN", 
                           "choose dataset",
                           choices = ""
               ),
             tags$h5("Status"),
             conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                              tagAppendChild(tags$div(class="progress"),
                                             tagAppendChild(tags$div(class="progress-bar progress-bar-success", 
                                                                     role="progressbar", `aria-valuenow`="100", 
                                                                     `aria-valuemin`="0", `aria-valuemax`="100", 
                                                                     style="width: 100%"), tags$strong("ready")))
             ),
             conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                              tagAppendChild(tags$div(class="progress"),
                                             tagAppendChild(tags$div(class="progress-bar progress-bar-striped active", 
                                                                     role="progressbar", `aria-valuenow`="100", 
                                                                     `aria-valuemin`="0", `aria-valuemax`="100", 
                                                                     style="width: 100%"), tags$strong("loading")))
             ),
             tagAppendChildren(
               tags$div(style="display:inline-block"),
               list(
                 actionButton("submit", "Go!", icon("check-circle")),
                 actionButton("update-data", "reData", icon("refresh")),
                 actionButton("update-graph", "reGraph", icon("refresh"))
               )
             ),
             HTML("
                  <h3>
                  Data 
                  <button type = 'button' class = 'btn btn-secondary btn-xs' data-toggle='collapse' data-target='#dparams' aria-expanded='true' aria-controls='dparams'> 
                  show/hide
                  </button>
                  </h3>
                  "),
             div(id = "dparams", class = "collapse in", 
                 # h3("Data Parameters"),
                 uiOutput("selectUI"),
                 uiOutput("selectPops"),
                 
                 #checkboxInput("genclone", "Convert to genclone?", TRUE),
                 selectInput("distance", 
                             "Choose a distance calculation", 
                             choices = c("Dissimilarity",
                                         "Bruvo",
                                         "Nei",
                                         "Rogers",
                                         "Edwards",
                                         "Provesti",
                                         "Reynolds",
                                         "Custom")
                 ),
                 conditionalPanel("input.distance == 'Custom'",
                                  uiOutput("customDist")
                 ),
                 conditionalPanel("input.distance == 'Bruvo'",
                                  selectInput("bruvo_model",
                                              "Select a model for missing data",
                                              choices = c("Genome Addition",
                                                          "Genome Loss",
                                                          "Infinite",
                                                          "Average Addition/Loss"),
                                              selected = "Average Addition/Loss"),
                                  textInput("replen", "SSR repeat lengths\n(comma separated or a valid R expression)", "1, 2, 3")
                 ),
                 conditionalPanel("input.distance != 'Bruvo'",
                                  uiOutput("distargsUI")
                 ),
                 checkboxInput("reticulate", "Include reticulations?", TRUE)
             ),
             HTML("
                  <h3>
                  Display
                  <button type = 'button' class = 'btn btn-secondary btn-xs'  data-toggle='collapse' data-target='#gparams' aria-expanded='true' aria-controls='gparams'>
                  show/hide
                  </a>
                  </h3>
                  "),
             div(id = "gparams", class = "collapse in", 
                 selectInput("layout", 
                             "Choose a layout", 
                             choices = c(
                               "layout_nicely",
                               "layout_randomly",
                               "layout_with_dh",
                               "layout_on_grid",
                               "layout_in_circle",
                               "layout_on_sphere",
                               "layout_as_tree",
                               "layout_as_star",
                               "layout_with_drl",
                               "layout_with_fr",
                               "layout_with_gem",
                               "layout_with_graphopt",
                               "layout_with_kk",
                               "layout_with_lgl",
                               "layout_with_mds",
                               "Custom")
                 ),
                 conditionalPanel("input.layout == 'Custom'",
                                  uiOutput("customLayout")
                 ),
                 checkboxInput("pop.leg", "Population legend", TRUE),
                 checkboxInput("size.leg", "Node size legend", TRUE),
                 checkboxInput("scale.leg", "Scale bar", TRUE), 
                 sliderInput("greyslide",
                             "Grey scale",
                             min = 0,
                             max = 25,
                             value = 3,
                             step = 1
                 ),
                 sliderInput("nodescale",
                             "Node scale",
                             value = 10, 
                             min = 1,
                             max = 100,
                             step = 1),
                 numericInput("seed", 
                              "Random Seed",
                              "69"
                 ),
                 radioButtons("ind_or_mlg", "Labels", 
                              choices = c("sample names", "MLGs"),
                              selected = "sample names", inline = TRUE
                 ),
                 textInput("inds", NULL, "ALL"),
                 checkboxInput("mlgs", "Show MLG", FALSE),
                 radioButtons("pal", "Indicate a color palette to be used",
                              choices=c("rainbow", 
                                        "cm.colors", 
                                        "topo.colors", 
                                        "terrain.colors", 
                                        "gray.colors",
                                        "funky",
                                        "spectral",
                                        "seasun",
                                        "azur",
                                        "wasp",
                                        "custom"), inline = TRUE
                 ),
                 conditionalPanel("input.pal == 'custom'",
                                  textInput("custom_pal", "Custom palette/function", "'purple'")
                 ),
                 numericInput("cutoff",
                              "Distance cutoff",
                              NULL,
                              step = 0.001
                 ),
                 checkboxInput("beforecut", "Keep graph position", TRUE)
             )
            )
    ),
    mainPanel(
      tabsetPanel(id = "tabselected",
                  tabPanel("PCA", value=3, id = "t3",
                           conditionalPanel("input.Submitpca",
                                fluidRow(
                                  box(width = 12,
                                      plotOutput(outputId = "pcaInd", height = "800px")
                                      %>% withLoader(loader = "dnaspin")
                                  )
                                ),
                                  fluidRow(
                                    box(width = 12,
                                      plotOutput(outputId = "pcaVar", height = "800px")
                                      %>% withLoader(loader = "dnaspin")
                                    )
                                  ),
                            conditionalPanel(condition = "input.colsupDiv != 'None'",
                                    fluidRow(
                                      box(width = 12,
                                          plotOutput(outputId = "pcahab", height = "800px")
                                          %>% withLoader(loader = "dnaspin")
                                        )
                                      ),
                                      fluidRow(
                                        box(width = 12,
                                            plotOutput(outputId = "pcahabi", height = "800px")
                                            %>% withLoader(loader = "dnaspin")
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
                                                    h4("Basic statistics per locus (hierfstat) :"),
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
                                  p("The genotype accumulation curve is generated by the Poppr package (R::poppr:: genotype_curve, https://doi.org/10.7717/peerj.281). 
t describes the genotypic diversity in relation to different combinations of TR loci, and is used to estimate the genotypic resolution of the MLVA scheme 
(further details: https://doi.org/10.1111/j.1471-8286.2006.01522.x). The curve is generated by sampling x loci randomly and counting the number of multilocus genotypes (MLG) observed. 
This sampling is repeated r times from 1 to n-1 loci, creating n-1 distributions of observed MLGs. The genotypic resolution is considered good when the curve reaches a plateau. "),
                                  plotOutput(outputId = "genocurve", height = "600px") %>% withLoader(loader = "dnaspin")
                           )
                  ),
                  tabPanel("Multilocus Genotype diversity (Poppr)", value=6,
                           conditionalPanel("input.Submitstat",
                                  box(width = 12,
                                      DT::dataTableOutput(outputId = "popprtab") %>% withLoader(loader = "dnaspin")
                                  )
                           )
                  )
#                  tabPanel("Minimum spanning networks in poppr", value=7,
#                           box(width = 12,
#                            plotOutput("plotMSN", height = '800px')
#                           ),
#                           conditionalPanel("output.plotMSN",
#                            box(width = 12,
#                                h3("SAVE PLOT"),
#                                    radioButtons("pdf_png", label = "Choose output filetype",
#                                                 choices = c("pdf", "png"),
#                                                 selected = "pdf",
#                                                 inline = TRUE),
#                                    conditionalPanel("input.pdf_png == 'pdf'",
#                                         numericInput("pdf_plot_width", "Width (in)",
#                                                      value = 7,
#                                                      step = 0.1,
#                                                      min = 1, 
#                                                      max = 20),
#                                         numericInput("pdf_plot_height", "Height (in)",
#                                                      value = 7,
#                                                      step = 0.1,
#                                                      min = 1, 
#                                                      max = 20),
#                                         downloadButton("save_pdf", "Save PDF", class = "btn-info")
#                                    ),
#                                    conditionalPanel("input.pdf_png == 'png'",
#                                                     numericInput("png_plot_width", "Width (px)",
#                                                                  value = 400,
#                                                                  min = 1, 
#                                                                  max = 5000),
#                                                     numericInput("png_plot_height", "Height (px)",
#                                                                  value = 400,
#                                                                  min = 1, 
#                                                                  max = 5000),
#                                                     numericInput("png_res", "Resolution (dpi)",
#                                                                  value = 300,
#                                                                  min = 72,
#                                                                  max = 2000,
#                                                                  step = 1),
#                                                     downloadButton("save_png", "Save PNG", class = "btn-info")
#                                    )
#                                 )
#                               )
#                  )
      )
    )
  )
)
