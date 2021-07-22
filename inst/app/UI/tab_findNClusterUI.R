tabItem(
  tabName ="FindK",
  pageWithSidebar(
    headerPanel("Find K : the number of clusters"),
    ## SIDE PANEL CONTENT
    sidebarPanel(
      ## define the type of input
      # p("Input = Genind calculated previously"),
      # p("mport genind = Genind imported (Rdata)"),
      # radioButtons("datatype", "What data source to use?",
      #              list("Input"="expl","Import genind"="file")),
      # 
      # ## choice of dataset if source is an example
      # conditionalPanel(condition = "input.datatype=='expl'",
      #                  selectInput("dataset", "The dataset", choices="Input")
      # ),
      # 
      # ## choice of dataset if source is a file
      # conditionalPanel(condition = "input.datatype=='file'",
      #                  fileInput('datafile', 'Choose input file',
      #                            accept=c('gtx/gen/dat/GTX/GEN/DAT/RData/Rdata/Rda/rda', 'GENETIX/genepop/Fstat/R data')),
      #                  tags$hr()
      # ),
      
      conditionalPanel(condition="input.FindKtab == '121'",
             sliderInput("dapcnclusterT", "Number of cluster max tested:", min=1, max=100, value=10, step = 1)
      ),
      conditionalPanel(condition="input.FindKtab == '122'",
        sliderInput("npca", "Number of PCA axes retained:", min=1, max=1000, value=10)
      )
    ),
  mainPanel(
  tabsetPanel(id = "FindKtab",
              tabPanel("Snapclust", value=121,
                  box(width = 12, class="box2", 
                      "Two methods to select the right number of clusters for DAPC clusterisation:",
                      br(),
                      "- Snapclust is a fast maximum-likelihood method, combining the advantages of both model-based and geometric approaches (Beugin et al., 2018). The optimal number of clusters (k) are estimated using both the Akaike, Kullback and Bayesian Information Criterion (AIC, KIC, BIC, respectively). Ten runs of the Expectation-Maximisation (EM) algorithm are advised to estimate an accurate K and the probability of assignment (Q) of each individual into each of the k inferred.",
                      br(),
                      "- The function find.clusters runs successive k-means clustering with increasing number of clusters (k) and the optimal number of clusters is selected based on lowest Bayesian information criterion (BIC) (Jombart et al., 2010). 10 to 20 runs are advised to estimate an accurate K.",
                      br(),
                      "References:",
                      br(),
                      "Beugin, M.-P., Gayet, T., Pontier, D., Devillard, S. and Jombart, T. (2018) A fast likelihood solution to the genetic clustering problem. Methods in Ecology and Evolution, 9, 4. doi: 10.1111/2041-210X.12968.",
                      br(),                      
                      "Jombart, T., Devillard, S. and Balloux, F. (2010) Discriminant analysis of principal components: a new method for the analysis of genetically structured populations. BMC Genet., 11, 94. doi: 10.1186/1471-2156-11-94."
                      ),
                      box(width = 12, class="box2", 
                          "Ideally, the lowest AIC/BIC corresponds to the best model."
                          ),
                  box(width = 12,plotOutput("dapcBIC") %>% withLoader(loader = "dnaspin")),
                  box(width = 12,plotOutput("dapcAIC") %>% withLoader(loader = "dnaspin")),
                  box(width = 12,plotOutput("dapcKIC") %>% withLoader(loader = "dnaspin"))
              ),
              tabPanel("FindClusters", value=122,
                  plotOutput("PCAclusterPlot") %>% withLoader(loader = "dnaspin")
              )
      )
    )
  )
)
