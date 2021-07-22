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
                      "- Snapclust = This function implements methods for investigating the optimal number of genetic clusters ('k') 
                        using the fast maximum-likelihood genetic clustering approach described in Beugin et al (2018). 
                      The method runs snapclust for varying values of 'k', and computes the requested summary statistics for each clustering solution to assess goodness of fit.",
                      br(),
                      "- findclusters = cluster identification using successive K-means (adegenet)"
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
