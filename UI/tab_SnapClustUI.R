tabItem(
  tabName ="SnapClust",
  pageWithSidebar(
    headerPanel("SnapClust Analysis"),
    ## SIDE PANEL CONTENT
    sidebarPanel(
        ## define the type of input
        radioButtons("datatype", "What data source to use?",
                     list("Input"="expl","Input file"="file")),
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
        h3("SNAPCLUST PARAMETER"),
        conditionalPanel(condition="input.tabSnapClust != '31'",
          sliderInput("SCncluster", "Number of cluster:", min=1, max=100, value=10, step = 1)
        ),
        conditionalPanel(condition="input.tabSnapClust == '32'",
            selectInput("GroupComparison", "group for comparison", choices = "")
        )
      ),
    ## MAIN PANEL CONTENT
    mainPanel(
      tabsetPanel(id = "tabSnapClust", 
         tabPanel("Compoplot",value=33,
          plotOutput("compoplotSC")
         ),
         tabPanel("Group Representation",value=32,
          plotOutput("GroupPlot"),
          plotOutput("GroupPlotHM")
         )
      )
    )
  )
)
