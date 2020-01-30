tabItem(
  tabName ="DAPC",
  
  pageWithSidebar(
    headerPanel("DAPC Analysis"),
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
      
      sliderInput("npca", "Number of PCA axes retained:", min=1, max=1000, value=10),
      
      conditionalPanel(condition="input.tabDAPC != '20'",
                       
         ## select number of DA axes
         ##sliderInput("nda", "Number of discriminant functions retained:", min=1, max=100, value=1),
         uiOutput("nda"), 
             
        radioButtons("clusters", "Number of clusters to use?",
                     list("Choose a number of clusters"="num","Clusters are genind object pop"="pop")),
        
        ## nclust
        conditionalPanel(condition="input.clusters == 'num'", 
                         sliderInput("nclust",
                                     "Number of clusters:",
                                     min = 1,
                                     max = 100,
                                     value = 3)
        )


      ),


      conditionalPanel(condition="input.tabDAPC == '21'",
                       
                       h3("Graphical parameters"),
                       
                       ## select first axis to plot
                       ##numericInput("xax", "Indicate the x axis", value=1, min=1),
                       uiOutput("xax"),
                       
                       ## select second axis to plot
                       ##numericInput("yax", "Indicate the y axis", value=1, min=1),
                       uiOutput("yax"),
                       
                       h3("Aesthetics"),
                       
                       ## select color palette
                       selectInput("col.pal", "Indicate a color palette to be used", choices=c("funky","spectral","seasun","azur","wasp")),
                       
                       ## select transparency
                       sliderInput("alpha", "Choose transparency", min=0, max=1, step=0.05, value=0.5),
                       
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


      # Select Output variable:
       #conditionalPanel(condition="input.tabDAPC != '25'", 
       #                 checkboxInput("useoptimnpca", "Use suggested number of PCA components?", FALSE)


      ## input specific of compoplot tab
      conditionalPanel(condition="input.tabDAPC=='23'",
                       h3("Aesthetics"),
                       selectInput("col.pal", "Indicate a color palette to be used", choices=c("funky","spectral","seasun","azur","wasp")),
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
                       
      ),
      
      ## CROSS-VALIDATION
      # n.pca.max slider
      conditionalPanel(condition="input.tabDAPC=='25'",  
                       h3("Cross-validation"),
                       uiOutput("doxval"),
                       uiOutput("npcaMax"),
                       ## nrep slider
                       sliderInput("nrep",
                                   "Number of replicates:",
                                   min = 1,
                                   max = 100,
                                   value = 3),
                       ## trainingset slider
                       sliderInput("trainingset",
                                   "Training set size:",
                                   min = 0.1,
                                   max = 0.95,
                                   value = 0.9,
                                   step = 0.01),
                       ## result type
                       radioButtons("result", "Assess by:",
                                    list("Group" = "groupMean",
                                         "Overall" = "overall"))
      ),
      
      
      conditionalPanel(condition="input.tabDAPC == '26'",
                       selectInput("groupslider", "Choose group:",
                                   choices="")
      )
    ), # end sidebarPanel
    
    ## MAIN PANEL
    mainPanel(
      tabsetPanel(id = "tabDAPC",
                  tabPanel("FindClusters", value=20,
                           plotOutput("PCAclusterPlot")
                           ),
                  tabPanel("Scatterplot", value=21,
                           plotOutput("scatterplot",height = "600px"),
                           h3(br(),"Scatter Plot"),
                           p("The Scatter Plot page provides a visual assessment of between-population differentiation.
                             Generated by applying the R function scatterplot to a dapc object, the output generated will appear in one of two forms.
                             If only one DA is retained (always the case if there are only 2 groups),
                             or both the x-axis and y-axis of the scatterplot are set to the same value, the output will display the
                             densities of individuals on the given discriminant function.
                             If more than one DA is retained and selected, the output will display individuals as dots and
                             groups as inertia ellipses, and will represent the relative position of each along the two
                             selected axes."),
                           
                           p("The number of axes retained in both the PCA and DA steps of DAPC will have an impact on the
                             analysis and affect the scatter plot. By default, the number of DA axes retained is set at
                             the maximum of (K - 1) axes, where K is the number of groups. The default value of the number of
                             PCA axes is more arbitrarily defined, however, the 'Use suggested number of PCA components?'
                             tickbox provides the user with the option to use cross-validation to identify and select an optimal number
                             of PCs, where one exists. For more on this, see the section on cross-validation."),
                           
                           p("There are a wide variety of graphical parameters for the DAPC scatterplot that can be customised
                             by the user. Those parameters that lack intuitive definition are described further in the Glossary.")
                           ),
                  tabPanel("Summary", value=22, 
                           verbatimTextOutput("summary"),
                           h3(br(),"Summary"),
                           p("This page provides a summary of the dapc object."),
                           p("$n.dim' indicates the number of retained DAPC axes, which is affected by both the number of PCA
                             axes and DA axes retained."),
                           p("'$n.pop' indicates the number of groups or populations, which is defined by the dataset."),
                           p("'$assign.prop' indicates the proportion of overall correct assignment"),
                           p("'$assign.per.pop' indicates the proportions of successful reassignment (based on the discriminant
                             functions) of individuals to their original clusters. Large values indicate clear-cut clusters, while low
                             values suggest admixed groups."),
                           p("'$prior.grp.size' indicates prior group sizes."),
                           p("'$post.grp.size' indicates posterior group sizes.")
                           ),
                  tabPanel("Compoplot", value=23, 
                           plotOutput("compoplot",height = "600px"),
                           h3(br(),"Compoplot"),
                           p("This page displays a compoplot, which is a bar plot showing the probabilities of assignment of
                             individuals to the different clusters. Individuals are plotted along the x-axis and membership probabilities are
                             plotted along the y-axis.From the compoplot, one can draw inferences about potential
                             admixture, and about the way in which the selection of PCA axes affects the stability of membership probabilities.")
                           ),
                  tabPanel("AssignPlot", value=231,
                           plotOutput("assignplot",height = "900px")
                  ),
                  tabPanel("Loading Plot", value=24, 
                           plotOutput("loadingplot",height = "600px"),
                           conditionalPanel(condition = "input.FS==1",
                                            h3("Number of selected vs. unselected alleles"),
                                            verbatimTextOutput("FS1"),
                                            h3("List of selected alleles"),
                                            verbatimTextOutput("FS2"),
                                            h3("Names of selected alleles"),
                                            verbatimTextOutput("FS3"),
                                            h3("Contributions of selected alleles to discriminant axis"),
                                            verbatimTextOutput("FS4")),
                           h3(br(), "Loading Plot"),
                           p("The Loading Plot page allows the user to examine how the original variables contribute to the
                             discriminant functions created by DAPC. Variables are plotted along the x-axis, and the contribution
                             of those variables to the DAPC is plotted in the y-axis."),
                           p("The side panel on the Loading Plot page provides the option of selecting a threshold above which variables are identified.
                             This can be useful simply for clarifying the image; hence, by default, only variables above the third quartile threshold are labelled.
                             A drop-down menu contains a variety of clustering methods that can also be used to set this threshold.
                             If desired, the user can choose to 'Select and describe features above the threshold'")
                           ),
                  
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
                           verbatimTextOutput("xvalResults2"),
                           h3(br(),"Cross-validation"),
                           p("When the 'Perform cross-validation?' box is ticked, this optimisation procedure will be carried
           out on the Cross-validation page."),
                           p("Cross-validation is an optimisation procedure that is used in the context of DAPC to identify the number of principal components
           that gives rise to the model with the highest predictive capacity. In cross-validation for DAPC, the data is divided into a training
           set and a validation set (by default, comprising 90% and 10% of the data, respectively). The analysis is run on the training set with
           variable numbers of PCs retained, and the degree to which the analysis is able to accurately predict the group membership of
           excluded individuals (those in the validation set) is used to select the optimal number of PCs to retain. This procedure is replicated
           with different random sub-samples a number of times specified by a slider on the side panel. In  the interest of computational time,
           only 3 replicates are performed by default, though more replicates are recommended to achieve greater optimisation.
           Success is calculated either by group (the default) or measured as overall success."),
                           
                           p("A scatterplot of the results is displayed, showing the number of PCs retained on the x-axis and
           success on the y-axis. Individual replicates appear as dots, and the density of points is displayed
           in blue."),
                           p("Ideally, the data should fall in an arc, indicating an optimal point at its maximum where
           the number of PCs retained leads to better predictive success than numbers of PCs either
           above or below."),
                           p("Below the plot, a variety of summary statistics are provided.
           Ultimately, it is the number of PCs associated with the lowest RMSE (root mean squared error,
           see Glossary) which is selected if 'Use suggested number of PCA components?' is ticked.",br(),br(),br())
                  ),
                  tabPanel("Group Representation", value=26,
                           plotOutput("representationplot"),
                           plotOutput("representationHM")
                  )
                  
                           ) # end tabsetPanel
                           ) # end mainPanel
                  ) # end pageWithSidebar
    ) # end shinyUI

