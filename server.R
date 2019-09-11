options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
options(tinytex.verbose = TRUE)

#liste des réactifs

sr <- reactiveValues(
  datapath = getwd(),
  table = NULL,
  tableF = NULL,
  sep="\t",
  colnames = NULL,
  checkboxcol = NULL,
  colsupDiv = NULL,
  genindtype = NULL,
  haplotype_out = NULL,
  haplotypeloc_out = NULL,
  
  outpca = NULL,
  axeschoices = "axe1 vs axe2",
  checkboxcolPCA = NULL,
  
  archive = NULL,
  xvm.stat = NULL,
  stats = NULL,
  ploidy_number = 1,
  Genind = NULL,
  path = NULL,
  
  #Stat Tab variable
  drop = F,
  dropna = F,
  samplegeno = 10000,
  thresgeno = 0.95,
  submitBoolean = FALSE,
  
  #poppr
  strata = 1,
  samplepoppr= 1000,
  minsamp = 8,
  missingpopp = "mean",
  resume = NULL,
  
  #cluster
  
  nbclust = NULL,
  nbmaxclust = 50,
  nbpca = NULL,
  clusters = NULL
)

shinyServer(function(input, output, session) {

  #disabled tab on load:

  js$disableTab("t2")
  js$disableTab("t3")
  js$disableTab("t4")
  js$disableTab("t5")
  
  observe_helpers() # active help icon
  ## input dataset
  source(file.path("Server","inputFileServer.R"), local = TRUE)$value
  
  ## creation genind
  source(file.path("Server","genindServer.R"), local = TRUE)$value
  
  ### PCA
  source(file.path("Server","PCAServer.R"), local = TRUE)$value
  
  ###Statistics
  source(file.path("Server","StatServer.R"), local = TRUE)$value
  
  ###DAPC
  source(file.path("Server","DAPCServer.R"), local = TRUE)$value
  
}) # end shinyServer
