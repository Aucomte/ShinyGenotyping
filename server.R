options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

server <-function(input,output,session){
  
  observe_helpers() # active help icon
  
  #liste des réactifs
  
  sr <- reactiveValues(
    datapath = getwd(),
    table = NULL,
    tableF = NULL,
    sep="\t",
    colnames = NULL,
    checkboxcol = NULL,
    genindtype = NULL,
    haplotype_out = NULL,
    haplotypeloc_out = NULL,
    checkboxpca = NULL,
    checkboxpcasup = NULL,
    outpca = NULL,
    axeschoices = "axe1 vs axe2",
    checkboxcolDiv  = NULL,
    colsupDiv = NULL,
    archive = NULL,
    xvm.stat = NULL,
    ploidy_number = 1
  )
  
  ## input dataset
  source(file.path("Server", "inputFileServer.R"), local = TRUE)$value
   
   ## creation genind
  source(file.path("Server", "genindServer.R"), local = TRUE)$value
  
   ### PCA
  source(file.path("Server", "PCAServer.R"), local = TRUE)$value
  
   ###Diversité par locus
  source(file.path("Server", "StatServer.R"), local = TRUE)$value
   
}