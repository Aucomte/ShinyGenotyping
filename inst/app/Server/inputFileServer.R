## Server File for the input file tab

observeEvent(input$file1,{
  sr$inputF= read.csv(input$file1$datapath, sep = sr$sep, dec=".", row.names=1)
})
observeEvent(input$genemapperfile,{
  sr$genemapperCSV=read.csv(input$genemapperfile$datapath, sep = sr$sep, dec=".")
})
observeEvent(input$repFile,{
  sr$repCSV=read.csv(input$repFile$datapath, sep = sr$sep, dec=".")
})
observeEvent(input$Metadata,{
  sr$metadataCSV=read.csv(input$Metadata$datapath, sep = sr$sep, dec=".")
})

#download file test
output$downloadData <- downloadHandler(
  filename = "datatest.zip",
  content = function(filename) {
    file.copy("www/datatest.zip", filename)
  },
  contentType = "application/zip"
)

## GET DATA ##S
getInput <- reactive({
  out <- NULL
  if(input$inputtype=="rep" && !is.null(input$file1)){
    myCSV = sr$inputF
  }
  else if(input$inputtype=="genemapper-output"){
    genemapperCSV3 <- sr$genemapperCSV[order(sr$genemapperCSV$Strain,sr$genemapperCSV$Marker),] 
    genemapperCSV2 = data.frame(Strain = genemapperCSV3$Strain,
                                Marker = genemapperCSV3$Marker,
                                Allele.1 = genemapperCSV3$Allele.1)
    
    #gestion des NA:
    verif.mark <- data.frame(ftable(Marker~Strain,data=genemapperCSV2))
    if (nrow(verif.mark[verif.mark$Freq!=1,])!=0){
      nb.echant <- verif.mark[verif.mark$Freq==0,]
      genemapperCSV2 <- rbind(genemapperCSV2,data.frame(nb.echant[,c("Sample.Name","Marker")],Allele.1=NA))
    }
    
    ##ON nomme les colonnes
    names(genemapperCSV2)<-c("Strain", "Locus", "Size")

    jeu <- matrix(as.vector(t(as.matrix(genemapperCSV2[,"Size"]))),nrow=nlevels(as.factor(genemapperCSV2$Strain)),ncol=nlevels(as.factor(genemapperCSV2$Locus)),byrow=TRUE)
    rownames(jeu) <- levels(as.factor(genemapperCSV2$Strain))
    nom.marqueur <- rep(levels(as.factor(genemapperCSV2$Locus)),each=1)
    
    jeu2<-data.frame(jeu) # Commandes à utiliser uniquement si on veut exporter le tableau avec les noms de locus. POur la suite par contre, il vaut mieux avoir un tableau sans tête de colonne.
    names(jeu2)<-levels(as.factor(genemapperCSV2$Locus))

    ##POr mettre la colonne "Strain" en premier:
    Strain<-rownames(jeu2)
    jeu2<-cbind(Strain,jeu2)
    
    myCSV = jeu2
    for(i in 2:ncol(jeu2)){
      for(j in 1:nrow(sr$repCSV)){
        if(colnames(jeu2[i]) == sr$repCSV[j,1] ){
          myCSV[,paste0(colnames(jeu2[i]),"_rep")] = ceiling((jeu2[,i]-sr$repCSV[j,3])/sr$repCSV[j,2])
        }
      }
    }
    # metadata
    if (!is.null(input$Metadata)){
      myCSV <- left_join(myCSV, sr$metadataCSV, by="Strain")
    }
    rownames(myCSV) = myCSV[,"Strain"]
    myCSV = myCSV[,colnames(myCSV) != "Strain"]
    #Strain<-rownames(myCSV)
    #myCSV<-cbind(Strain,myCSV)
    myCSV = as.data.frame(myCSV)
  }
  return(myCSV)
})

observeEvent(input$sep, {
  sr$sep = input$sep
})
observeEvent(
  input$submitGenemapperFiles, ignoreInit = TRUE,{
    myCSV <- getInput()
    sr$table = as.data.frame(myCSV)
    sr$colnames = colnames(sr$table)
    
    updateCheckboxGroupInput(session, "checkboxcol", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames)
    updateCheckboxGroupInput(session, "checkboxcolPCA", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames)

    updateSelectInput(session, "strata", choices = c("None",sr$colnames))
    updateSelectInput(session, "colsupDiv", choices = c("None",sr$colnames))
    
    updateSelectInput(session, "GroupComparison", choices = sr$colnames)
  })

output$GMdataset <- DT::renderDataTable(
  DT::datatable(
   sr$genemapperCSV, 
    filter = list(position = 'top', clear = TRUE, plain = FALSE), 
    options = list(
      scrollX = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
)

output$Metadataset <- DT::renderDataTable(
  DT::datatable(
    sr$metadataCSV, 
    filter = list(position = 'top', clear = TRUE, plain = FALSE), 
    options = list(
      scrollX = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
)

output$REPdataset <-  DT::renderDataTable(
  DT::datatable(
    sr$repCSV, 
    filter = list(position = 'top', clear = TRUE, plain = FALSE), 
    options = list(
      scrollX = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
)

output$fileUploaded <- reactive({
  return(!is.null(sr$table))
})

output$DataSetFinal <- DT::renderDataTable(server = FALSE,{
  DT::datatable(
    sr$table, 
    extensions = 'Buttons',
    filter = list(position = 'top', clear = TRUE, plain = FALSE), 
    options = list(
      scrollX = TRUE,
      dom = 'Blfrtip',
      buttons = list(
        'copy', 
        'print',
        list(
          extend = "collection", 
          text = "Download",
          action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('testDLtable', true, {priority: 'event'});}")
        )
      ),
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
})
output$downloadIF <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(sr$table, file, sep="\t", dec= ",", col.names = T, row.names = F)
  }
)
myModalIF <- function() {
  div(id = "testDLtable",
      modalDialog(downloadButton("downloadIF","Download as csv"),easyClose = TRUE, title = "Download Table")
  )
}
observeEvent(input$testDLtable, {
  showModal(myModalIF())
})


output$DataSet <- DT::renderDataTable(
  DT::datatable(
    sr$inputF, 
    filter = list(position = 'top', clear = TRUE, plain = FALSE), 
    options = list(
      scrollX = TRUE,
      dom = 'Blfrtip',
      lengthMenu = list( c(10, 20, -1), c(10, 20, "All")),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#3C3C3C', 'color': '#fff'});",
        "}"
      )
    )
  )
)
