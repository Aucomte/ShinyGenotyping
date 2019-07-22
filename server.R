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
  
  observeEvent(input$sep, {
    sr$sep = input$sep
  })
  observeEvent(c(
    input$file1,
    sr$sep), ignoreInit = TRUE,{
      myCSV <- reactiveFileReader(100, session, input$file1$datapath, read.csv, sep = sr$sep, dec=".", row.names=1) #fill = TRUE ? 
      sr$table = as.data.frame(myCSV())
      
      
      sr$colnames = colnames(sr$table)
      
      updateCheckboxGroupInput(session, "checkboxcol", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames)
      updateCheckboxGroupInput(session, "checkboxpca", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames)
      updateSelectInput(session, "checkboxpcasup", choices = sr$colnames)
      
      updateCheckboxGroupInput(session, "checkboxcolDiv", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames)
      updateSelectInput(session, "colsupDiv", choices = sr$colnames)
    })
  
  output$fileUploaded <- reactive({
    return(!is.null(sr$table))
  })
  
   output$DataSet <- DT::renderDataTable(
    DT::datatable(
      sr$table, 
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
   
   ## creation genind
   observeEvent(input$Submit, {
     sr$ploidy_number = input$ploid
     sr$checkboxcol = input$checkboxcol
     updateCheckboxGroupInput(session, "checkboxpca", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames, selected = sr$checkboxcol)
     updateCheckboxGroupInput(session, "checkboxcolDiv", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames, selected = sr$checkboxcol)
     sr$genindtype = input$genindtype
     sr$haplotype_out = haplotypes(sr$table, sr$checkboxcol, sr$genindtype, sr$ploidy_number)
     sr$haplotypeloc_out = haplotypesLocus(sr$table, sr$checkboxcol, sr$haplotype_out)
   })
   
   #output tabhaplo
   output$tabhaplo <- DT::renderDataTable(server = FALSE,{
     DT::datatable(
       sr$haplotype_out, 
       extensions = 'Buttons', 
       rownames= FALSE,
       options = list(
         scrollX=TRUE,
         dom = 'Blfrtip', 
         buttons = list(
           'copy', 
           'print',
           list(
             extend = "collection", 
             text = "Download entire dataset",
             #buttons = c("csv","excel","pdf"),
             action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test', true, {priority: 'event'});}")
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
   
   output$numberOfHaplo <- renderText({
     x = nrow(sr$haplotypeloc_out)
     paste("In this dataset there is ", x, " haplotypes", sep = "")
   })
   
   output$download1 <- downloadHandler(
     filename = function() {
       paste("data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.table(sr$haplotype_out, file, sep="\t", dec= ",", col.names = T, row.names = F)
     }
   )
   myModal <- function() {
     div(id = "test",
         modalDialog(downloadButton("download1","Download as csv"),easyClose = TRUE, title = "Download Table")
     )
   }
   observeEvent(input$test, {
     showModal(myModal())
   })
   
   #output tabhaploloc
   output$tabhaploloc <- DT::renderDataTable(server = FALSE,{
     DT::datatable(
       sr$haplotypeloc_out, 
       extensions = 'Buttons', 
       options = list(
         scrollX=TRUE,
         dom = 'Blfrtip', 
         buttons = list(
           'copy', 
           'print',
           list(
             extend = "collection", 
             text = "Download entire dataset",
             #buttons = c("csv","excel","pdf"),
             action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test1', true, {priority: 'event'});}")
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
   output$download2 <- downloadHandler(
     filename = function() {
       paste("data-", Sys.Date(), ".csv", sep="")
     },
     content = function(file) {
       write.table(sr$haplotypeloc_out, file, sep="\t", dec= ",", col.names = T, row.names = F)
     }
   )
   myModal2 <- function() {
     div(id = "test1",
         modalDialog(downloadButton("download2","Download as csv"),easyClose = TRUE, title = "Download Table")
     )
   }
   observeEvent(input$test, {
     showModal(myModal2())
   })
   
   ### PCA
   
   observeEvent(input$Submitpca, ignoreInit = TRUE, {
     sr$checkboxpca = input$checkboxpca
     sr$checkboxpcasup = input$checkboxpcasup
     updateSelectInput(session, "colsupDiv", choices = sr$colnames, selected=sr$checkboxpcasup)
     sr$axeschoices = input$axeschoices
     sr$outpca = calculPCA(sr$table, sr$checkboxpca, sr$checkboxpcasup)
     output$pcaInd <- renderPlot({
       if(sr$axeschoices == "axe1 vs axe2"){
         plotind12(sr$outpca)
       }
       else if(sr$axeschoices == "axe1 vs axe3"){
         plotind13(sr$outpca)
       }
       else if(sr$axeschoices == "axe2 vs axe3"){
         plotind23(sr$outpca)
       }
     })
     output$pcaVar <- renderPlot({
       if(sr$axeschoices == "axe1 vs axe2"){
         plotvar12(sr$outpca)
       }
       else if(sr$axeschoices == "axe1 vs axe3"){
         plotvar13(sr$outpca)
       }
       else if(sr$axeschoices == "axe2 vs axe3"){
         plotvar23(sr$outpca)
       }
     })
     output$pcahab <- renderPlot({
       if(sr$axeschoices == "axe1 vs axe2"){
         habillageind12(sr$outpca, sr$checkboxpca)
       }
       else if(sr$axeschoices == "axe1 vs axe3"){
         habillageind13(sr$outpca, sr$checkboxpca)
       }
       else if(sr$axeschoices == "axe2 vs axe3"){
         habillageind23(sr$outpca, sr$checkboxpca)
       }
     })
     output$pcahabi <- renderPlot({
       if(sr$axeschoices == "axe1 vs axe2"){
         habillageind12inv(sr$outpca, sr$checkboxpca)
       }
       else if(sr$axeschoices == "axe1 vs axe3"){
         habillageind13inv(sr$outpca, sr$checkboxpca)
       }
       else if(sr$axeschoices == "axe2 vs axe3"){
         habillageind23inv(sr$outpca, sr$checkboxpca)
       }
     })
   })

   ###Diversité par locus
   
   archiveCreation <- observeEvent(input$submitarchive, ignoreInit = TRUE, {
     sr$checkboxcolDiv  = input$checkboxcolDiv
     sr$colsupDiv = input$colsupDiv
     sr$archive = diversitybyloc(sr$table, sr$checkboxcolDiv, sr$colsupDiv)
     sr$xvm.stat = genind2hierfstat(sr$archive$out1)
     print(sr$xvm.stat)
     output$heatmapDiv <- renderPlot({
       info_table(sr$archive$out1, plot=TRUE)
     })
     
     output$genind2hierfstat<- DT::renderDataTable({
       DT::datatable(
         sr$xvm.stat,
         extensions = 'Buttons', 
         options = list(
           scrollX=TRUE,
           dom = 'Blfrtip', 
           buttons = list(
             'copy', 
             'print',
             list(
               extend = "collection", 
               text = "Download entire dataset",
               #buttons = c("csv","excel","pdf"),
               action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test1', true, {priority: 'event'});}")
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
     output$genostatbasePerLoc <- DT::renderDataTable({
       DT::datatable(
         if(sr$ploidy_number == 2){
           basic.stats(sr$xvm.stat, diploid=T, digits=2)$perloc
         }
         else{
           basic.stats(sr$xvm.stat, diploid=F, digits=2)$perloc
         },
         extensions = 'Buttons', 
         options = list(
           scrollX=TRUE,
           dom = 'Blfrtip', 
           buttons = list(
             'copy', 
             'print',
             list(
               extend = "collection", 
               text = "Download entire dataset",
               #buttons = c("csv","excel","pdf"),
               action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test1', true, {priority: 'event'});}")
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
     
     output$genocurve <- renderPlot({
       geomc = genotype_curve(sr$archive$out1, sample=10000, drop=FALSE, dropna=FALSE, thresh=0.95) + theme_bw() 
       geomc + geom_smooth()
       #arguments en option: drop (non prise en compte des loci monomorphes), dropna (non prise en compte des donn?es manquantes) 
     })
   })

   output$downloadDiv <- downloadHandler(
     filename <- function() {
       paste("output", "zip", sep=".")
     },
     content <- function(file) {
       file.copy(sr$archive$path, file)
     },
     contentType = "application/zip"
   )
   
}