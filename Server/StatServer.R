
archiveCreation <- observeEvent(input$submitarchive, ignoreInit = TRUE, {
  sr$archive = diversitybyloc(sr$Genind)
  sr$xvm.stat = genind2hierfstat(sr$Genind)
  print(sr$xvm.stat)
  sr$pairwisefst = as.matrix(genet.dist(sr$Genind, method="WC84"))
  X = allelic.richness(sr$xvm.stat,diploid=F)
  sr$richness = X$Ar
  sr$richnessMIN = X$min.all
  colnames(sr$richness) = levels(X$pop)
  
  if(sr$ploidy_number == 2){
    sr$stats =rbind(basic.stats(sr$Genind, diploid=T, digits=2)$perloc, rbind(basic.stats(sr$Genind, diploid=T, digits=2)$overall))
    rownames(sr$stats)[nrow(sr$stats)] = "Overall"
  }
  else{
    sr$stats =rbind(basic.stats(sr$Genind, diploid=F, digits=2)$perloc, rbind(basic.stats(sr$Genind, diploid=F, digits=2)$overall))
    rownames(sr$stats)[nrow(sr$stats)] = "Overall"
  }
  
  output$heatmapDiv <- renderPlot({
    info_table(sr$Genind, plot=TRUE)
  })
  
  output$genind2hierfstat<- DT::renderDataTable({
    DT::datatable(
      sr$xvm.stat,
      extensions = 'Buttons',
      rownames= FALSE,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
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
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test2', true, {priority: 'event'});}")
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
      write.table(sr$xvm.stat, file, sep="\t", dec= ",", col.names = T, row.names = F)
    }
  )
  myModal2 <- function() {
    div(id = "test2",
        modalDialog(downloadButton("download2","Download as csv"),easyClose = TRUE, title = "Download Table")
    )
  }
  observeEvent(input$test2, {
    showModal(myModal2())
  })
  
  output$pairwiseFST<- DT::renderDataTable({
    DT::datatable(
      sr$pairwisefst,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
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
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
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
      sr$stats,
      filter = list(position = 'top', clear = TRUE, plain = FALSE),
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
            action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
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
  output$download3 <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.table(sr$stats, file, sep="\t", dec= ",", col.names = T, row.names = F)
    }
  )
  myModal3 <- function() {
    div(id = "test3",
        modalDialog(downloadButton("download3","Download as csv"),easyClose = TRUE, title = "Download Table")
    )
  }
  observeEvent(input$test3, {
    showModal(myModal3())
  })
})


observeEvent(input$Submitcurve,{
  sr$drop = input$drop
  sr$dropna = input$dropna
  sr$thresgeno = input$thresgeno
  sr$samplegeno = input$samplegeno
       output$genocurve <- renderPlot({
           genotype_curve(sr$Genind, sample=sr$samplegeno, drop=sr$drop, dropna=sr$dropna, thresh=sr$thresgeno) + theme_bw() + geom_smooth()
       })
})


observeEvent(input$Submitstat,{
     #nameStrata(sr$Genind) <-~sr$strata
     sr$samplepoppr = input$samplepoppr
     sr$minsamp = input$minsamp
     sr$missingpopp = input$missingpopp
    sr$resume<-poppr(sr$Genind, missing=sr$missingpopp, sample=sr$samplepoppr, minsamp=sr$minsamp, strata=sr$strata, plot=T, legend=T)
   
     output$popprplot <- renderPlot({
       sr$resume
     })
     
     output$popprtab <- DT::renderDataTable({
       DT::datatable(
         sr$resume,
         filter = list(position = 'top', clear = TRUE, plain = FALSE),
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
               action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
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
})


#download tar
output$downloadDiv <- downloadHandler(
  filename = function() {
    paste("output", "zip", sep=".")
  },
  content = function(file) {
    file.copy(sr$archive$path, file)
  },
  contentType = "application/zip"
)

# MST

output$MSTpop <- renderPlot(
  aboot(x = sr$Genind, strata=sr$Genind@pop)
)
output$MSTind <- renderPlot(
  aboot(x = sr$Genind, strata=NULL)
)

output$AllelicRichness<- DT::renderDataTable({
  DT::datatable(
    sr$richness,
    filter = list(position = 'top', clear = TRUE, plain = FALSE),
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
          action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('test3', true, {priority: 'event'});}")
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

output$AllelicRichnessMIN <- renderText(
  sr$richnessMIN
)
