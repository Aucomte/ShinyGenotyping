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
