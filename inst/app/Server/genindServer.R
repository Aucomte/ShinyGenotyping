observeEvent(input$Submit, {
  sr$strata = input$strata
  #sr$ploidy_number = input$ploid
  sr$genindtype = input$genindtype
  sr$checkboxcol = input$checkboxcol
  sr$genindfilterbypop = input$genindfilterbypop
  
  # update PCA to adapt the already selected checkbox
  #updateSelectInput(session, "colsupDiv", choices = csr$colnames, selected=sr$strata)
  updateCheckboxGroupInput(session, "checkboxcolPCA", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames, selected=sr$checkboxcol)
  
  # Calculate 

  sr$Genind <- CreateGenindObject(sr$table, sr$checkboxcol, sr$strata, sr$genindtype, sr$ploidy_number)
  sr$haplotype_out = haplotypes(sr$Genind)
  sr$haplotypeloc_out = haplotypesLocus(sr$table, sr$checkboxcol, sr$haplotype_out)
  
  sr$filtergenindpop = input$filtergenindpop
  if (sr$genindfilterbypop == "yes"){
    gi = sr$Genind
    poptoremove = c()
    for(i in levels(gi@pop)) {
      if(length(gi@pop[sr$Genind@pop == i]) <= sr$filtergenindpop){
        poptoremove <- c(poptoremove, i)
      }
    }
    if (length(poptoremove) != 0){
    for(i in 1:length(poptoremove)) {
        pop = poptoremove[i]
        sr$table = sr$table[sr$table[sr$strata] != pop,]
      }
    }
    #gl = gi2gl(sr$Genind)
    #gl2 = gl.drop.pop(gl, pop.list = poptoremove)
    #sr$Genind = gl2gi(gl2)
    
    sr$Genind <- CreateGenindObject(sr$table, sr$checkboxcol, sr$strata, sr$genindtype, sr$ploidy_number)
    sr$poptoremove = poptoremove
    sr$haplotype_out = haplotypes(sr$Genind)
    sr$haplotypeloc_out = haplotypesLocus(sr$table, sr$checkboxcol, sr$haplotype_out)
  }
  updateSelectInput(session, "datasetMSN", choices = "genind", selected = "genind")
  
})

# texte nombre d'haplo / nombre d'individus
output$numberOfHaplo <- renderText({
  x = nrow(sr$haplotypeloc_out)
  paste("Number of haplotypes: ", x, sep = "")
})
output$numberOfind<- renderText({
  y = nrow(sr$haplotype_out)
  paste("Number of Individuals: ", y, sep = "")
})
# texte pop removed
output$populationremoved <- renderText({
  k = ""
  for (i in 1:length(sr$poptoremove)){k=paste(poptoremove[i], k, sep=" ")}
  paste("population removed because they have not enough individuals: ", k, sep = "")
})

output$genindStat<- renderPrint({
  sr$Genind
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

output$download <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(sr$haplotype_out, file, sep="\t", dec= ",", col.names = T, row.names = F)
  }
)
myModal <- function() {
  div(id = "test",
      modalDialog(downloadButton("download","Download as csv"),easyClose = TRUE, title = "Download Table")
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
output$download1 <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(sr$haplotypeloc_out, file, sep="\t", dec= ",", col.names = T, row.names = F)
  }
)
myModal1 <- function() {
  div(id = "test1",
      modalDialog(downloadButton("download1","Download as csv"),easyClose = TRUE, title = "Download Table")
  )
}
observeEvent(input$test1, {
  showModal(myModal1())
})
