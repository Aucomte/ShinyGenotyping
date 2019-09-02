## Server File for the input file tab

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

    updateSelectInput(session, "strata", choices = sr$colnames)
    
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
