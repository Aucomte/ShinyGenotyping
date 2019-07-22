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
