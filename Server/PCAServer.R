## PCA server

observeEvent(input$Submitpca, ignoreInit = TRUE, {
  sr$axeschoices = input$axeschoices
  sr$checkboxcolPCA = input$checkboxcolPCA
  sr$colsupDiv = input$colsupDiv
  sr$outpca = calculPCA(sr$table, sr$checkboxcolPCA, sr$colsupDiv)
  #updateCheckboxGroupInput(session, "checkboxcol", inline = TRUE, choiceNames = sr$colnames, choiceValues = sr$colnames, selected = sr$checkboxcolPCA)
  #updateSelectInput(session, "strata", choices = sr$colnames, selected = sr$colsupDiv)
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
      habillageind12(sr$outpca, sr$checkboxcolPCA)
    }
    else if(sr$axeschoices == "axe1 vs axe3"){
      habillageind13(sr$outpca, sr$checkboxcolPCA)
    }
    else if(sr$axeschoices == "axe2 vs axe3"){
      habillageind23(sr$outpca, sr$checkboxcolPCA)
    }
  })
  output$pcahabi <- renderPlot({
    if(sr$axeschoices == "axe1 vs axe2"){
      habillageind12inv(sr$outpca, sr$checkboxcolPCA)
    }
    else if(sr$axeschoices == "axe1 vs axe3"){
      habillageind13inv(sr$outpca, sr$checkboxcolPCA)
    }
    else if(sr$axeschoices == "axe2 vs axe3"){
      habillageind23inv(sr$outpca, sr$checkboxcolPCA)
    }
  })
})

