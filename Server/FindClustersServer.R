# server for th find-cluster tab of DAPC

observeEvent(input$npca, {
  sr$findClus = input$npca
})

output$PCAclusterPlot <- renderPlot({
  plot(find.clusters(sr$Genind, n.pca=sr$findClus, choose.n.clust = FALSE)$Kstat, type="b", col="blue", ylab = "Kstat")
})