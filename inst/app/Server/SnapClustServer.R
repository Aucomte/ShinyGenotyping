# Get snapclust

observeEvent(input$SCncluster, ignoreInit = TRUE, {
  sr$SCncluster = input$SCncluster
})

getSnapclust <- function(){
  genind <- getData()
  x.clust<-snapclust(genind, k=sr$SCncluster)
  return(x.clust)
}

# group representation

observeEvent(input$SRstrata,{
  sr$SRstrata = input$SRstrata
})
observeEvent(input$GroupComparison,{
  sr$GroupComparison = input$GroupComparison
})
output$GroupPlot <- renderPlot({
  SC <- getSnapclust()
  genind <- getData()
  print(sr$GroupComparison)
  print(genind$other)
  print(genind$pop)
  table.value(table(SC$group, genind$other[,sr$GroupComparison]), col.labels=levels(as.factor(genind$other[,sr$GroupComparison])))
})
output$GroupPlotHM <- renderPlot({
  x <- getData()
  SC <- getSnapclust()
  Tab = as.matrix(table(SC$group, x$other[,input$GroupComparison]))
  Tab2 = matrix(Tab, nrow(Tab), ncol(Tab))
  colnames(Tab2) = colnames(Tab)
  rownames(Tab2) = rownames(Tab)
  col_fun = colorRamp2(c(0, max(Tab2)), c("yellow", "red"))
  Heatmap(Tab2, col = col_fun)
})


# compoplot

output$compoplotSC <- renderPlot({
  SC <- getSnapclust()
  compoplot(SC)
})



