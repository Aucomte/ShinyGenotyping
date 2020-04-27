# Find cluster number : tabSnapClust

observeEvent(input$SCncluster,{
  sr$SCncluster = input$SCncluster
})

output$BIC_FC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$SCncluster,genind,IC="BIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="BIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})

output$AIC_FC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$SCncluster,genind,IC="AIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="AIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})

output$KIC_FC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$SCncluster,genind,IC="KIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="KIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})

# Get snapclust
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



