# server for th find-cluster tab of DAPC

observeEvent(input$npca, {
  sr$findClus = input$npca
})

output$PCAclusterPlot <- renderPlot({
  plot(find.clusters(sr$Genind, n.pca=sr$findClus, choose.n.clust = FALSE)$Kstat, type="b", col="blue", ylab = "Kstat")
})

# Find cluster number : tabSnapClust

observeEvent(input$dapcnclusterT,{
  sr$dapcnclusterT = input$dapcnclusterT
})

output$dapcBIC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$dapcnclusterT,genind,IC="BIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="BIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})

output$dapcAIC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$dapcnclusterT,genind,IC="AIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="AIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})

output$dapcKIC <- renderPlot({
  genind <- getData()
  x.bic<-snapclust.choose.k(sr$dapcnclusterT,genind,IC="KIC")
  plot(x.bic, type="b", cex=2, xlab="k",ylab="KIC")
  points(which.min(x.bic), min(x.bic), col="blue", pch=20, cex=2)
  abline(v=which.min(x.bic), lty=2,col="red")
})
