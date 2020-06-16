#kmeans
#BIC

#grp.m<-find.clusters(c.xvm, n.clust = NULL, max.n.clust = 100, n.pca = 50)

observeEvent(input$submitarchive, ignoreInit = TRUE, {
  sr$nbclust = input$Nclust
  sr$nbmaxclust = input$maxNclust
  sr$nbpca = input$Npca
  sr$clusters = find.clusters(c.xvm, n.clust = sr$nbclust, max.n.clust = sr$nbmaxclust, n.pca = sr$nbpca)
})