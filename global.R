require(shiny)
require(shinythemes)
require(shinyBS)
require(stringr)
require(shinydashboard)
require(shinyjs)
require(shinyWidgets)
require(DT)
require(shinyhelper)

library(adegenet)
library(poppr)
library(plyr)
library(FactoMineR)
library(PopGenReport)
library(hierfstat)
library(pegas)
require(colourpicker) #couleur selecteur
require(shinyFeedback) #met des warning aux inputs

library(shinyFiles)
library(shinycssloaders)
library(shinycustomloader)

library(ComplexHeatmap)
library(circlize)

library(ggplot2)
library(dplyr)

#leaflet = carte interactive

# install.packages(c("shiny","shinythemes","shinyBS","stringr","shinydashboard","shinyjs","shinyWidgets","DT","shinyhelper",
#                    "adegenet","poppr","plyr","FactoMineR","PopGenReport","hierfstat","pegas","colourpicker","shinyFeedback","shinyFiles",
#                    "shinycssloaders","shinycustomloader"))

CreateGenindObject <- function(col.xvm, colone, colonesup, typehap, ploidy_number){
  if (colonesup != "None"){
    c.xvm<-df2genind(col.xvm[,colone],pop=as.factor(col.xvm[,colonesup]), ploidy=ploidy_number, ncode=2, NA.char="NA", type=typehap)
    c.xvm$other <- col.xvm[, !(names(col.xvm) %in% colone)]
    #strata(c.xvm)<-genind2df(c.xvm$other)[colonesup]
  }
  else {
    c.xvm<-df2genind(col.xvm[,colone],pop=NULL, ploidy=ploidy_number, ncode=2, NA.char="NA", type=typehap)
    c.xvm$other <- col.xvm[, !(names(col.xvm) %in% colone)]
  }
  return(c.xvm)
}

haplotypes <- function(c.xvm){

  ## Identification des haplotypes, par le package poppr
  mlg.xvm<-mlg(c.xvm) #compte le nombre de g?notypes multilocus
  liste.haplo<-mlg.id(c.xvm)# liste les genotypes multilocus et leurs ID correspondants (souches). Le r?sultat est une liste.
  
  ##COnversion de la liste d'haplo en dataframe (pour export) : par le package plyr
  haplo.xvm<-ldply(liste.haplo,data.frame) #ldply coupe la liste en morceaux et la rassemble en dataframe
  names(haplo.xvm)<-c("Haplotype","Strain")
return(haplo.xvm)
}

haplotypesLocus <- function(col, colonnes, haplo.xvm){
  x = matrix(nrow = nrow(col), ncol = length(colonnes)+1)
  colnames(x) =  c("Haplotypes", colonnes)
  for(i in 1:nrow(col)){
    for(i2 in 1:nrow(haplo.xvm)){
      if(as.character(rownames(col)[i]) == as.character(haplo.xvm[i2,2])){
        x[i,1] = haplo.xvm[i2,1]
        for(j in 1:ncol(col)){
          for(j2 in 2:ncol(x)){
            if(as.character(colnames(col)[j]) == as.character(colnames(x)[j2])){
              x[i,j2] <- col[i,j]
            }
         }
       }
     }
    }
  }
  y = unique(x)
  return(y)
}

#PCA

calculPCA <- function(tab, colone, colonesup){
  listpca = list()
  if(colonesup != "None"){
    genotypes<-tab[,c(colone,colonesup)] 
    l = length(colone)
    mlva12<-PCA(genotypes, quali.sup=l+1)
  }
  else{
    genotypes<-tab[,colone] 
    l = length(colone)
    mlva12<-PCA(genotypes, quali.sup=l)
  }
  return(mlva12)
}
#1v2
plotind12 <- function(mlva12){ 
  pi12 = plot.PCA(mlva12, axes=c(1,2), choix = "ind")
  return(pi12)
}
plotvar12 <- function(mlva12){ 
  pv12 = plot.PCA(mlva12, axes=c(1,2), choix = "var")
  return(pv12)
}
habillageind12 <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h12 = plot.PCA(mlva12, axes=c(1,2), choix="ind", habillage=l+1)
  }
  return(h12)
}
habillageind12inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h12i = plot.PCA(mlva12, axes=c(1,2), choix="ind", invisible="ind", habillage=l+1)
  }
  return(h12i)
}
#1v3
plotind13 <- function(mlva12){ 
  pi13 = plot.PCA(mlva12, axes=c(1,3), choix = "ind")
  return(pi13)
}
plotvar13 <- function(mlva12){ 
  pv13 = plot.PCA(mlva12, axes=c(1,3), choix = "var")
  return(pv13)
}
habillageind13 <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h13 = plot.PCA(mlva12, axes=c(1,3), choix="ind", habillage=l+1)
  }
  return(h13)
}
habillageind13inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h13i = plot.PCA(mlva12, axes=c(1,3), choix="ind", invisible="ind", habillage=l+1)
  }
  return(h13i)
}
#2v3
plotind23 <- function(mlva12){ 
  pi23 = plot.PCA(mlva12, axes=c(2,3), choix = "ind")
  return(pi23)
}
plotvar23 <- function(mlva12){ 
  pv23 = plot.PCA(mlva12, axes=c(2,3), choix = "var")
  return(pv23)
}
habillageind23 <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h23 = plot.PCA(mlva12, axes=c(2,3), choix="ind", habillage=l+1)
  }
  return(h23)
}
habillageind23inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h23i = plot.PCA(mlva12, axes=c(2,3), choix="ind", invisible="ind", habillage=l+1)
  }
  return(h23i)
}

#diversity 

diversitybyloc <- function(c.xvm){

  now<-format(Sys.time(), "%b%d%H%M%S")
  dirT = tempdir()
  div<-popgenreport(c.xvm, mk.counts = T, mk.differ.stats = T, mk.allele.dist=T, mk.null.all=T, mk.allel.rich=T, mk.pdf=F, path.pgr=dirT,foldername="Allelic_Freq_RES")
  
  output = list()
  system(paste("tar cvf ", dirT,"/",now,".tar ", "-C ", dirT, "/Allelic_Freq_RES .", sep=""))
  output$path = paste(dirT,"/",now,".tar", sep="")
  #output$path = paste(dirT,"/Allelic_Freq_RES .", sep="")
  print(output$path)
  return(output)
}


