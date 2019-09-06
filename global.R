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

#leaflet = carte interactive

#install.packages(c("shiny","shinythemes","shinyBS","stringr","shinydashboard","shinyjs","shinyWidgets","DT","shinyhelper","adegenet","poppr","plyr","FactoMineR","PopGenReport","hierfstat","pegas","colourpicker","shinyFeedback","shinyFiles","shinycssloaders","shinycustomloader"))


haplotypes <- function(col, colonnes, typehap, ploidy_number){
  #Import genind (adegenet)
  c.xvm<-df2genind(col[,colonnes], ploidy=ploidy_number, NA.char="0", type=typehap)
  c.xvm$other <- col.xvm[, names(col.xvm) != colonnes]
  
  ## Identification des haplotypes, par le package poppr
  mlg.xvm<-mlg(c.xvm) #compte le nombre de g?notypes multilocus
  liste.haplo<-mlg.id(c.xvm)# liste les genotypes multilocus et leurs ID correspondants (souches). Le r?sultat est une liste.
  
  ##COnversion de la liste d'haplo en dataframe (pour export) : par le package plyr
  haplo.xvm<-ldply(liste.haplo,data.frame) #ldply coupe la liste en morceaux et la rassemble en dataframe
  names(haplo.xvm)<-c("Haplotype","Strain")
return(haplo.xvm)
}

haplotypesLocus <- function(col, colonnes, haplo.xvm){
  x = matrix(nrow = nrow(haplo.xvm), ncol = length(colonnes)+1)
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
  genotypes<-tab[,c(colone,colonesup)] # we extract the genotypes and the columns "Origin"(Pays) and "hplotype_SNP"
  l = length(colone)
  mlva12<-PCA(genotypes, quali.sup=l+1) # correlations circle, to assess the contribution of variables (loci)
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
habillageind12 <- function(mlva12, colone){ 
  l = length(colone)
  h12 = plot.PCA(mlva12, axes=c(1,2), choix="ind", habillage=l+1)
  return(h12)
}
habillageind12inv <- function(mlva12, colone){ 
  l = length(colone)
  h12i = plot.PCA(mlva12, axes=c(1,2), choix="ind", invisible="ind", habillage=l+1)
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
habillageind13 <- function(mlva12, colone){ 
  l = length(colone)
  h13 = plot.PCA(mlva12, axes=c(1,3), choix="ind", habillage=l+1)
  return(h13)
}
habillageind13inv <- function(mlva12, colone){ 
  l = length(colone)
  h13i = plot.PCA(mlva12, axes=c(1,3), choix="ind", invisible="ind", habillage=l+1)
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
habillageind23 <- function(mlva12, colone){ 
  l = length(colone)
  h23 = plot.PCA(mlva12, axes=c(2,3), choix="ind", habillage=l+1)
  return(h23)
}
habillageind23inv <- function(mlva12, colone){ 
  l = length(colone)
  h23i = plot.PCA(mlva12, axes=c(2,3), choix="ind", invisible="ind", habillage=l+1)
  return(h23i)
}

#diversity 

diversitybyloc <- function(col.xvm, colone, colonesup){
  c.xvm<-df2genind(col.xvm[,colone],pop=as.factor(col.xvm[,colonesup]), ploidy=1, ncode=2, NA.char="NA", type="codom")
  now<-format(Sys.time(), "%b%d%H%M%S")
  div<-popgenreport(c.xvm, mk.counts = T, mk.differ.stats = T,mk.allele.dist=T, mk.null.all=T, mk.allel.rich=T, mk.pdf=T, foldername = now)
  c.xvm$other <- col.xvm[, names(col.xvm) != colone]
  system(paste("tar cvf ", now , " -C ", tempdir(), "/", now, " .", sep = ""))

  output = list()
  output$out1 = c.xvm
  output$path = now
  
  return(output)
}
