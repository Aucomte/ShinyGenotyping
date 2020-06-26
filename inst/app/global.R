require(shiny, quietly=TRUE, warn.conflicts = FALSE)
require(shinythemes, quietly=TRUE, warn.conflicts = FALSE)
require(shinyBS, quietly=TRUE, warn.conflicts = FALSE)
require(stringr, quietly=TRUE, warn.conflicts = FALSE)
require(shinydashboard, quietly=TRUE, warn.conflicts = FALSE)
require(shinyjs, quietly=TRUE, warn.conflicts = FALSE)
require(shinyWidgets, quietly=TRUE, warn.conflicts = FALSE)
require(DT, quietly=TRUE, warn.conflicts = FALSE)
require(shinyhelper, quietly=TRUE, warn.conflicts = FALSE)

library(adegenet, quietly=TRUE, warn.conflicts = FALSE)
library(poppr, quietly=TRUE, warn.conflicts = FALSE)
library(plyr, quietly=TRUE, warn.conflicts = FALSE)
library(FactoMineR, quietly=TRUE, warn.conflicts = FALSE)
library(factoextra, quietly=TRUE, warn.conflicts = FALSE)
library(PopGenReport, quietly=TRUE, warn.conflicts = FALSE)
library(hierfstat, quietly=TRUE, warn.conflicts = FALSE)
library(pegas, quietly=TRUE, warn.conflicts = FALSE)
require(colourpicker, quietly=TRUE, warn.conflicts = FALSE) #couleur selecteur
require(shinyFeedback, quietly=TRUE, warn.conflicts = FALSE) #met des warning aux inputs

library(shinyFiles, quietly=TRUE, warn.conflicts = FALSE)
library(shinycssloaders, quietly=TRUE, warn.conflicts = FALSE)
library(shinycustomloader, quietly=TRUE, warn.conflicts = FALSE)

library(ComplexHeatmap, quietly=TRUE, warn.conflicts = FALSE)
library(circlize, quietly=TRUE, warn.conflicts = FALSE)

library(ggplot2, quietly=TRUE, warn.conflicts = FALSE)
library(dplyr, quietly=TRUE, warn.conflicts = FALSE)
library(plotly, quietly=TRUE, warn.conflicts = FALSE)


library(magrittr, quietly=TRUE, warn.conflicts = FALSE)

library(purrr, quietly=TRUE, warn.conflicts = FALSE)
library(skimr, quietly=TRUE, warn.conflicts = FALSE)


#leaflet = carte interactive

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
plotind12 <- function(mlva12, Showsup, Showind){ 
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  }
  pi12 <- plot(mlva12, axes=c(1,2), choix = "ind", label = labPCA, cex=1.5)
  #pi12 <- pi12 + geom_text(check_overlap = TRUE, size = 18)
  pi12 <- pi12 + theme(panel.grid.major = element_blank(),
             plot.title=element_text(size=18, color="blue"),
             axis.text=element_text(size=18, color="black"),
             axis.title = element_text(size=18, color="red"))
  return(pi12)
}
plotvar12 <- function(mlva12){ 
  pv12 = plot(mlva12, axes=c(1,2), choix = "var", cex=1.5)
  pv12 = pv12 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  return(pv12)
}
habillageind12 <- function(mlva12, colone, colonesup,Showsup, Showind){ 
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  } 
  l = length(colone)
  if(colonesup != "None"){
    h12 = plot.PCA(mlva12, axes=c(1,2), choix="ind", label = labPCA, habillage=l+1, cex=1.5)
    h12 = h12 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"),
                      legend.title = element_text(size = 18),
                      legend.text = element_text(size = 18))
  }
  return(h12)
}
habillageind12inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h12i = plot.PCA(mlva12, axes=c(1,2), choix="ind", invisible="ind", habillage=l+1, cex=1.5)
    h12i = h12i + theme(panel.grid.major = element_blank(),
                        plot.title=element_text(size=18, color="blue"),
                        axis.text=element_text(size=18, color="black"),
                        axis.title = element_text(size=18, color="red"))
  }
  return(h12i)
}
#1v3
plotind13 <- function(mlva12, Showsup, Showind){
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  }
  pi13 = plot.PCA(mlva12, axes=c(1,3), choix = "ind", cex=1.5, label = labPCA)
  pi13 = pi13 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  return(pi13)
}
plotvar13 <- function(mlva12){ 
  pv13 = plot.PCA(mlva12, axes=c(1,3), choix = "var", cex=1.5)
  pv13 = pv13 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  return(pv13)
}
habillageind13 <- function(mlva12, colone, colonesup,Showsup, Showind){ 
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  } 
  l = length(colone)
  if(colonesup != "None"){
    h13 = plot.PCA(mlva12, axes=c(1,3), choix="ind", habillage=l+1, cex=1.5, label = labPCA)
    h13 = h13 + theme(panel.grid.major = element_blank(),
                        plot.title=element_text(size=18, color="blue"),
                        axis.text=element_text(size=18, color="black"),
                        axis.title = element_text(size=18, color="red"),
                        legend.title = element_text(size = 18),
                        legend.text = element_text(size = 18))
  }
  return(h13)
}
habillageind13inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h13i = plot.PCA(mlva12, axes=c(1,3), choix="ind", invisible="ind", habillage=l+1, cex=1.5)
    h13i = h13i + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  }
  return(h13i)
}
#2v3
plotind23 <- function(mlva12, Showsup, Showind){ 
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  }
  pi23 = plot.PCA(mlva12, axes=c(2,3), choix = "ind", cex=1.5, label = labPCA)
  pi23 = pi23 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  return(pi23)
}
plotvar23 <- function(mlva12){ 
  pv23 = plot.PCA(mlva12, axes=c(2,3), choix = "var", cex=1.5)
  pv23 = pv23 + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
  return(pv23)
}
habillageind23 <- function(mlva12, colone, colonesup,Showsup, Showind){ 
  if (Showsup==TRUE && Showind == TRUE){
    labPCA = c("ind","quali")
  }
  else if (Showsup==TRUE && Showind == FALSE){
    labPCA = "quali"
  }
  else if (Showsup==FALSE && Showind == TRUE){
    labPCA = "ind"
  }
  else{
    labPCA = "none"
  }
  l = length(colone)
  if(colonesup != "None"){
    h23 = plot.PCA(mlva12, axes=c(2,3), choix="ind", habillage=l+1, cex=1.5, label = labPCA)
    h23 = h23 + theme(panel.grid.major = element_blank(),
                        plot.title=element_text(size=18, color="blue"),
                        axis.text=element_text(size=18, color="black"),
                        axis.title = element_text(size=18, color="red"),
                        legend.title = element_text(size = 18),
                        legend.text = element_text(size = 18))
  }
  return(h23)
}
habillageind23inv <- function(mlva12, colone, colonesup){ 
  l = length(colone)
  if(colonesup != "None"){
    h23i = plot.PCA(mlva12, axes=c(2,3), choix="ind", invisible="ind", habillage=l+1,cex=1.5)
    h23i = h23i + theme(panel.grid.major = element_blank(),
                      plot.title=element_text(size=18, color="blue"),
                      axis.text=element_text(size=18, color="black"),
                      axis.title = element_text(size=18, color="red"))
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

## combinaison DAPC et Snapclust

DAPCandSnap <- function(c.xvm, xclust, npcaDC, daDC){
  DAPCwithSNAP = dapc(c.xvm, x.clust$group, n.pca = npcaDC, n.da = daDC)
  return(DAPCwithSNAP)
}

## create table with dapc groups

DAPCdatagrp <-function(col, dapcdata){
  mat = matrix("NA", nrow(col), 1)
  rownames(mat) = rownames(col)
  mat[,1] = as.character(dapcdata$assign)
  return(mat)
}



########################------------


#msn

get_dist <- function(indist){
  indist <- switch(indist,
                   Dissimilarity = "diss.dist",
                   Bruvo         = "bruvo.dist",
                   Nei           = "nei.dist",
                   Rogers        = "rogers.dist",
                   Edwards       = "edwards.dist",
                   Provesti      = "provesti.dist",
                   Reynolds      = "reynolds.dist"
  )
  return(indist)
}

#------------------------------------------------------------------------------#
# If the user does select custom, this function will make sure that it is 
# encapsulated in parentheses. This makes sure that the entire expression is
# evaluated.
#------------------------------------------------------------------------------#
parse_distfun <- function(x){
  if (grepl("function", x)){
    x <- paste0("(", x, ")")
  }
  return(x)
}

#------------------------------------------------------------------------------#
# This is a function that will print vectors of numerics or characters in a way
# that can be run directly from R. For example, if the user checks two boxes
# labeling the populations "pop1" and "pop2", that vector of populations gets 
# passed onto the popsub function. By default, it will print like so:
#
# [1] "pop1" "pop2"
#
# You cannot copy and paste this into R because it will throw an error, thus, 
# this function will print the vector like so:
#
# c("pop1", "pop2")
#
# This is then usable in the Command tab for the popsub function.
#------------------------------------------------------------------------------#
make_dput <- function(x){
  the_dput <- capture.output(dput(x))
  return(paste(the_dput, collapse = ""))
}

#------------------------------------------------------------------------------#
# A function to search the user's global environment and grab all of the useable
# objects. In this case, it's genind and genclone objects, but the objclass
# argument allows this to be extensible to any class. This is immensely useful
# so that the user does not have to save their objects as rda files, nor do they
# have to save them as text files for input.
#------------------------------------------------------------------------------#
get_globals <- function(objclass = c("genind", "genclone")){
  # Grab all object names in users R session.
  myobjs <- ls(envir = .GlobalEnv) 
  if (length(myobjs) == 0){
    return(myobjs)
  }
  # Go through each name and test if it is any of the classes in objclass.
  gens <- vapply(myobjs, FUN = is_usable, FUN.VALUE = logical(1), objclass)
  myobjs[gens]
}

#------------------------------------------------------------------------------#
# This function tests a single object name to see if it is of class objclass.
# The function is used in get_globals
#------------------------------------------------------------------------------#
is_usable <- function(object, objclass = c("genind", "genclone", "genlight", "snpclone")){
  inherits(get(object, .GlobalEnv), objclass)
}



