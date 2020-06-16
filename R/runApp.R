#!/usr/bin/Rscript

#' Launch ShinyGenotyping GUI
#'
#' This function run the GUI shiny interface to run ShinyGenotyping
#'
#' @examples
#' runShinyGenotyping()
runShinyGenotyping <- function() {
  appDir <- system.file("app", package = "ShinyGenotyping")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }
  if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2) #pour éviter de surcharger les serveurs
  shiny::runApp(appDir = appDir,launch.browser = TRUE)
}

