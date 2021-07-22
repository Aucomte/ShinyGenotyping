##TODO trouver un endroit ou le mettre
jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"


header <- dashboardHeader(title = "ShinyGenotyping", titleWidth = 380)
useShinyjs()
extendShinyjs(text = jscode, functions = c("pageCol"))
inlineCSS(css)
sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("Input", tabName = "inputs", icon = icon("book-open")), 
    menuItem("Statistics", tabName = "stats", icon = icon("calculator")),
    menuItem("Find K", tabName = "FindK", icon = icon("calculator")),
    #menuItem("SnapClust", tabName = "SnapClust", icon = icon("calculator")),
    menuItem("DAPC", tabName = "DAPC", icon = icon("calculator"))
    #menuItem("Session Info", tabName = "SI", icon = icon("book-open"))
  )
)
body <- dashboardBody(
  includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  #tags$head(tags$script(HTML(js))),
  tabItems(
    
    source(file.path("UI", "tab_HomeUI.R"), local = TRUE, chdir = TRUE)$value,
    
    source(file.path("UI", "tab_InputUI.R"), local = TRUE, chdir = TRUE)$value,
    
    source(file.path("UI", "tab_StatsUI.R"), local = TRUE, chdir = TRUE)$value,
    
    source(file.path("UI", "tab_findNClusterUI.R"), local = TRUE, chdir = TRUE)$value,
    
    source(file.path("UI", "tab_DAPCUI.R"), local = TRUE, chdir = TRUE)$value,
    
    source(file.path("UI", "tab_SnapClustUI.R"), local = TRUE, chdir = TRUE)$value
    
    #tabItem(
    #  tabName = "SI",
    #  fluidRow(
    #    verbatimTextOutput("urlText1"),
    #    verbatimTextOutput("urlText2")
    #  )
    #)
  )
)
shinyUI(
  dashboardPage(title = "SninyGenotyping", skin = "yellow", header, sidebar, body)
)
