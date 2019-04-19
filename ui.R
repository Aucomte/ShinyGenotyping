header <- dashboardHeader(title = "MLVA", titleWidth = 380)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    #menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("input table", tabName = "input", icon = icon("book-open")), 
    menuItem("Genotype object", tabName = "genind", icon = icon("book-open"))
  )
)

body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem(
      tabName ="input",
      fluidRow(
        box(width=12, class = "box1",
            column(width = 6,
              fileInput("file1", "CSV File", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
            ),
            column(width = 6,
               column(width = 3,
                  radioButtons('sep', 'Separator',
                                 c(Semicolon=';',
                                 Tab='\t'),
                                 selected = '\t')
               )
            )
        )
      ),
      fluidRow(
        box(width = 12,
            DT::dataTableOutput(outputId = "DataSet")
        )
      )
    ),
    tabItem(
      tabName ="genind",
      fluidRow(
        box(width = 12,
            checkboxGroupInput(inputId = "checkboxcol", "column to use")
        ),
        box(width = 12,
            radioButtons(inputId = "genindtype", "type of marker", choiceNames = c("codominant","presence/absence"), choiceValues = c("codom","PA"), selected = "codom")
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(title = "MLVA", skin = "yellow", header, sidebar, body)
)
