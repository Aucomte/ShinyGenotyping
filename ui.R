
header <- dashboardHeader(title = "MLVA", titleWidth = 380)

sidebar <- dashboardSidebar(
  width = 180,
  sidebarMenu(
    #menuItem("Home", tabName = "menu", icon = icon("home")),
    menuItem("input table", tabName = "input", icon = icon("book-open")), 
    menuItem("Genotype object", tabName = "genind", icon = icon("book-open")),
    menuItem("PCA", tabName = "PCAtab", icon = icon("calculator")),
    menuItem("Diversity Heatmap", tabName = "DiversityByLocus", icon = icon("calculator")),
    menuItem("Statitics", tabName = "Statitics", icon = icon("calculator"))
  )
)

# js <- "
# $(document).ready(function() {
#   $('#heatmapDiv').on('shiny:recalculating', function() {
#     $('downloadDiv').prop('disabled', true);
#     $('downloadDiv').css('color', 'red');
#   });
#   $('#heatmapDiv').on('shiny:recalculated', function() {
#     $('downloadDiv').prop('disabled', false);
#     $('downloadDiv').css('color', 'black');
# });
# });
# "

body <- dashboardBody(
  #includeCSS('www/styles.css'),
  useShinyjs(),
  useShinyFeedback(),
  #tags$head(tags$script(HTML(js))),
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
            checkboxGroupInput(inputId = "checkboxcol", "column to use to calculate haplotypes: "),
            radioButtons(inputId = "genindtype", "type of marker :", choiceNames = c("codominant","presence/absence"), choiceValues = c("codom","PA"), selected = "codom"),
            actionButton(inputId="Submit","Submit")
            )
      ),
      fluidRow(
        box(width = 12,
            DT::dataTableOutput(outputId = "tabhaplo")
        )
      ),
      fluidRow(
        box(width = 12,
            DT::dataTableOutput(outputId = "tabhaploloc")
        )
      )
    ),
    tabItem(
      tabName ="PCAtab",
      fluidRow(
        box(width = 12,
            checkboxGroupInput(inputId = "checkboxpca", "Variables for PCA : "),
            selectInput(inputId = "checkboxpcasup", "suplementary variables for PCA : ", choice=""),
            selectInput(inputId = "axeschoices", "axes", choice=c("axe1 vs axe2", "axe1 vs axe3", "axe2 vs axe3")),
            actionButton(inputId = "Submitpca","Submit")
        )
      ),
      fluidRow(
        conditionalPanel("input.Submitpca",
          box(width = 6,
            plotOutput(outputId = "pcaInd", height = "600px") 
            #%>% withSpinner(color="#0dc5c1")
            %>% withLoader(loader = "dnaspin")
          ),
          box(width = 6,
            plotOutput(outputId = "pcaVar", height = "600px") 
            #%>% withSpinner(color="#0dc5c1")
            %>% withLoader(loader = "dnaspin")
          )
        ),
        fluidRow(
          box(width = 6,
            plotOutput(outputId = "pcahab", height = "600px") 
            #%>% withSpinner(color="#0dc5c1")
            %>% withLoader(loader = "dnaspin")
  
          ),
          box(width = 6,
            plotOutput(outputId = "pcahabi", height = "600px") 
            #%>% withSpinner(color="#0dc5c1")
            %>% withLoader(loader = "dnaspin")
          )
        )
      )
    ),
    tabItem(
      tabName ="DiversityByLocus",
      fluidRow(
        box(width = 12,
            checkboxGroupInput(inputId = "checkboxcolDiv", "column to use to calculate haplotypes: "),
            selectInput(inputId = "colsupDiv", "suplementary column : ", choice = ""),
            actionButton("submitarchive", "Calculate the diversity by locus (can be long)")
        ),
        fluidRow(
            conditionalPanel("output.heatmapDiv",
              box(width = 12,
                downloadButton('downloadDiv', 'Download Output archive',style="color: #fff; background-color: #ff0000; border-color: #000000; text-align: center;")
              )
          )
        ),
        conditionalPanel("input.submitarchive",
          box(width = 6,
              plotOutput(outputId = "heatmapDiv", height = "600px") 
                %>% withLoader(loader = "dnaspin")
          ),
          box(width = 6,
              plotOutput(outputId = "genocurve", height = "600px") 
              %>% withLoader(loader = "dnaspin")
          )
        )
      )
    ),
    tabItem(
      tabName ="Statitics",
      fluidRow(
        box(width = 12,
            plotOutput(outputId = "genostat", height = "600px") 
                %>% withLoader(loader = "dnaspin")
        )
      )
    )
  )
)

shinyUI(
  dashboardPage(title = "MLVA", skin = "yellow", header, sidebar, body)
)
