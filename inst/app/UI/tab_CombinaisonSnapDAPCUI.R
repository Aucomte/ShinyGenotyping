tabItem(
  tabName ="Combinaison",
  pageWithSidebar(
    headerPanel("Comparison and combinaison of Snapclust and DAPC"),
    sidebarPanel(
    ),
    mainPanel(
       tabsetPanel(id = "tabselected",
      #     tabPanel("Comparison", value=61,
      #              plotOutput("comp")
      #     ),
          tabPanel("Combinaison", value=62,
                   plotOutput("comb")
          )
      )
    )
  )
)