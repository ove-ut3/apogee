library(shinydashboard)

dashboardPage(
  dashboardHeader(title = paste("Offre de formation")),
  dashboardSidebar(width = 300, disable = TRUE, sidebarMenu(
    menuItem("Formations", tabName = "formations")#,
    # uiOutput("composante"),
    # uiOutput("type_diplome"),
    # uiOutput("mention"),
    # uiOutput("annee_formation")
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "formations",
              fixedRow(
                column(width = 12,
                       fluidRow(
                         DT::dataTableOutput("formations")
                       )
                )
                
              )
      )
    )
  )
)
