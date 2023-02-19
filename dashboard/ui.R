library(shinydashboard)
library(shinythemes)

dashboardPage(
  title = 'Download a PDF report',
  dashboardHeader(title = "yabliki"),
  dashboardSidebar(
    sidebarMenu(
    )
  ),
  dashboardBody(
    fluidRow(
      valueBox(35, "Some lame numbers", color = "blue", width = 3),
      valueBox(35, "Some other numbers", color = "blue", width = 3),
      valueBox(35, "Some cool numbers", color = "blue", width = 3),
      valueBox(35, "Some sad numbers", color = "blue", width = 3)
    ),
    fluidRow(
      
    )
  )
)