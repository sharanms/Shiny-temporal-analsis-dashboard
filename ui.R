library(shinydashboard)

shinyUI(
  column(12,
  dashboardPage(skin = "red",
    dashboardHeader(title = "Insnap Dashboard",titleWidth = 350),
    # dashboardHeader(title = ),
    dashboardSidebar(disable = T),
    dashboardBody(tags$head(tags$style(HTML('
      .main-header .logo {
        # font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
        img:insnap_logo_space.png;
      }
    '))),
      fluidRow(
        # dashboardSidebar(width = 350),
        # valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
        valueBoxOutput("Users",width = 4),
        valueBoxOutput("Photos",width = 4),
        valueBoxOutput("GPSON",width = 4)
      ),
      fluidRow(
        # dashboardSidebar(width = 350),
        # valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
        valueBoxOutput("Country_count",width = 4),
        valueBoxOutput("State_count",width = 4),
        valueBoxOutput("City_count",width = 4)
      ),
      fluidRow(
        # dashboardSidebar(width = 350),
        # valueBox(10 * 2, "New Orders", icon = icon("credit-card")),
        valueBoxOutput("interest0_count",width = 4),
        valueBoxOutput("interest1_count",width = 4),
        valueBoxOutput("interest2_count",width = 4)
      ),
fluidRow(
  HTML('<hr style="color: purple;height: 20px">'),
  titlePanel("Regional Activity Density"),
  uiOutput("UIforregional_Density")
),
fluidPage(
  HTML('<hr style="color: purple;height: 20px">'),
  titlePanel("Most Engaging Interests"),
  uiOutput("UIforStateComparison")
),
fluidPage(
  HTML('<hr style="color: purple;height: 20px">'),
  titlePanel("Monthly Interest Trends"),
  uiOutput("UIforMonths")
),
fluidPage(
  HTML('<hr style="color: purple;height: 20px">'),
  titlePanel("Daily Interest Trends"),
  uiOutput("UIforDays")
),

fluidPage(
  HTML('<hr style="color: purple;height: 20px">'),
  titlePanel("Hourly Interest Trends"),
  uiOutput("UIforhours")
)


  )
  )

)
)

