library(shiny)
library(shinydashboard)
library(stringr)
library(DT)

setwd("/Users/Carl/Google Drive/women soccer/dashboard")
# source("../script.R")
load("./soccer.RData")

# dbHeader <- dashboardHeader(title = "Michigan Soccer",
#                             tags$li(a(icon("power-off"),
#                                       title = "Back to Apps Home"),
#                                     class = "dropdown"),
#                             tags$li(a(img(src = "Mlogo.png",
#                                           title = "Company Home", height = "30px"),
#                                       style = "padding-top:10px; padding-bottom:10px;"),
#                                     class = "dropdown"))

# dbHeader = dashboardHeader(
#   tags$li(class = "dropdown",
#     tags$style(".main-header {max-height: 80px}"),
#     tags$style(".main-header .logo {height: 80px}")
#   ),
#   title = tags$a(tags$img(src = "Mlogo.jpg", height = "80px", width = "200px"))
# )

dbHeader = dashboardHeader(title = "Michigan Soccer", titleWidth = 300)

dbSidebar = dashboardSidebar(width = 300, 
  sidebarMenu(
    menuItem("Home Page", tabName = "home", icon = icon("home")), 
    menuItem("Acute-Chronic Load", tabName = "acute_chron", icon = icon("bar-chart")), 
    menuItem("Player Charts", tabName = "player", icon = icon("address-card-o"))
  )
)

dbBody = dashboardBody(
  fluidRow(
    column(width = 10, offset = 1,
      tabItems(
        tabItem(tabName = "home"), 
        tabItem(tabName = "acute_chron"), 
        tabItem(tabName = "player", 
          h2("Player Analytics"), 
          fluidRow(
            column(width = 2, 
              box(width = NULL, title = "Select Player", status = "primary", solidHeader = TRUE, 
                  radioButtons("sel_player", label = NULL, choices = names, selected = names[1]))
            ), 
            column(width = 10, 
              box(width = NULL,  title = "Player Information", 
                  status = "primary", solidHeader = TRUE, 
                  fluidRow(column(width = 4, img(src = "emptyPortrait.png")), 
                           column(width = 4, htmlOutput("player_info")), 
                           column(width = 4, 
                             valueBoxOutput("goal", width = NULL), 
                             valueBoxOutput("assist", width = NULL), 
                             valueBoxOutput("start", width = NULL) 
                           )
                  )
              ), 
              fluidRow(
                box(width = 6, height = 450, title = "Weekly Load", status = "primary", 
                    plotOutput("week_load")),
                box(width = 6, height = 450, title = "Daily Load", status = "primary", 
                    plotOutput("day_load"))
              )
            )
          )
        )
      )
    )
  ) 
)

ui = dashboardPage(
  header = dbHeader,
  sidebar =dbSidebar,
  body = dbBody
)

server = function(input, output) {
  output$player_info = renderUI({
    # something like this in the future: 
    # p_info = players_info[[input$sel_player]]
    temp = "<h3>%s</h3>
            Position: %s<br>
            Date of Birth: %s<br>
            Department: %s<br>
            Class Standing: %s<br>
           "
    HTML(sprintf(temp, input$sel_player, "Midfielder", "1996-01-01", "Statistics", "Junior"))
  })
  output$week_load = renderPlot(p_week[[input$sel_player]])
  output$day_load = renderPlot(p_day[[input$sel_player]])

  output$goal = renderValueBox(
    valueBox(5, "Goals", icon = icon("soccer-ball-o"), color = "yellow")
  )
  output$assist = renderValueBox(
    valueBox(3, "Assists", icon = icon("mail-reply"), color = "yellow")
  )
  output$start = renderValueBox(
    valueBox(8, "Starts", icon = icon("user"), color = "yellow")
  )
}

shinyApp(ui = ui, server = server)
