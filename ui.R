library(shinydashboard)
library(shiny)
library(ncdf4)
library(chron)
library(sp)
library(rgdal)
library(raster)
library(tmap)
library(R.utils)
a1 <- "Temperature"
a2 <- "Max temperature"
a3 <- "Min temperature"
a4 <- "Wind 10m"
a5 <- "Wind 30m"
a6 <- "Wind 60m"
a7 <- "Gust max"
a8 <- "Cape"
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem(a1, tabName = "dashboard1", icon = icon("dashboard")),
    menuItem(a2, tabName = "dashboard2", icon = icon("dashboard")),
    menuItem(a3, tabName = "dashboard3", icon = icon("dashboard")),
    menuItem(a4, tabName = "dashboard4", icon = icon("dashboard")),
    menuItem(a5, tabName = "dashboard5", icon = icon("dashboard")),
    menuItem(a6, tabName = "dashboard6", icon = icon("dashboard")),
    menuItem(a7, tabName = "dashboard7", icon = icon("dashboard")),
    menuItem(a8, tabName = "dashboard8", icon = icon("dashboard")),
    menuItem("Chart", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    box(width = 15,
        title = "Animation", status = "primary", solidHeader = T,
        collapsible = TRUE,
        sliderInput("bins", "Looping Animation:",
                    min = 1, max = 48,
                    value = 1, step = 1,
                    animate =
                      animationOptions(interval = 800, loop = T))
        
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard1", h2(a1),
            plotOutput("Plot_temp" , width = "100%"),
            br(),br(),br(),br(),
            uiOutput("czas_temp")),
    tabItem(tabName = "dashboard2", h2(a2)),
    tabItem(tabName = "dashboard3", h2(a3)),
    tabItem(tabName = "dashboard4", h2(a4)),
    tabItem(tabName = "dashboard5", h2(a5)),
    tabItem(tabName = "dashboard6", h2(a6)),
    tabItem(tabName = "dashboard7", h2(a7)),
    
    tabItem(tabName = "dashboard8",
            h2(a8),

            
            
            plotOutput("Plot_cape" , width = "100%"),
            br(),br(),br(),br(),
            uiOutput("czas_cape")
            
            
    ),
    
    tabItem(tabName = "widgets",
            h2("Chart"),
            tabBox(
              title = "Chart Box",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "550px",
              tabPanel("Temp",
                       plotOutput("widget_temp"))#,
              #tabPanel("Cape",
                     #  plotOutput("widget_cape"))
            )
            
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  
  dashboardHeader(title = "Meteo",
                  dropdownMenuOutput("messageMenu"),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "12 items delivered",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "Server load at 86%",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  )
                  ),
  sidebar,
  body
)
