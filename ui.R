library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(rhandsontable)
library(plotly)
library(broom)
library(deSolve)


dashboardPage(
  dashboardHeader(title = "My Dashboard"),

  dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("cases", tabName = "cases"),
    menuItem("timeseries", tabName = "timeseries")
  )
  ),

  
 # sliderInput("size", "Size of Points:", min=0.2, max=5, value=2)

  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      tabItem(tabName = "cases",
              h2("COVID-19 Florida Cases"),


    fluidRow(
      # box(width=5,
      #   title = "Controls",
      #   sliderInput("sliderday", "Days:", 1, 30, 1)
      # )
      
      box(width = 5,
        sliderInput("sliderday",
                  "Dates:",
                  min = as.Date("2020-03-01","%Y-%m-%d"),
                  max = as.Date("2020-04-08","%Y-%m-%d"),
                  value=as.Date("2020-03-08"),timeFormat="%Y-%m-%d")
      ),
        box(width = 5,
            radioButtons("casedeath", "Choose Cases or Deaths:",
                   choices = 
                     c("Cases" = "Cases",
                       "Deaths" = "Deaths"),
                   selected = "Cases")
                  )
      ),

    fluidRow(    
      box(width=5, 
          status="info", 
          title="Number of Cases in Each County",
          solidHeader = TRUE,
          plotlyOutput("myplot")
      ),
      box(width=7, 
          #height = 10,
          status="warning", 
          title = "Cases in Florida Counties",
          solidHeader = TRUE, 
          collapsible = TRUE, 
          footer="Read Remotely from File",
          dataTableOutput("mydata")
      )
    )
  ),
  
    #),
  #tabItems(
    tabItem(tabName = "timeseries",
            h2("COVID-19 Time Series"),
            fluidRow(
            box(width=8,
                height = "400px",
                status="info", 
                title="Number of Cases in Each County",
                solidHeader = TRUE,
                plotlyOutput("time1")
            )
            )
            )
        )
    )
)
