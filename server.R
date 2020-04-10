library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(DT)
library(rhandsontable)
library(plotly)
library(dplyr)
library(broom)
library(deSolve)



mygitpath <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

#mygitpath <- 'https://raw.githubusercontent.com/mandab749/covid19/master/CoronaCases.csv'

library(tidyverse)
library(maps)
library(mapproj)
corona <- read_csv(mygitpath)
coronadf <- corona

shinyServer(function(input, output, session) {
  
  observe({
    if (input$tabs == 'cases') {

  df <- reactiveFileReader(
           intervalMillis = 10000,
           session = session,
           filePath = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv',
          readFunc = read_csv)

 output$mydata <- DT::renderDataTable({#df()

   
   df <- df() %>% filter(state == "Florida") %>% group_by(state, county) %>% filter(date == input$sliderday)
   return(df)
   
   })
 
  
  output$myplot <- renderPlotly({
    df <- df()
    corona <- df()

    #corona <- read_csv(mygitpath)
    #view counties
    get.county <- map_data("county") %>% filter(region =="florida") 
    corona <- corona %>% filter(county != "Unknown")
    unique(get.county$subregion) #unknown regions
    
    # Getting county info
    corona$county <- tolower(corona$county)
    setdiff(corona$county, unique(get.county$subregion))
    
    #changing county names
    get.county.1 <- get.county %>% mutate(subregion = fct_recode(subregion, `st. lucie` = "st lucie",`st. johns` = "st johns", `dade` = "miami-dade"))
    setdiff(corona$County, unique(get.county.1$subregion))
    
    corona.case <- corona %>% group_by(county) %>% filter(date == input$sliderday) %>% dplyr::mutate(count = n())
    #print(corona.case)
    
    
    #filtering by "deaths" and "cases" for that date
    
    if(input$casedeath == "Deaths"){
      #print("death")
      corona.case1 <- corona.case %>% group_by(county, deaths) %>% filter(date == input$sliderday) %>% dplyr::mutate(count = n()) 
      coronaMap <- left_join(get.county.1, corona.case, by = c("subregion" = "county"))
      p <- ggplot(coronaMap, aes(x = long, y = lat, group = group)) + 
        geom_polygon(aes(fill = count))
      
      return(ggplotly(p))
    }
    
     if(input$casedeath == "Cases"){
       #print("case")
       corona.case2 <- corona.case %>% group_by(county, cases) %>% filter(date == input$sliderday) %>% dplyr::mutate(count = n())
       coronaMap <- left_join(get.county.1, corona.case, by = c("subregion" = "county"))
       p2 <- ggplot(coronaMap, aes(x = long, y = lat, group = group)) + 
         geom_polygon(aes(fill = count))
       
       return(ggplotly(p2))
     }
    

    #mapping results
    # coronaMap <- left_join(get.county.1, corona.case1, by = c("subregion" = "county"))
    # p <- ggplot(coronaMap, aes(x = long, y = lat, group = group)) + 
    #   geom_polygon(aes(fill = count))
    # 
    # return(ggplotly(p))
    # 
      })
    }
  })
  
  
  observe({
    if (input$tabs == 'timeseries') {
      output$nrows1 <- renderValueBox({
        nr <- nrow(df())
        valueBox(
          value = nr,
          subtitle = "Number of Rows",
          icon = icon("table"),
          color = if (nr <=6) "yellow" else "aqua"
        )
      })
      
      output$ncol1 <- renderInfoBox({
        nc <- ncol(df())
        infoBox(
          value = nc,
          title = "Colums",
          icon = icon("list"),
          color = "purple",
          fill=TRUE)
      })
      output$time1 <- renderPlotly({
        initial.parameters <- c(
          beta  = 0.5, # transmission rate (per contact per day). beta=0.5 means that out of every contact a susceptible has in a day, 0.5 result in an infection.
          gamma = 1/10    # recovery rate (per day)
        )
        
        initial.group <- c(
          S = 100000,  # number of susceptibles at time = 0
          I = 1,  # number of infectious at time = 0
          R = 0   # number of recovered (and immune) at time = 0
        )
        
        times <- seq(0, 90, 0.5) # better for solving diff. eqns to go in small steps
        
        SIRmodel <- function(time, group, parameters) {
          S <- group[1]; I <- group[2];  R <- group[3]
          N <- S + I + R
          beta <- parameters[1]; gamma <- parameters[2]
          dS <- -beta*S*I/N
          dI <- +beta*S*I/N - gamma*I
          dR <- gamma*I
          list(c(dS, dI, dR))
        }

        #corona.sama.all <- coronadf %>% filter(county %in% c("Sarasota","Manatee")) %>%
        corona.sama.all <- coronadf %>% filter(state == "Florida") %>%
          group_by(date) %>%
          summarize(sumcases=sum(cases)) %>%
          select(date,sumcases) %>%
          mutate(
            time=c(0,cumsum(as.numeric(diff(date)))),
            logsumcases = log(sumcases)
          ) %>%
          select(date, time, sumcases, logsumcases)

        colnames(corona.sama.all) <- c("date", "time", "cases", "logcases")
        #corona.sama.all
        cutoff <- "2020/04/08"
        corona.sama <- corona.sama.all %>% filter(date<=cutoff)
        fit.lm <- lm(logcases ~ time, data=corona.sama)
        corona.sama$logfitted <- fitted(fit.lm)
        corona.sama$fitted <- exp(fitted(fit.lm))

        plotdata <- pivot_longer(corona.sama, col=3:4, names_to="Type", values_to="values")
        alpha <- exp(as.numeric(tidy(fit.lm)[1,2]))
        gamma <- 1/8
        beta <- as.numeric(tidy(fit.lm)[2,2]) + gamma
        times <- seq(0,31,1)

        ## Solving Differential Equation:
        solution <- lsoda(
          y = c(S=800000, I=alpha, R=0),
          times = times,
          func = SIRmodel,
          parms = c(beta=beta, gamma=gamma)
        )

        ## Solution:
        result <- data.frame(solution)[,c(1,3)] #first column holds time, third column holds I(t)
        result <- result %>% mutate(date = seq(as.Date(corona.sama$date[1]), by = "day", length.out = length(time)))


       p <- ggplot(data=result, aes(x=date, y=I)) +
          geom_line(size=1.0, col="blue") +
          geom_point(data=corona.sama, aes(x=date, y=cases), col="red", inherit.aes = FALSE) +
          scale_x_date(date_breaks="4 days", date_label="%b %d") +
          labs(title="Number of Infections in Florida",
               subtitle="Recovery Period is assumed as 8 days",
               x="Day", y="Number Infected")

       return(ggplotly(p))
        
      })
      
      
      
    }
  })

})
