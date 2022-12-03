# load library
# library(shiny)
library(ggridges)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(recipes)
library(vtable)
library(psych)
library(plotly)
library(corrplot)
library(ggcorrplot)
# library(tidymodels)

# setwd('/Users/zhangshuxin/Desktop/AA/APAN5800/group project')
# import data
data <- read.csv('cod.csv', 
                    stringsAsFactors=FALSE, sep=",", header=TRUE)
# data = read.csv('cod.csv')
df<-data[,-1]
df <- df
options(scipen = 0)


# define ui for application
# main panel and sidebars
ui <- dashboardPage(
  dashboardHeader(title = "COD Time played Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Descriptive Statistics", tabName = 'descriptive', icon = icon("dashboard")),
      menuItem("Boxplots", tabName = 'boxplot', icon = icon("dashboard")),
      menuItem("Scatterplots", tabName = 'scatterplot', icon = icon("dashboard")),
      menuItem("Correlation Analysis", tabName = 'correlation', icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # descriptive statistics
      tabItem(tabName = 'descriptive',
              fluidPage(titlePanel("Descriptive Statistics"),
                        # instruction
                        strong('Here are some basic descriptive statistics about the dataset of player behaviors in Call of Duty'),
                        br(),
                        br(),
                        box(width = 12, DT::dataTableOutput("descriptive_table"))
                        )),
      # scatter plot
      tabItem(tabName = 'scatterplot',
              fluidPage(titlePanel("Scatter Plot"),
                        
                        # sidebar control
                        sidebarLayout(
                          sidebarPanel(
                            # x and y variables input
                            selectInput(inputId = "VarX",
                                        label = "Select X-axis Variable:",
                                        choices = list("wins",
                                                       "kills", "kdRatio",
                                                       "level", "timePlayed","gamesPlayed",
                                                       "scorePerMinute", "deaths")),
                            selectInput(inputId = "VarY",
                                        label = "Select Y-axis Variable:",
                                        choices = list("wins",
                                                       "kills", "kdRatio",
                                                       "level", "timePlayed","gamesPlayed",
                                                       "scorePerMinute", "deaths")),
                            
                            # slider control x axis limit
                            sliderInput(inputId = "slider",
                                        label = "X-Axis Limit",
                                        min = 0,
                                        max = 10000,
                                        value = c(10, 9990)),
                            p('Instruction: Select x and y variables to check how player behaviors associate with each other, use the slider to change the range of x-axis') 
                          ),
                          
                          # main plot
                          mainPanel(
                            plotlyOutput("scatter"),
                            strong('')
                          )
                        )
              )),
      # boxplot
      tabItem(tabName = 'boxplot',
              fluidPage(titlePanel("Box Plot"),
                        
                        # sidebar control
                      
                        sidebarLayout(
                          sidebarPanel(
                            # variable input
                            selectInput(inputId = "Var",
                                        label = "Select Variable:",
                                        choices = list("wins",
                                                       "kills", "kdRatio",
                                                       "level", "timePlayed","gamesPlayed",
                                                       "xp","scorePerMinute", "deaths")),
                            p('Instruction: Select each variable to check its distribution') 
                          ),
                          
                          # main plot
                          mainPanel(
                            plotlyOutput("boxplot"),
                            strong('')
                          )
                        ))),
     
      # correlation matrix
       tabItem(tabName = 'correlation',
              fluidPage(titlePanel("Correlation Matrix of selected variables"),
                        
                        strong('Top 4 variables that are highly correlated with time played:'),
                        HTML("<ul><li>level</li><li>number of wins</li><li>number of deaths</li><li>number of kills</li></ul>"),
                        
                        # sidebar control
                        sidebarLayout(
                          sidebarPanel(
                            # checkbox for select all or select none
                            checkboxInput(inputId = 'all', 
                                          label = 'Select All/None', 
                                          value = TRUE),
                            
                            # checkbox to select multiple variables
                            checkboxGroupInput(inputId = "CorrVar",
                                        label = "Select two or more Variables:",
                                        all_var <- c("Number of wins" = "wins", 
                                          "Number of kills" = "kills", 
                                          "Kill/Death ratio" = "kdRatio",
                                          "Level" = "level",
                                          "Number of losses" = "losses",
                                          "Prestige mode" = "prestige",
                                          "Number of hits" = "hits",
                                          "Number of games layed" = 'gamesPlayed',
                                          "Number of Misses" = "misses",
                                          "Score gained per minute" = "scorePerMinute",
                                          "Number of shots" = "shots",
                                          "Number of deaths" = "deaths",
                                          "Time played" = "timePlayed"
                                          ))
                            
                          ),
                          
                          # main plot
                          mainPanel(
                            plotOutput("correlation")
                            
                          )
                        )
                        
                        
                        )
    )
  )
))


# define server for application  
server <- function(input, output, session) {
  
  # descriptive statistics table
  output$descriptive_table <- DT::renderDataTable({
    DT::datatable(
      describe(df, fast=TRUE, quant=c(.25,.50,.75)) %>%
        mutate_if(is.numeric, .funs = ~round(.x, digits = 2)),
      rownames = TRUE
    )
  })
  
  
  # scatter plot
  output$scatter <- renderPlotly({
    
    # calculate regression line
    fit <- lm(df[,input$VarY] ~ df[,input$VarX]) %>% fitted.values()
     
    # plot scatter plots 
    plot_ly(data = df, x = df[,input$VarX], y = df[,input$VarY], name = 'data',
            type = 'scatter', mode = 'markers') %>%
        
      # add regression line
      add_trace(data = df,x = df[,input$VarX], y = fit,
                name = 'Regression Fit', mode = "lines", alpha = 1) %>%
      
      # add annotation, change size, color
      layout(title = paste("Scatter Plot of", input$VarX, "Vs", input$VarY), 
             plot_bgcolor = "#e5ecf6", xaxis = list(title = input$VarX, range = c(input$slider[1], input$slider[2])), 
             yaxis = list(title = input$VarY), height = "600px", width = "600px")
      
  })
  
  
  # boxplot
  output$boxplot <- renderPlotly({
    
    # plot boxplots
    plot_ly(data = df, y = df[,input$Var], name = input$Var,
            type = 'box', boxpoints = 'all', jitter = 0.3, pointpos = -1.8) %>% 
      
      # change layout
      layout(title = paste("Box Plot of", input$Var),
             plot_bgcolor = "#e5ecf6", height = "600px", width = "600px")
     
  })
  
  # select all or none
  observe({
    updateCheckboxGroupInput(
      session, 'CorrVar', choices = all_var,
      selected = if(input$all) all_var
    )
  })
  
  # plot correlation matrix
  output$correlation <- renderPlot({
    
    df1 <- df[,c(input$CorrVar)]
    corr <- round(cor(df1), 2)
    
    corrplot.mixed(corr, order = 'AOE', lower="circle", upper="number", diag = "l", tl.pos = "lt")
      
    
    
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
