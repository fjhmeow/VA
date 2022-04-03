library(shiny)
library(tidyverse)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(corrplot)
library(gplots)
library(forcats)
library(shinydashboard)

thematic::thematic_shiny()

A2020 <- read_csv("data/road-casualty-statistics-accident-2020.csv")
A2019 <- read_csv("data/road-casualty-statistics-accident-2019.csv")
A2018 <- read_csv("data/road-casualty-statistics-accident-2018.csv")
A2017 <- read_csv("data/road-casualty-statistics-accident-2017.csv")
AA2020 <- filter(A2020, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2019 <- filter(A2019, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2018 <- filter(A2018, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
AA2017 <- filter(A2017, local_authority_ons_district %in% c("E09000001", "E09000002","E09000003","E09000004",
                                                            "E09000005","E09000006","E09000007","E09000008",
                                                            "E09000009","E09000010","E09000011","E09000012",
                                                            "E09000013","E09000014","E09000015","E09000016",
                                                            "E09000017","E09000018","E09000019","E09000020",
                                                            "E09000021","E09000022","E09000023","E09000024",
                                                            "E09000025","E09000026","E09000027","E09000028",
                                                            "E09000029","E09000030","E09000031","E09000032",
                                                            "E09000033"))
accident <- rbind(AA2020, AA2019, AA2018, AA2017)



ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'Analysis For London Accident', titleWidth = 350),
                    dashboardSidebar(width = 300,
                                     sidebarMenu(id = 'sbm',
                                                 menuItem('Exploratory Data Analysis', tabName = 'EDA', icon = icon('signal')),
                                                 menuItem('Geovisual Analysis', tabName = 'GeoVisual', icon = icon('map')),
                                                 menuItem('Regression Analysis', tabName = "Regression", icon = icon("chart-line"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = 'EDA',
                                fluidPage(
                                  titlePanel("Exploratory Data Analysis (EDA)"),
                                  fluidRow(column(width = 6,
                                                  selectInput(inputId = "cat1",
                                                              label = "Environment conditions:",
                                                              choices = c("light conditions" = "light_conditions",
                                                                          "weather conditions" = "weather_conditions",
                                                                          "day of week" = "day_of_week",
                                                                          "road type" = "road_type",
                                                                          "speed limit" = "speed_limit",
                                                                          "pedestrian crossing human control" = "pedestrian_crossing_human_control",
                                                                          "road surface conditions" = "road_surface_conditions",
                                                                          "special conditions at site" = "special_conditions_at_site",
                                                                          "carriageway hazards" = "carriageway_hazards",
                                                                          "urban or rural area" = "urban_or_rural_area"),
                                                              selected = "light_conditions"),
                                                  selectInput(inputId = "cat2",
                                                              label = "Accident conditions:",
                                                              choices = c("accident severity" = "accident_severity",
                                                                          "number of casualties" = "number_of_casualties",
                                                                          "number of vehicles" = "number_of_vehicles"),
                                                              selected = "accident_severity")
                                                  
                                  )),
                                  fluidRow(column(width = 8,offset = 0,
                                                  div(style = "height:50px;width:100%",tags$h3("Univariate Analyis on Distribution of Accident Count")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("barplot1"))),
                                          column(width = 4,offset = 0,
                                                  div(style = "height:50px;width:100%",tags$h3(" ")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("barplot2")))),
                                  fluidRow(column(width = 12,offset = 0,
                                                  div(style = "height:30px;width:100%",tags$h3("Chi-square Test Results on Chosen Variables")),
                                                  box(width = 12,
                                                      height = 50,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,tableOutput("results")))),
                                  
                                  fluidRow(column(width = 12,
                                                  div(style = "height:30px;width:100%",tags$h3("Balloon Plot of Frequency Table")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("balloonplot")))),
                                  fluidRow(column(width = 12,
                                                  div(style = "height:30px;width:100%",tags$h3("Corrplot on Chi-square Residuals")),
                                                  box(width = 12,
                                                      height = 420,
                                                      solidHeader = TRUE,
                                                      collapsible = FALSE,
                                                      collapsed = FALSE,plotOutput("corrplot"))),
                                  
                                  fluidRow(column(width = 12,offset = 0,
                                                  div(style = "height:30px;width:100%",tags$h3("Chi-square Frequency Table")),
                                                  tableOutput("tables")))
                                ) # end of fluidPage
                        ) # end of tabItem for Introduction
                      ))))

                                                
  


server <- function(input, output) {
  
  
  output$barplot1 <- renderPlot({
    ggplot(data=accident,
           aes_string(x=input$cat1)) +
      theme(axis.text.x = element_text(angle = -20, hjust = 0))+
      labs(y = 'Accident Count')+
      geom_bar(fill="lightblue")
  })

  output$barplot2 <- renderPlot({
    ggplot(data=accident,
           aes_string(x = input$cat2)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      labs(y = 'Accident Count')+
      geom_bar(fill="#FF9999")
  })
  
  London_Accident <- reactive({
    req(input$cat1,input$cat2)
    accident  %>% {table(.[[input$cat1]], .[[input$cat2]])}
  })
  
  
  output$results <- renderPrint({
    print(chisq.test(London_Accident()))
  })
  
  
  output$tables <- renderTable({
    accident  %>% {table(.[[input$cat1]], .[[input$cat2]])}
  })
  
  output$balloonplot <- renderPlot({
    balloonplot(t(accident  %>% {table(.[[input$cat1]], .[[input$cat2]])}),main ="", xlab ="", ylab="",
                label = FALSE, show.margins = FALSE)
  })
  
  output$corrplot <- renderPlot({
    corrplot(chisq.test(London_Accident())$residuals, is.cor = FALSE, tl.col = "black")
  })
  
}

shinyApp(ui = ui, server = server)