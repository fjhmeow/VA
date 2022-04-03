library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(plotly)

thematic::thematic_shiny()

Accident2020 <- read.csv("data/dft-road-casualty-statistics-accident-2020.csv")
Accident2019 <- read.csv("data/dft-road-casualty-statistics-accident-2019.csv")

ui <- fluidPage(
  titlePanel("London Accident Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "xvariable",
                  label = "x Variable:",
                  choices = c("Speed Limit" = "speed_limit",
                              "Light Conditions" = "light_conditions",
                              "Weather Conditions" = "weather_conditions"),
                  selected = "light_conditions"),
    ),
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

server <- function(input, output, session){
  output$barPlot <- renderPlot({
    ggplot(data=Accident2020,
           aes_string(x = input$xvariable)) +
      theme(axis.text.x = element_text(angle = 90))+
      geom_bar()
  })
}

shinyApp(ui = ui, server = server)