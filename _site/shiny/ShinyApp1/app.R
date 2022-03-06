library(shiny)
library(tidyverse)
library(bslib)
library(thematic)

thematic::thematic_shiny()

exam <- read.csv("data/Exam_data.csv")

ui <- fluidPage(
  theme = bs_theme(bg = "#0b3d91",
                   fg = "white",
                   primary = "#FCC780",
                   base_font = font_google("Roboto"),
                   code_font = font_google("Roboto")),
  titlePanel("Pupils Examination Results Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      sliderInput(inputId = "bin",
                  label = "Number of Bins",
                  min = 5,
                  max = 20,
                  value = 10)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output){
  output$distPlot <- renderPlot({
    ggplot(exam,
           aes_string(x = input$variable)) +
      geom_histogram(bins = input$bin,
                     color = "black",
                     fill = "light blue")
  })
}

shinyApp(ui = ui, server = server)