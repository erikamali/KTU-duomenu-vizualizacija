
library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title ="Sodros duomenys"),
  dashboardSidebar(
    selectizeInput(inputId = "imones_kodas", label = "Imones vardas", choices = NULL, selected = NULL)
  ),
  dashboardBody(
    fluidRow(box("grafikas", plotOutput("plot"))),
    
  )
)
server <- function(input, output, session) {
  data <- read.csv("https://github.com/kestutisd/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  updateSelectizeInput(session, "imones_kodas", choices = data$name, server = T)
  
  output$table <- renderTable(
    data %>%
      filter(name == input$imones_kodas), digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(name == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) + 
      geom_line()
  )
}

shinyApp (ui, server)
