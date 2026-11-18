#Week 11?? Homework - Shiny App
# Mei Iwamoto
# Tuesday, Nov 17th 2026

library(shiny)
library(here)
library(tidyverse)
library(DT)

#cleaned ikea furniture naming data from tidy tuesday!
ikea_places <- read.csv(here("ikeadata.csv"))


#attempt 2
ui <- fluidPage(
  
  # Inputs functions
  varSelectInput(inputId = "variable", #use varselectinput to select variables/column names from a data frame
                 label = "Variable:", 
                 ikea_places),
  # Outputs functions
  plotOutput("data"), #creates space for a plot named data
  dataTableOutput("table") 
)

server <- function(input, output){
  
    output$data <- renderPlot({
      ggplot(ikea_places, aes(!!input$variable)) + #need !! before input here
        geom_bar() +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(title="IKEA Seating Furniture Names: Where do they come from?",
             x="Name",
             y="Count")
    })
  
    output$table <- 
      renderDataTable({datatable(ikea_places)}) 
}

shinyApp(ui = ui, server = server)


