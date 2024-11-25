library(shiny)
library(dplyr)
library(ggplot2)



outerModUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    uiOutput(ns("outer_slider")),
    plotOutput(ns("outer_plot"))

  ))
}

outerMod <- function(input, output, session) {
  ns <- session$ns
  
  makeUI <- function(...){
    tagList(sliderInput(ns("slider1"), label = "outer module slider", min = round(min(mtcars$mpg)), 
                        max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg)), step = 1))
  }
  
  output$outer_slider <- renderUI({
    makeUI()
    # sliderInput(ns("slider1"), label = "outer module slider", min = round(min(mtcars$mpg)), 
    #             max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg)), step = 1)
    
  })
  
  output$outer_plot <- renderPlot({
    req(input$slider1)
    data <- filter(mtcars, between(mpg, input$slider1[1], input$slider1[2]))
    ggplot(data, aes(mpg, wt)) + geom_point()
  })
}

ui <- fluidPage(
  fluidRow(
    outerModUI("outer")
  )
)

server <- function(input, output, session) {
  callModule(outerMod, "outer")
  
}

shinyApp(ui = ui, server = server)
