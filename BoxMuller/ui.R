

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Tarea 2: Boxmuller"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("size",
                  "Selecciona el tamaño:",
                  min = 1,
                  max = 1000,
                  value = 500)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
