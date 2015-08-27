

library(shiny)



shinyUI(fluidPage(

  # Application title
  titlePanel("Tarea 1:Distribucion Exponencial"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  "Seleccionar parametro Lambda:",
                  min = 1,
                  max = 70,
                  value = 25)
            ),
      #agregar aqui seleccion de numeros aleatorios
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("view")
    )
  
)))