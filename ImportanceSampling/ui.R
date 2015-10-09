
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Importance Sampling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    #aqui van los sliders
    sidebarPanel(
      #sliderInput("nsim",
      #            "Selecciona de la muestra:",
      #            min = 1000,
      #            max = 10000,
      #            value = 100),
      
      sliderInput("Nm",
                  "Selecciona tamaño de muestras:",
                  min = 150,
                  max = 700,
                  value = 200),
      
      sliderInput("lam2",
                  "Selecciona Lambda Funcion Objetivo:",
                  min = .1,
                  max = 5,
                  value = 3),
      
      sliderInput("lam",
                  "Selecciona Lambda de g(x):",
                  min = .1,
                  max = 5,
                  value = 1.5)
      
      #sliderInput("alfa",
      #            "Alfa Fija: mejor opcion 2",
      #            min = 0,
      #            max = 3,
      #            value = 2),
      
      #sliderInput("beta",width = '100px',
      #            "Beta Fija: mejor opcion 1.5",
      #            min = 0,
      #            max = 3,
      #            value = 1.5)
      
    ), #aqui terminan los sliders

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2"),
      plotOutput("distPlot3"),      
      plotOutput("distPlot4"), 
      tableOutput("view"),
      tags$head(tags$style("#view table {background-color: lightgray; }", media="screen", type="text/css"))
    )
  )
))
