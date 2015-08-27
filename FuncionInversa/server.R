
#Tarea 1: Estadistica Computacional: Exponencial, histograma y tabla
#Ariana Lopez

library(shiny)
library(datasets)

shinyServer(function(input, output) {

  u <- reactive(runif(1000)) # generador de probas aleatorias
  x <- reactive((1/input$lambda)*log(1/(1-u()))) #inversa de la exponencial
  
  output$distPlot <- renderPlot({ #salida de histograma
    
    hist(x(), breaks = 100, col = 'darkblue', border = 'darkgray', main = "Histograma")
  })
  
  output$view <- renderTable({
    data <- as.data.frame(cbind(x(),u()))
    names(data) <- c("X","U")
    
    data  
  })

})
