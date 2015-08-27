

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    
    set.seed(30)
    
    #entrada de x uniforme
    u<-runif(input$size)
    v<-runif(input$size)
    
    x<-rep(0,input$size)
    y<-rep(0,input$size)
    
    #salida de normal
    for (i in 1:input$size){
      x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
      y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
    }
    
    plot(density(c(x,y)), main="Normal distribution", col=2)
    
  })

})
