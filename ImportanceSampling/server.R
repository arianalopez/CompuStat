
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
   
    ImpSamplingMC <- function(nsim,lam2,lam,alfa,beta){
      
      #crudo
      ux <- runif(nsim,0,2)
      phi <- function(x) (2*(lam2*exp(-lam2*x)))
      estim <- mean(phi(ux))
      s2c <- var(phi(ux))
      za2c <- qnorm(.025, lower.tail=FALSE)
      limsupc <- (estim + (za2c*sqrt(s2c/nsim)))
      liminfc <- (estim - (za2c*sqrt(s2c/nsim)))
      
      #exponencial
      u <- runif(nsim)
      xe <- (1/lam)*log(1/(1-(1-exp(-2*lam))*u)) #exp truncada inversa
      f_obj <- function(x) {
        (lam2*exp(-lam2*x))}                 
      f_w <- function(x) {
        dexp(x,lam)/(1-exp(-2*lam))} #esta podria llevar lambda
      f_res <- function(x) {
        f_obj(x)/f_w(x)}
      estim2 <- mean(f_res(xe))
      s2x <- var(f_res(xe))
      za2x <- qnorm(.025, lower.tail=FALSE)
      limsupx <- (estim2 + (za2x*sqrt(s2x/nsim)))
      liminfx <- (estim2 - (za2x*sqrt(s2x/nsim)))
      
      
      #beta 1 y 1.1
      bx <- rbeta(nsim, shape1=alfa, shape2=beta)
      f_objb <- function(x) {
        (lam2*exp(-lam2*x))}                 
      f_wb <- function(x) {
        dexp(x, lam)/(dbeta(x,shape1=alfa, shape2=beta))}#esta podria llevar lambda
      f_resb <- function(x) {
        f_objb(x)/f_wb(x)}
      estim3 <- mean(f_resb(bx))
      s2b <- var(f_resb(bx))
      za2b <- qnorm(.025, lower.tail=FALSE)
      limsupb <- (estim3 + (za2b*sqrt(s2b/nsim)))
      liminfb <- (estim3 - (za2b*sqrt(s2b/nsim)))
      
      return(data.frame(estim,s2c,liminfc,limsupc,estim2,s2x,liminfx,limsupx,estim3,s2b,liminfb,limsupb))
    }
    
    #ImpSamplingMC(i,input$lam2,input$lam,input$alfa,input$beta)
    
    res <-data.frame()
    vec <- seq(1,input$Nm,1)
    for (i in vec){
      res <- rbind(res,ImpSamplingMC(i,input$lam2,input$lam,2,1.5))}
        
        par(pin=c(5, 3))
        plot(res$estim, type="l", col="blue", main = "Monte Carlo Crudo (Media,LS,LI)",ylim = c(0,3),lwd=2, ylab = " Lim Inf (---)  Media  Lim Sup (---)", xlab = "tamano muestra")
        lines(res$liminfc, type="l", col ="darkgray",lty=5)
        lines(res$limsupc, type="l", col ="darkgray",lty=5)
      
      output$distPlot2 <- renderPlot({
        par(pin=c(5, 3))
        plot(res$estim2, type="l", col="green", main = "Exponencial (Media,LS,LI)",ylim = c(.3,1.8),lwd=2,ylab = " Lim Inf (---)  Media  Lim Sup (---)",xlab = "tamano muestra")
        lines(res$liminfx, type="l", col ="darkgray",lty=5)
        lines(res$limsupx, type="l", col ="darkgray",lty=5)
      })
      
      output$distPlot3 <- renderPlot({
        par(pin=c(5, 3))
        plot(res$estim3, type="l", col="black", main = "Beta (Media,LS,LI)",ylim = c(),lwd=2,ylab = " Lim Inf (---)  Media  Lim Sup (---)",xlab = "tamano muestra")
        lines(res$liminfb, type="l", col ="darkgray",lty=5)
        lines(res$limsupb, type="l", col ="darkgray",lty=5)
          
      })
      
      output$distPlot4 <- renderPlot({
        #varianzas
        par(mfrow=c(2, 2))
        plot(res$s2c, type="l", col="black", main = "Varianza Crudo",lwd=2,ylab = "error")
        plot(res$s2x, type="l", col="blue", main = "Varianza Exponencial",lwd=2,ylab = "error")
        plot(res$s2b, type="l", col="gray", main = "Varianza Beta",lwd=2,ylab = " error")
      })
      
      #Salida de tablas
      output$view <- renderTable({
      data <- as.data.frame(cbind(res$estim,res$liminfc,res$limsupc,res$estim2,res$liminfx,res$limsupx,res$estim3,res$liminfb,res$limsupb))  
      
      names(data)<- c("Media crudo","LInf Crudo","LSup Crudo","Media Exp","LInf Exp","LSup Exp","Media Beta","LInf Beta","LSup Beta")
      
        data})
    
  })

})
