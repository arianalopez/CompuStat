#install.packages(pkgs="C:/Users/AriJime/Documents/R/win-library/3.2/LaplacesDemon_13.03.04.tar", repos=NULL, type="source")
#library(LaplacesDemon)

lam <- 1
nsim <- 100000

#Opcion Crudo
ux <- runif(nsim,0,2)
phi <- function(x) ((lam*exp(-lam*x)))
estim <- mean(phi(ux))
print(estim)
curve(phi(x),from=0, to=2,ylim=c(0,1.5))

#Opción Importance Sampling EXPONENCIAL

#generamos X que se comporten exponencial usando la inversa
u <- runif(nsim)
xe <- (1/lam)*log(1/(1-(1-exp(-2*lam))*u)) #exp truncada inversa

#funcion a estimar f_m(x)=m*exp(-mx)
f_obj <- function(x) {
  (lam*exp(-lam*x))
}                 

#funcion peso / f truncada
f_w <- function(x) {
  dexp(x)/(1-exp(-2*lam))
}

#funcion resultante
f_res <- function(x) {
  f_obj(x)*f_w(x)
  }

estim2 <- mean(f_res(xe))

print(estim2)


par(mfrow=c(2,2))
hist(xe) 
curve(f_obj(x), from=0, to=2, ylim=c(0,1.5))
#curve(f_w(x),from=0, to=2, ylim=c(0,1.5))   
curve(f_res(x),from=0, to=2,ylim=c(0,1.5))   


#Opción Importance Sampling BETA

#generamos X que se comporten beta 
a <-  1
b <- 1.1
bx <- rbeta(nsim, shape1 = a, shape2=b)


#funcion a estimar f_m(x)=m*exp(-mx)
f_objb <- function(x) {
  (lam*exp(-lam*x))
}                 

#funcion peso / f truncada
f_wb <- function(x) {
  dexp(x)/(dbeta(x,shape1 = a, shape2=b))
}

#funcion resultante
f_resb <- function(x) {
  f_objb(x)*f_wb(x)
}

estim3 <- mean(f_resb(bx))

print(estim3)


par(mfrow=c(2,2))
hist(bx)
curve(f_objb(x), from=0, to=1, ylim=c(0,1.5), main = "Funcion Objetivo")
#curve(f_wb(x),from=0, to=1, ylim=c(0,1.5))   
curve(f_resb(x),from=0, to=1,ylim=c(0,1.5))

