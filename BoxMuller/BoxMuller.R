
boxmuller<-function(){ 
  
set.seed(15)

size<-10000

u<-runif(size)
v<-runif(size)

x<-rep(0,size)
y<-rep(0,size)

for (i in 1:size){
  x[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
  y[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
}

plot(density(c(x,y)))

}

