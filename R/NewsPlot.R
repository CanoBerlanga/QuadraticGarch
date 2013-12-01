NewsPlot <-
function(x){
persistence<-x[3,1]+x[4,1]
Hy<-(x[2,1]/(1-persistence)) 
#news<-function(z) x[2,1]+x[4,1]*Hy+x[3,1]*z^2+x[5,1]*z
news<-function(z) x[2,1]*((1-x[3,1])/(1-x[3,1]-x[4,1]))-x[5,1]^2/(4*x[3,1])+x[3,1]*(z+x[5,1]/(2*x[3,1]))^2

y<-seq(-5,5,by=0.001)
# Main Plot 
plot(y,news(y)^0.5,
axes=FALSE,
type="l",
col = "#0191C8",
lwd=2,
xlab=expression(epsilon[t-1]),
ylab=expression(sigma[t]),
main="",family="Times"
)
abline(v=0,col="dodgerblue4")
grid()
title(main = list("NEWS IMPACT OF Quadratic GARCH (1,1)", cex = 1,col = "dodgerblue4", font = 2))
axis(1,family="Times")
axis(2,family="Times")
}
