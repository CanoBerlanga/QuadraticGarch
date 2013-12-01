VolatilityPlot <-
function(x,y){

 n <- length(y)
 sigma.sqs <- vector(length=n) 
 sigma.sqs[1] <- sd(y)^2 
 for(ii in c(1:(n-1))) { 
   sigma.sqs[ii + 1] <- (
     x[2,1] +
     x[3,1] * (y[ii]-x[1,1]) ^ 2 + 
     x[4,1] * sigma.sqs[ii]) +
	 x[5,1] * (y[ii])
 }

plot(sqrt(sigma.sqs),type="l",col="#0191C8",axes=FALSE,main="",xlab="Time",ylab=expression(sigma[t]),family="Times")
title(main = list("VOLATILITY PLOT", cex = 1,col = "dodgerblue4", font = 2))
axis(1,family="Times")
axis(2,family="Times")
	
return(sqrt(sigma.sqs))	
	
}
