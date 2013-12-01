QuadraticGarch <-
function(x){

require(fGarch)
require(Rsolnp)
 
df<-length(x)

CalcResiduals <- function(th, data) {

 th[1] -> mu
 th[2] -> alpha.0
 th[3] -> alpha.1
 th[4] -> beta.1
 th[5] -> gamma
 th[6] -> shape


y <- data 

 n <- length(y)
 sigma.sqs <- vector(length=n) 
 sigma.sqs[1] <- sd(y) ^ 2
 for(ii in c(1:(n-1))) { 
   sigma.sqs[ii + 1] <- (
     alpha.0 +
     alpha.1 * (y[ii]-mu) ^ 2 + 
     beta.1  * sigma.sqs[ii] +
	 gamma   * y[ii] )
 }

 return(list(et = y, ht = sigma.sqs))
}

GarchLogL <- function(th, data) {

 res <- CalcResiduals(th, data) 
 sigma.sqs <- res$ht
 y <- res$et
 return (-sum(log(dged(y[-1], mean=th[1] , sd=sqrt(abs(sigma.sqs[-1])),nu=th[6]))))
}

x0<-c(mean(x),0.001,0.1,0.7,0,1)

#Inicio solver

fit2<- solnp(x0,
fun = GarchLogL,
data = x
)




Out=matrix(NA,ncol=4,nrow=6)
colnames(Out)<-c( "Estimate", "Std. Error" ,"t value", "Pr(>|t|)" )
rownames(Out)<-c("mu","omega","alpha","beta","tau","shape")
Out[,1]<-round(fit2$pars,5)
Out[,2]<-sqrt(diag(solve(fit2$hessian)))
Out[,3]<-Out[,1]/Out[,2]
Out[,4]<-round(pt(abs(Out[,3]) ,df,lower.tail=FALSE),5)
return(Out)

}
