Report <-
function(x){

UncMean<-x[1,1]
persistence<-x[3,1]+x[4,1]
uncvolatility<-	(x[2,1]/(1-persistence)) ^ 0.5
halflife<-log(0.5)/log(persistence)

Results<-matrix(NA,nrow=4,ncol=1)
Results[1,1]<-UncMean
Results[2,1]<-uncvolatility
Results[3,1]<-persistence
Results[4,1]<-halflife	

rownames(Results)<-c("UncMean","UncVolatility","Persistence","HalfLife")
colnames(Results)<-"Value"	

return(list(Coefficients.Matrix=x,Additional.Information=Results))
	
}
