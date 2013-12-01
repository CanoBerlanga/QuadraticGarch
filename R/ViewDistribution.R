ViewDistribution <-
function(x,y){
	
	sigma <- sqrt(x[2,1]/(1-x[3,1]-x[4,1]))
	VAR <- qsged(0.05, mean = x[1,1], sd = sigma , nu = x[6,1])
	Sim <- rged(9e+05, mean = x[1,1], sd = sigma , nu = x[6,1])
	plot(density(y), xlim = c(-7, 7), axes = FALSE, lty = 1, 
    lwd = 1.5, col = "#AEA79F", main = "Returns Distribution", 
    xlab = expression(y[t]), family = "Times")
    axis(1, family = "Times")
    axis(2, family = "Times")
    lines(density(Sim), col = "#0191C8", lty = 1, lwd = 1.5)
    abline(v = VAR, lty = 2, lwd = 0.5)
    text(VAR, 0.005, paste("Value at Risk = ", round(VAR, digits = 4), 
        sep = ""), offset = 0.5, pos = 4, cex = 0.8, srt = 0, 
        family = "Times")
}
