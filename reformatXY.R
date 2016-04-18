#takes list of lists of tuples
getMaxMins <- function(coords) {
	X <- double()
	Y <- double()
	for (i in 1:length(coords)) {
		for (j in 1:length(coords[[i]])) {
			X <- c(X,as.numeric(coords[[i]][[j]][1]))
			Y <- c(Y,as.numeric(coords[[i]][[j]][2]))
		} 
	}
	return (c(min(X),max(X),min(Y),max(Y)))
}

translateAll <- function(coords,fourvector) {
	XFactor <- fourvector[2] - fourvector[1];
	YFactor <- fourvector[4] - fourvector[3];
	for (i in 1:length(coords)) {
		for (j in 1:length(coords[[i]])) {
			coords[[i]][[j]][1] <- (as.numeric(coords[[i]][[j]][1])-fourvector[1])/XFactor
			coords[[i]][[j]][2] <- (as.numeric(coords[[i]][[j]][2])-fourvector[3])/YFactor
		} 
	}
	return (coords)
}

translate<- function(X,Y,fourvector) {
	XFactor <- fourvector[2] - fourvector[1]
	YFactor <- fourvector[4] - fourvector[3]
	return (c(as.numeric(X)/XFactor,as.numeric(Y)/YFactor))
}
detranslate <- function(translatedX,translatedY,fourvector) {
	XFactor <- fourvector[2] - fourvector[1]
	YFactor <- fourvector[4] - fourvector[3]
	return (c((translatedX*XFactor + fourvector[1]),(translatedY*YFactor + fourvector[3])))
}
