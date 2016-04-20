#taken from "popularpaths.R"
getAllCoords <- function(file) {

	data <- read.csv(file, head=TRUE, sep=",")#loading in the data
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	#polylines <- polylines[-c(1)] #this will delete the first row of data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary

	options(digits=8) #this will allow for 8 digit variables (which we need)

	coords <- list()
#matrix(, nrow=length(polylines), ncol=4) #creating the matrix
					# to hold the x/y start/end coordinates

	#for loop to grab start/end x/y of polylines
	for (i in 1:length(polylines))
	{
		str <- strsplit(strsplit(polylines[i],"\\[\\[")[[1]][2],"\\]\\]")[[1]][1] #get rid of start and end brackets
		arr <- strsplit(str, "\\],\\[")[[1]] #get all coords
		trip <- list()
		for (j in 1:length(arr))
		{
			x_y_tuple = as.numeric(strsplit(arr[j],",")[[1]])
			trip <- append(trip,list(x_y_tuple))
		}
		coords[[i]] = trip
	}
	return (coords)
}

#taken from "reformatXY.R"
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

#taken from "reformatXY.R"
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

#usage: 
#input: normalized list of all trips and its respective ticks
#output: matrix of only x and y coordinates of all the endpoints
getEndpointMatrix <- function(coords) {
	endpoints <- matrix(nrow=length(coords), ncol=2)
	for (i in 1:length(coords)){
		endpoints[i, 1] = coords[[i]][[length(coords[[i]])]][1]
		endpoints[i, 2] = coords[[i]][[length(coords[[i]])]][2]
	}
	return(endpoints)
}

#
getClusteredEndpoint <- function(similarEndPoints,numClusters) {
	fit <- kmeans(similarEndPoints,numClusters)
	aggregate(similarEndPoints,by=list(fit$cluster),FUN=mean)
	similarEndPoints <- data.frame (similarEndPoints, fit$cluster) 
	return (similarEndPoints)
}


#PREPARATION GOES AS FOLLOWS:
#1. specify file to read from
#coords <- getAllCoords("test.csv")
#2. define boundaries given these data
#fourvector <- getMaxMins(coords)
#3. reformat your data
#coords <- translateAll(coords, fourvector)
# ** coords is now a normalized set of every tick of every trip
#4. take only the endpoints as we will only consider them for clustering
#endpoints <- getEndpointMatrix(coords)
# ** endpoints is now a length(coords) x 2 matrix containing all x and y
# coordinates of the endpoints


