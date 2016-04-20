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


#Function to get the similarity matrix.
#Takes coords in normal list format, index of trip we want to predict,
#and the indexes we want to compare.

getSimilarityMatrix <- function(coords, indexOfTrip,index1,index2,threshold=.999) {
	comparisonVector <- c(coords[[indexOfTrip]][[index1]],coords[[indexOfTrip]][[index2]])
	len <- length(coords)

	masterList <- c()

	for (i in 1:len)
	{
		if (i == indexOfTrip) next
		if (length(coords[[i]]) < index2) next
		if (cosineSimilarity(c(coords[[i]][[index1]], coords[[i]][[index2]]),comparisonVector) >= threshold)
		{				
			masterList <- c(masterList, i)
		}
	}

	matrix <- matrix(nrow=length(masterList), ncol=2)
	for (i in 1:length(masterList)) {
		matrix[i,1]=coords[[masterList[i]]][[length(coords[[masterList[i]]])]][1]
		matrix[i,2]=coords[[masterList[i]]][[length(coords[[masterList[i]]])]][2]
	}
	return (matrix)
}

#usage: 
#input: normalized list of all trips and its respective ticks
	#and vector of all the indicies of paths we want to consider
#output: matrix of only x and y coordinates of all the endpoints
getEndpointMatrix <- function(coords, similar) {
	endpoints <- matrix(nrow=length(similar), ncol=2)
	for (i in 1:length(similar)){
		endpoints[i, 1] = coords[[similar[i]]][[length(coords[[similar[i]]])]][1]
		endpoints[i, 2] = coords[[similar[i]]][[length(coords[[similar[i]]])]][2]
	}
	return(endpoints)
}

#usage:
#input: matrix of endpoints (n x 2) matrix, # of clusters to be created
#output: returns a (n x 3) matrix, third column denots the cluster 
	#the point belongs to
getClusteredEndpoint <- function(similarEndPoints,numClusters) {
	fit <- kmeans(similarEndPoints,numClusters)
	aggregate(similarEndPoints,by=list(fit$cluster),FUN=mean)
	similarEndPoints <- data.frame (similarEndPoints, fit$cluster) 
	return (similarEndPoints)
}

#usage:
#input: (n x 3) matrix of the points and the clusters they belong to
#output: (#clusters x 3) matrix denoting mean of coordinates in each
	#cluster and probability 
#getEndpointProbabilities <- function(

#PREPARATION GOES AS FOLLOWS:
#1. specify file to read from
#coords <- getAllCoords("test.csv")

#2. define boundaries given these data
#fourvector <- getMaxMins(coords)

#3. reformat your data
#coords <- translateAll(coords, fourvector)
# ** coords is now a normalized set of every tick of every trip

#4. we consider only similar paths to our unknown path
#get a vector 'similars' that contains the indicies (within coords) of these paths

#5. take only the endpoints as we will only consider them for clustering
#endpoints <- getEndpointMatrix(coords, similars)
# ** endpoints is now a length(similars) x 2 matrix containing all x and y
# coordinates of the endpoints

#6 determine the number of clusters to be used (TBD)

#7 append the cluster each point belongs to into the matrix
#similarEndpoints <- getClusteredEndpoint(endpoints, numClusters)

#8 get a probability


