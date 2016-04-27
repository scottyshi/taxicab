options(digits=8) #this will allow for 8 digit variables (which we need)

#taken from "popularpaths.R"
getAllCoords <- function(file) {

	data <- read.csv(file, head=TRUE, sep=",")#loading in the data
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	#polylines <- polylines[-c(1)] #this will delete the first row of data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary

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

#taken from 'popularpaths.R'
#usage, cosineSimilarity(matrix[1,],matrix[2,])
#returns the cosine of the angle in-between 
# ~1 means they are very similar
cosineSimilarity <- function(vector1,vector2) {
	#if (length(vector1) != length(vector2)) {
	#	return NULL;
	#}
	dotProduct = 0;
	length1 = 0;
	length2 = 0;
	for (i in 1:length(vector1)) {
		dotProduct <- dotProduct + vector1[i] * vector2[i];
		length1 <- length1 + (vector1[i])^2;
		length2 <- length2 + (vector2[i])^2;
	}
	length1 <- sqrt(length1);
	length2 <- sqrt(length2);
	product <- dotProduct/(length1 * length2);
	return (product);
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

#usage: get probabilities that the path will go to the given clusters
#input: matrix of endpoints(n x 2) matrix, # of clusters to be created
#output: (#cluster by 3)  matrix that denotes probability as well as the
	#mean x and y coordinates of each cluster
getProbabilities <- function(mat, numClusters) {
	fit <- kmeans(mat, numClusters)
	prob <- matrix(nrow=numClusters, ncol=3)
	#initalizing all probabilities by 0
	for (i in 1:numClusters) {
		prob[i,1] = 0
	}
	clusvec <- fit$cluster

	#inputting frequency of each cluster
	for (i in 1:length(clusvec)) {
		prob[clusvec[i],1] = prob[clusvec[i],1] + 1
	}

	#divide by cluster size to get length
	for (i in 1:numClusters) {
		prob[i,1] = prob[i,1] / length(clusvec)
		prob[i,2] = fit$centers[i,1]
		prob[i,3] = fit$centers[i,2]
	}
	return(prob)
}











#usage:OLD? DONT NEED TO APPEND NUMCLUSTERS TO 3RD COL OF MATRIX?
#input: matrix of endpoints (n x 2) matrix, # of clusters to be created
#output: returns a (n x 3) matrix, third column denots the cluster 
	#the point belongs to
getClusteredEndpoint <- function(similarEndPoints,numClusters) {
	fit <- kmeans(similarEndPoints,numClusters)
	aggregate(similarEndPoints,by=list(fit$cluster),FUN=mean)
	similarEndPoints <- data.frame (similarEndPoints, fit$cluster) 
	return (similarEndPoints)
}

#Algorithm GOES AS FOLLOWS:
#1. specify file to read from
#2. define boundaries given these data
#3. reformat your data
#4. narrow down our path choices to those only similar to a certain path
#5. determine the number of clusters to be used (TBD)
#6. use kmeans to output probability for each cluster 

main <- function(file,arg=1) {
	coords <- getAllCoords(file)
	fourvector <- getMaxMins(coords)
	coords <- translateAll(coords, fourvector)
	
	#must define here what path to try to predict + which ticks to use
	mat <- getSimilarityMatrix(coords, arg, 1, 4)
	
	#must define here what number of clusters is?
	numClusters <- 5

	if (length(mat)/2 <= numClusters) numClusters <- length(mat)/2-1
	
	
	prob <- getProbabilities(mat, numClusters)
	return(prob)
}

#ENDGOAL: allocate drivers in a way that minimum gas is used

#ASSUMPTIONS: 
#once i'm done with ride, i stop unless i see PURPOSE
#all cars spend same amt of gas for same trip

#IN PROGRESS:
#do bipartite matching between unbusy drivers and customers

#FUTURE TASKS:
#1. see how "fair" we are --> how many rides we all get 
# --> each taxi driver should get a minimum number of trips
#2. predict when taxi driver will be free based on which cluster it might end up in
# --> try to have an offline solution in the meantime
#3. given 2 different drivers, how likely is that if they go on the same
#path will they take the same time to complete the trip?

#statistical techniques
# linear regression
# polynomial regression
# CART: classification and regresstion trees
# look at first 'k' ticks and plot how likely you are to predict correctly 