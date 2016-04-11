getStartEndCoords <- function(file) {

	data <- read.csv(file, head=TRUE, sep=",")#loading in the data
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	#polylines <- polylines[-c(1)] #this will delete the first row of data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary

	options(digits=8) #this will allow for 8 digit variables (which we need)

	coords <- matrix(, nrow=length(polylines), ncol=4) #creating the matrix
					# to hold the x/y start/end coordinates

	#for loop to grab start/end x/y of polylines

	for (i in 1:length(polylines))
	{
		str <- polylines[i]
		arr <- as.list(strsplit(str, ",")[[1]]) #get all coords
		#some data only have a starting x/y but not an ending
		#i just leave the entire row N/A in this case
		if(length(arr)!= 2)
		{
			coords[i, 1] <- as.numeric(substr(arr[[1]], 3, 12))
			coords[i, 2] <- as.numeric(strsplit(arr[[2]], "[]]"))
			coords[i, 3] <- as.numeric(substr(arr[[length(arr)-1]], 2, 11))
			coords[i, 4] <- as.numeric(unlist(strsplit(arr[[length(arr)]], "[]]"))[1])
		}
	}
	#now coords holds all the x/y coordinates of start/end of every trip

	return (coords)
}

#usage, cosine(matrix[1,],matrix[2,])
#returns the cosine of the angle in-between 
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



	#cosine similarity -- 2 routes will be similar if there's close points
	#put all points into k-dimensional space
	#use start / end x and y coordinates to create 4 dimensional vectors
	#	for EACH trip --> create a vector for each trip
	#when comparing two trips --> if angle is small between them -- they're similar

	#1. see how geometrically similar two routes are
	#2. see what kind of time difference some geometrically similar routes are

	#PART TWO!!!
	#We have data about where trips start
	#We want to predict where it will end (w/ 